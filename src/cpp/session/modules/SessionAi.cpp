/*
 * SessionAi.cpp
 *
 * Copyright (C) 2022 by Posit Software, PBC
 *
 * Unless you have received this program directly from Posit Software pursuant
 * to the terms of a commercial license agreement with Posit Software, then
 * this program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
 *
 */

#include "SessionAi.hpp"

#include <algorithm>
#include <gsl/gsl-lite.hpp>

#include <boost/regex.hpp>
#include <boost/function.hpp>
#include <boost/format.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/algorithm/string/regex.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/iostreams/filter/aggregate.hpp>

#include <shared_core/Error.hpp>

#include <core/Algorithm.hpp>
#include <core/Exec.hpp>
#include <core/Log.hpp>

#include <core/http/Request.hpp>
#include <core/http/Response.hpp>
#include <core/http/URL.hpp>
#include <core/FileSerializer.hpp>
#include <core/system/Process.hpp>
#include <core/system/ShellUtils.hpp>
#include <core/r_util/RPackageInfo.hpp>

#define R_INTERNAL_FUNCTIONS
#include <r/RInternal.hpp>
#include <r/RSexp.hpp>
#include <r/RExec.hpp>
#include <r/RFunctionHook.hpp>
#include <r/ROptions.hpp>
#include <r/RUtil.hpp>
#include <r/RRoutines.hpp>
#include <r/session/RSessionUtils.hpp>

#include <session/SessionModuleContext.hpp>
#include <session/SessionPersistentState.hpp>

#include <session/prefs/UserPrefs.hpp>

#include "session-config.h"

#ifdef RSTUDIO_SERVER
#include <server_core/UrlPorts.hpp>
#endif

// protect R against windows TRUE/FALSE defines
#undef TRUE
#undef FALSE

using namespace rstudio::core;
using namespace boost::placeholders;

namespace rstudio {
namespace session {
namespace modules { 
namespace ai {

namespace {   

// save computed ai url prefix for comparison in rAiUrlHandler
const char * const kAiLocation = "/ai";
const std::string kPythonLocation = "/python";
const char * const kCustomLocation = "/custom";
const char * const kSessionLocation = "/session";

// are we handling custom urls internally or allowing them to
// show in an external browser
bool s_handleCustom = false;

// javascript callbacks to inject into page
const char * const kJsCallbacks = R"EOF(
<script type="text/javascript">

   if (window.parent.aiNavigated)
      window.parent.aiNavigated(document, window);

   if (window.parent.aiKeydown)
      window.onkeydown = function(e) { window.parent.aiKeydown(e); }

   if (window.parent.aiMousedown)
      window.onmousedown = function(e) { window.parent.aiMousedown(e); }

   if (window.parent.aiMouseover)
      window.onmouseover = function(e) { window.parent.aiMouseover(e); }

   if (window.parent.aiMouseout)
      window.onmouseout = function(e) { window.parent.aiMouseout(e); }

   if (window.parent.aiClick)
      window.onclick = function(e) { window.parent.aiClick(e); } 

   window.addEventListener("load", function(event) {

      // https://github.com/rstudio/rmarkdown/blob/de02c926371fdadc4d92f08e1ad7b77db069be49/inst/rmarkdown/templates/html_vignette/resources/vignette.css#L187-L201
      var classMap = {
         "at": "ace_keyword ace_operator",
         "ch": "ace_string",
         "co": "ace_comment",
         "cf": "ace_keyword",
         "cn": "ace_constant ace_language",
         "dt": "ace_identifier",
         "dv": "ace_constant ace_numeric",
         "er": "ace_keyword ace_operator",
         "fu": "ace_identifier",
         "kw": "ace_keyword",
         "ot": "ace_keyword ace_operator",
         "sc": "ace_keyword ace_operator",
         "st": "ace_string",
      };

      var els = document.querySelectorAll(".sourceCode span");
      for (el of els)
         el.className = classMap[el.className] || el.className;

   });

</script>
)EOF";

std::string localURL(const std::string& address, const std::string& port)
{
   return "http://" + address + ":" + port + "/";
}

std::string replaceRPort(const std::string& url,
                         const std::string& rPort,
                         const std::string& scope)
{

   // avoid replacing port in query params in R ai, as R uses this
   // for state management in its ai server from R 3.6.0 and onwards
   if (scope.empty())
   {
      std::vector<std::string> splat = core::algorithm::split(url, "?");
      boost::algorithm::replace_last(splat[0], rPort, session::options().wwwPort());
      return core::algorithm::join(splat, "?");
   }
   else
   {
      return boost::algorithm::replace_last_copy(url, rPort, session::options().wwwPort());
   }
}

bool isLocalURL(const std::string& url,
                const std::string& scope,
                std::string* pLocalURLPath = nullptr)
{
   // first look for local ip prefix
   std::string rPort = module_context::rLocalAiPort();
   std::string urlPrefix = localURL("127.0.0.1", rPort);
   size_t pos = url.find(urlPrefix + scope);
   if (pos != std::string::npos)
   {
      std::string relativeUrl = url.substr(urlPrefix.length());
      if (pLocalURLPath)
         *pLocalURLPath = replaceRPort(relativeUrl, rPort, scope);
      return true;
   }

   // next look for localhost
   urlPrefix = localURL("localhost", rPort);
   pos = url.find(urlPrefix + scope);
   if (pos != std::string::npos)
   {
      std::string relativeUrl = url.substr(urlPrefix.length());
      if (pLocalURLPath)
         *pLocalURLPath = replaceRPort(relativeUrl, rPort, scope);
      return true;
   }

   // none found
   return false;
}

std::string normalizeHttpdSearchContent(const std::string& content)
{
   return boost::regex_replace(
            content,
            boost::regex("(The search string was <b>\")(.*)(\"</b>)"),
            [](const boost::smatch& m)
   {
      std::string query = m[2];
      if (query.find('<') != std::string::npos)
         query = string_utils::htmlEscape(query);

      return m[1] + query + m[3];
   });
}

template <typename F>
bool isHttpdErrorPayload(SEXP payloadSEXP, F accessor)
{
   for (int i = 0; i < r::sexp::length(payloadSEXP); i++)
   {
      std::string line = r::sexp::asString(accessor(payloadSEXP, i));
      if (line.find("<title>R: httpd error</title>") != std::string::npos)
         return true;
   }

   return false;
}

bool isHttpdErrorPayload(SEXP payloadSEXP)
{
   switch (TYPEOF(payloadSEXP))
   {
   case STRSXP : return isHttpdErrorPayload(payloadSEXP, STRING_ELT);
   case VECSXP : return isHttpdErrorPayload(payloadSEXP, VECTOR_ELT);
   default     : return false;
   }
}

// Filter for HTML content
class AiContentsFilter : public boost::iostreams::aggregate_filter<char>
{
public:
   typedef std::vector<char> Characters;

   explicit AiContentsFilter(const http::Request& request)
   {
      requestUri_ = request.uri();
   }

   void do_filter(const Characters& src, Characters& dest)
   {
      std::string baseUrl = http::URL::uncomplete(
            requestUri_,
            kAiLocation);

      // copy from src to dest
      dest = src;
      
      // fixup hard-coded hrefs
      boost::algorithm::replace_all(dest, "href=\"/", "href=\"" + baseUrl + "/");
      boost::algorithm::replace_all(dest, "href='/", "href='" + baseUrl + "/");
      
      // fixup hard-coded src=
      boost::algorithm::replace_all(dest, "src=\"/", "src=\"" + baseUrl + "/");
      boost::algorithm::replace_all(dest, "src='/", "src='" + baseUrl + "/");
      
      // add classes to headers
      boost::regex reHeader("<h3>Arguments</h3>");
      std::string reFormat("<h3 class=\"r-arguments-title\">Arguments</h3>");
      boost::algorithm::replace_all_regex(dest, reHeader, reFormat);
      
      // append javascript callbacks
      std::string js(kJsCallbacks);
      std::copy(js.begin(), js.end(), std::back_inserter(dest));
   }
   
private:
   std::string requestUri_;
};

// Get the R library path from .libPaths()[1]
FilePath getRLibraryPath()
{
   std::string path;
   Error error = r::exec::evaluateString(".libPaths()[1]", &path);
   if (error)
   {
      LOG_ERROR(error);
      return FilePath();
   }
   return FilePath(path);
}

class AiFontSizeFilter : public boost::iostreams::aggregate_filter<char>
{
public:
   typedef std::vector<char> Characters;

   void do_filter(const Characters& src, Characters& dest)
   {
      std::string cssValue(src.begin(), src.end());
      cssValue.append("body, td {\n   font-size:");
      cssValue.append(safe_convert::numberToString(prefs::userPrefs().helpFontSizePoints()));
      cssValue.append("pt;\n}");
      std::copy(cssValue.begin(), cssValue.end(), std::back_inserter(dest));
   }
};

template <typename Filter>
void setDynamicContentResponse(const std::string& content,
                               const http::Request& request,
                               const Filter& filter,
                               http::Response* pResponse)
{
   // always attempt gzip
   if (request.acceptsEncoding(http::kGzipEncoding))
      pResponse->setContentEncoding(http::kGzipEncoding);
   
   // if the response doesn't already have Cache-Control then send an eTag back
   // and force revalidation (not for desktop mode since it doesn't handle
   // eTag-based caching)
   if (!pResponse->containsHeader("Cache-Control") &&
       options().programMode() == kSessionProgramModeServer)
   {
      // force cache revalidation since this is dynamic content
      pResponse->setCacheWithRevalidationHeaders();

      // set as cacheable content (uses eTag/If-None-Match)
      Error error = pResponse->setCacheableBody(content, request, filter);
      if (error)
      {
         pResponse->setError(http::status::InternalServerError,
                             error.getMessage());
      }
   }
   // otherwise just leave it alone
   else
   {
      pResponse->setBody(content, filter);
   }
}

void setDynamicContentResponse(const std::string& content,
                               const http::Request& request,
                               http::Response* pResponse)
{
   http::NullOutputFilter nullFilter;
   setDynamicContentResponse(content, request, nullFilter, pResponse);
}

// Function to handle AI requests, specifically for Wikipedia
void handleAiRequest(const http::Request& request, http::Response* pResponse)
{
   // Get the requested path
   std::string path = http::util::pathAfterPrefix(request, kAiLocation);
   
   // Check if this is a Wikipedia HTML file
   if (boost::algorithm::ends_with(path, ".html") && path.find("doc/html/") != std::string::npos)
   {
      // Extract the filename
      std::string filename = boost::algorithm::replace_all_copy(path, "doc/html/", "");
      
      // Remove leading slash if present
      if (!filename.empty() && filename[0] == '/')
         filename = filename.substr(1);
      
      // Build the full path to the file
      FilePath libPath = getRLibraryPath();
      FilePath aiDocDir = libPath.completeChildPath("ai/doc/html");
      
      // Make sure the directory exists
      if (!aiDocDir.exists())
      {
         Error error = aiDocDir.ensureDirectory();
         if (error)
         {
            LOG_ERROR(error);
            pResponse->setError(http::status::InternalServerError, "Failed to create AI directory");
            return;
         }
      }
      
      FilePath filePath = aiDocDir.completeChildPath(filename);
      
      // Serve the file if it exists
      if (filePath.exists())
      {
         // Set content type and encoding for proper HTML handling
         pResponse->setContentType("text/html; charset=UTF-8");
         
         // Read the file content
         std::string content;
         Error error = core::readStringFromFile(filePath, &content);
         if (error)
         {
            LOG_ERROR(error);
            pResponse->setError(http::status::InternalServerError, "Failed to read file content");
            return;
         }
         
         // Set the response body
         pResponse->setBody(content, AiContentsFilter(request));
         return;
      }
   }

   // server custom css file if necessary
   if (boost::algorithm::ends_with(path, "/R.css"))
   {
      core::FilePath cssFile = options().rResourcesPath().completeChildPath("R.css");
      if (cssFile.exists())
      {
         // ignoring the filter parameter here because the only other possible filter 
         // is AiContentsFilter which is for html
         pResponse->setFile(cssFile, request, AiFontSizeFilter());
         return;
      }
   }
   
   // For any other AI requests, delegate to the R implementation
   // Create the R call
   r::sexp::Protect rp;
   SEXP httpdSEXP;
   
   // Call the R httpd function with the path
   r::exec::RFunction httpd("tools:::httpd");
   httpd.addParam(path);
   httpd.addParam(R_NilValue);  // query
   httpd.addParam(R_NilValue);  // postBody
   
   Error error = httpd.call(&httpdSEXP, &rp);
   
   // Handle errors
   if (error)
   {
      pResponse->setError(http::status::InternalServerError, error.getMessage());
      return;
   }
   
   // Process the response if it's a valid R list
   if (TYPEOF(httpdSEXP) == VECSXP && r::sexp::length(httpdSEXP) >= 4)
   {
      // Extract response components
      std::string payload;
      if (TYPEOF(VECTOR_ELT(httpdSEXP, 0)) == STRSXP)
         payload = CHAR(STRING_ELT(VECTOR_ELT(httpdSEXP, 0), 0));
      
      std::string contentType;
      if (TYPEOF(VECTOR_ELT(httpdSEXP, 1)) == STRSXP)
         contentType = CHAR(STRING_ELT(VECTOR_ELT(httpdSEXP, 1), 0));
      
      int status = r::sexp::asInteger(VECTOR_ELT(httpdSEXP, 3));
      
      // Set response
      pResponse->setStatusCode(status);
      pResponse->setContentType(contentType);
      pResponse->setBody(payload);
   }
   else
   {
      pResponse->setError(http::status::InternalServerError, "Invalid response from R");
   }
}

// Simpler version of handleLocalHttpUrl that just handles Wikipedia searches
bool handleLocalHttpUrl(const std::string& url)
{
   // Check if it's a Wikipedia URL
   if (url.find("ai/doc/html/") != std::string::npos && url.find(".html") != std::string::npos)
   {
      std::string aiPath = "ai/" + url.substr(url.find("ai/") + 3);
      ClientEvent aiEvent(client_events::kShowAi, aiPath);
      module_context::enqueClientEvent(aiEvent);
      return true;
   }
   
   // check for custom
   std::string customPath;
   if (isLocalURL(url, "custom", &customPath))
   {
      if (s_handleCustom)
      {
         ClientEvent event = browseUrlEvent(customPath);
         module_context::enqueClientEvent(event);
         return true;
      }
      else // leave alone (show in external browser)
      {
         return false;
      }
   }

   // check for session
   std::string sessionPath;
   if (isLocalURL(url, "session", &sessionPath))
   {
      if (s_handleCustom)
      {
         ClientEvent event = browseUrlEvent(sessionPath);
         module_context::enqueClientEvent(event);
         return true;
      }
      else // leave alone (show in external browser)
      {
         return false;
      }
   }

   // leave portmapped urls alone
   if (isLocalURL(url, "p/"))
   {
      return false;
   }

   // otherwise look for ai (which would be all other localhost urls)
   std::string aiPath;
   if (isLocalURL(url, "", &aiPath))
   {
      aiPath = "ai/" + aiPath;
      ClientEvent aiEvent(client_events::kShowAi, aiPath);
      module_context::enqueClientEvent(aiEvent);
      return true;
   }

#ifdef RSTUDIO_SERVER
   // other localhost URLs can benefit from port mapping -- we map them
   // all since if we don't do any mapping they'll just fail hard
   
   // see if we can form a portmap path for this url
   std::string path;
   if (options().programMode() == kSessionProgramModeServer &&
       server_core::portmapPathForLocalhostUrl(url, 
            persistentState().portToken(), &path))
   {
      module_context::enqueClientEvent(browseUrlEvent(path));
      return true;
   }
#endif

   // Let other URLs be handled by the browser
   return false;
}

// Environment for topics
SEXP s_customHandlersEnv = nullptr;
SEXP lookupCustomHandler(const std::string& uri)
{
   // pick name of handler out of uri
   boost::regex customRegx(".*/custom/([A-Za-z0-9_\\-]*).*");
   boost::smatch match;
   if (regex_utils::match(uri, match, customRegx))
   {
      std::string handler = match[1];

      // load .httpd.handlers.env
      if (!s_customHandlersEnv)
      {
         SEXP toolsSEXP = r::sexp::findNamespace("tools");
         s_customHandlersEnv = Rf_eval(Rf_install(".httpd.handlers.env"), toolsSEXP);
      }

      // we only proceed if .httpd.handlers.env really exists
      if (TYPEOF(s_customHandlersEnv) == ENVSXP)
      {
         SEXP cl = Rf_findVarInFrame3(s_customHandlersEnv, Rf_install(handler.c_str()), TRUE);
         if (cl != R_UnboundValue && TYPEOF(cl) == CLOSXP) // need a closure
            return cl;
      }
   }

   // if we didn't find a handler then return handler lookup error
   return r::sexp::findFunction(".rs.handlerLookupError");
}

// handle custom URLs (as defined in the R handler)
void handleCustomRequest(const http::Request& request, 
                         http::Response* pResponse)
{
   // Create the R call
   r::sexp::Protect rp;
   SEXP httpdSEXP;
   
   // Look up the custom handler
   SEXP handler = lookupCustomHandler(request.uri());
   
   // Call the handler function with the path
   std::string path = http::util::pathAfterPrefix(request, kCustomLocation);
   
   r::exec::RFunction customFn(handler);
   customFn.addParam(path);
   customFn.addParam(R_NilValue);  // query
   customFn.addParam(R_NilValue);  // postBody
   
   Error error = customFn.call(&httpdSEXP, &rp);
   
   // Handle errors
   if (error)
   {
      pResponse->setError(http::status::InternalServerError, error.getMessage());
      return;
   }
   
   // Process the response if it's a valid R list
   if (TYPEOF(httpdSEXP) == VECSXP && r::sexp::length(httpdSEXP) >= 4)
   {
      // Extract response components
      std::string payload;
      if (TYPEOF(VECTOR_ELT(httpdSEXP, 0)) == STRSXP)
         payload = CHAR(STRING_ELT(VECTOR_ELT(httpdSEXP, 0), 0));
      
      std::string contentType;
      if (TYPEOF(VECTOR_ELT(httpdSEXP, 1)) == STRSXP)
         contentType = CHAR(STRING_ELT(VECTOR_ELT(httpdSEXP, 1), 0));
      
      int status = r::sexp::asInteger(VECTOR_ELT(httpdSEXP, 3));
      
      // Set response
      pResponse->setStatusCode(status);
      pResponse->setContentType(contentType);
      pResponse->setBody(payload);
   }
   else
   {
      pResponse->setError(http::status::InternalServerError, "Invalid response from R");
   }
}

// handle requests for session temporary directory
void handleSessionRequest(const http::Request& request, http::Response* pResponse)
{
   // get the raw uri & strip its location prefix
   std::string sessionPrefix = std::string(kSessionLocation) + "/";
   std::string uri = request.uri();
   if (!uri.compare(0, sessionPrefix.length(), sessionPrefix))
      uri = uri.substr(sessionPrefix.length());

   // remove query parameters and anchor
   std::size_t pos = uri.find("?");
   if (pos != std::string::npos)
      uri.erase(pos);
   pos = uri.find("#");
   if (pos != std::string::npos)
      uri.erase(pos);

   // ensure that this path does not contain ..
   if (uri.find("..") != std::string::npos)
   {
      pResponse->setNotFoundError(request);
      return;
   }

   // form a path to the temporary file
   FilePath tempFilePath = r::session::utils::tempDir().completeChildPath(uri);

   // return the file
   pResponse->setCacheableFile(tempFilePath, request);
}

// Handle Python help requests
void handlePythonAiRequest(const http::Request& request,
                          http::Response* pResponse)
{
   // get URL (everything after 'python/' bit
   std::string code = request.uri().substr(::strlen("/python/"));
   if (code.empty())
   {
      pResponse->setError(http::status::BadRequest, "Malformed URL");
      return;
   }

   // construct HTML ai file from requested object
   std::string path;
   Error error = r::exec::RFunction(".rs.python.generateHtmlAi")
         .addParam(code)
         .call(&path);
   if (error)
      LOG_ERROR(error);
   
   if (path.empty())
   {
      pResponse->setNotFoundError(request);
      return;
   }
   
   FilePath filePath(path);
   pResponse->setContentType("text/html");
   pResponse->setFile(filePath, request, AiContentsFilter(request));
}

// Function for R to call to show Python help
SEXP rs_showPythonAi(SEXP codeSEXP)
{
   std::string code = r::sexp::safeAsString(codeSEXP);
   boost::format fmt("python/%1%.html");
   std::string url = boost::str(fmt % http::util::urlEncode(code, true));
   ClientEvent event(client_events::kShowAi, url);
   module_context::enqueClientEvent(event);
   return R_NilValue;
}

// Function for R to call for Rd preview
SEXP rs_previewRd(SEXP rdFileSEXP)
{
   std::string rdFile = r::sexp::safeAsString(rdFileSEXP);
   boost::format fmt("ai/preview?file=%1%");
   std::string url = boost::str(fmt % http::util::urlEncode(rdFile, true));
   ClientEvent event(client_events::kShowAi, url);
   module_context::enqueClientEvent(event);
   return R_NilValue;
}

// Function to clear AI conversation JSON when refresh button is clicked
Error clearAiConversation(const json::JsonRpcRequest& request,
                         json::JsonRpcResponse* pResponse)
{
   // Call the R function to clear the conversation
   Error error = r::exec::RFunction(".rs.clear_ai_conversation").call();
   if (error)
      LOG_ERROR(error);
   
   return Success();
}

} // anonymous namespace
   
Error initialize()
{
   RS_REGISTER_CALL_METHOD(rs_previewRd, 1);
   RS_REGISTER_CALL_METHOD(rs_showPythonAi, 1);

   using boost::bind;
   using core::http::UriHandler;
   using namespace module_context;
   using namespace rstudio::r::function_hook;
   
   // Register handler for clearing conversation on refresh
   ExecBlock initBlock;
   initBlock.addFunctions()
      (bind(module_context::registerRpcMethod, "clear_ai_conversation", clearAiConversation))
      (bind(registerRBrowseUrlHandler, handleLocalHttpUrl))
      (bind(registerUriHandler, kAiLocation, handleAiRequest))
      (bind(registerUriHandler, kPythonLocation, handlePythonAiRequest))
      (bind(sourceModuleRFile, "SessionAi.R"));
   
   Error error = initBlock.execute();
   if (error)
      return error;

   // init ai
   bool isDesktop = options().programMode() == kSessionProgramModeDesktop;
   int port = safe_convert::stringTo<int>(session::options().wwwPort(), 0);
   error = r::exec::RFunction(".rs.initAi", port, isDesktop).call(&s_handleCustom);
   if (error)
      LOG_ERROR(error);

#ifdef _WIN32
   // R's ai server handler has issues with R 4.0.0; disable it explicitly
   // when that version of R is in use.
   // (see comments in module_context::sessionTempDirUrl)
   if (r::util::hasExactVersion("4.0.0"))
   {
      s_handleCustom = false;
   }
#endif

   // handle /custom and /session urls internally if necessary (always in
   // server mode, in desktop mode if the internal http server can't
   // bind to a port)
   if (s_handleCustom)
   {
      ExecBlock serverInitBlock;
      serverInitBlock.addFunctions()
         (bind(registerUriHandler, kCustomLocation, handleCustomRequest))
         (bind(registerUriHandler, kSessionLocation, handleSessionRequest));
      error = serverInitBlock.execute();
      if (error)
         return error;
   }

   return Success();
}

} // namespace ai
} // namespace modules
} // namespace session
} // namespace rstudio

