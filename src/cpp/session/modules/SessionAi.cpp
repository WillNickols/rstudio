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
#include <boost/algorithm/string.hpp>

#include <shared_core/Error.hpp>

#include <core/Exec.hpp>
#include <core/Log.hpp>

#include <core/http/Request.hpp>
#include <core/http/Response.hpp>
#include <core/http/URL.hpp>
#include <core/FileSerializer.hpp>

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

#include "session-config.h"

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

// Constants
const char * const kAiLocation = "/ai";
bool s_handleCustom = false;

// Filter for HTML content (minimal version)
class AiContentsFilter : public boost::iostreams::aggregate_filter<char>
{
public:
   typedef std::vector<char> Characters;

   explicit AiContentsFilter(const http::Request& request) 
      : requestUri_(request.uri()) 
   {
   }
   
   void do_filter(const Characters& src, Characters& dest)
   {
      // Pass through content unmodified
      std::string html(src.begin(), src.end());
      std::copy(html.begin(), html.end(), std::back_inserter(dest));
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
      
      // Build the full path to the file - fix path construction
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
         pResponse->setContentType("text/html");
         pResponse->setFile(filePath, request);
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
   
   // Let other URLs be handled by the browser
   return false;
}

} // anonymous namespace
   
Error initialize()
{
   using boost::bind;
   using core::http::UriHandler;
   using namespace module_context;
   using namespace rstudio::r::function_hook;
   
   ExecBlock initBlock;
   initBlock.addFunctions()
      (bind(registerRBrowseUrlHandler, handleLocalHttpUrl))
      (bind(registerUriHandler, kAiLocation, handleAiRequest))
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

   return Success();
}

} // namespace ai
} // namespace modules
} // namespace session
} // namespace rstudio

