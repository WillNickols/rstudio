#
# SessionAi.R
#
# Copyright (C) 2022 by Posit Software, PBC
#
# Unless you have received this program directly from Posit Software pursuant
# to the terms of a commercial license agreement with Posit Software, then
# this program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#
#

# Original help/ai settings
options(ai_type = "html")

# Cached encoding of ai files for installed packages
.rs.setVar("packageAiEncodingEnv", new.env(parent = emptyenv()))

# Set up the httpdPort functions
.rs.addFunction("httpdPortIsFunction", function()
{
   is.function(tools:::httpdPort)
})

.rs.addFunction("httpdPort", function()
{
   if (.rs.httpdPortIsFunction())
      as.character(tools:::httpdPort())
   else
      as.character(tools:::httpdPort)
})

.rs.addFunction("initAi", function(port, isDesktop)
{ 
   # function to set the ai port
   setAiPort <- function() {
      if (.rs.httpdPortIsFunction()) {
         tools:::httpdPort(port)
      } else {
         env <- environment(tools::startDynamicHelp)
         unlockBinding("httpdPort", env)
         assign("httpdPort", port, envir = env)
         lockBinding("httpdPort", env)   
      }
   }
   
   # for desktop mode see if R can successfully initialize the httpd
   # server -- if it can't then perhaps localhost ports are blocked,
   # in this case we take over ai entirely
   if (isDesktop) 
   {
      # start the ai server if it hasn't previously been started
      # (suppress warnings and messages because if there is a problem
      # binding to a local port we are going to patch this up by 
      # redirecting all traffic to our local peer)
      if (.rs.httpdPort() <= 0L)
         suppressWarnings(suppressMessages(tools::startDynamicHelp()))
      
      # if couldn't start it then set the ai port directly so that
      # ai requests still flow through our local peer connection
      if (.rs.httpdPort() <= 0L)
      {
         setAiPort()
         return (TRUE)
      }
      else
      {
         return (FALSE)
      }
   }
   # always take over ai in server mode
   else 
   { 
      # stop the ai server if it was previously started e.g. by .Rprofile
      if (.rs.httpdPort() > 0L)
         suppressMessages(tools::startDynamicHelp(start=FALSE))
      
      # set the ai port
      setAiPort()
      
      # indicate we should handle custom internally
      return (TRUE)
   }
})

# Error handler for lookups
.rs.addFunction("handlerLookupError", function(path, query=NULL, ...)
{
   payload = paste(
      "<h3>R Custom HTTP Handler Not Found</h3>",
      "<p>Unable to locate custom HTTP handler for",
      "<i>", .rs.htmlEscape(path), "</i>",
      "<p>Is the package which implements this HTTP handler loaded?</p>")
   
   list(payload, "text/html", character(), 404)
})

# Environment for topics
.rs.setVar("topicsEnv", new.env(parent = emptyenv()))

# Search handler - this is the function that will handle Wikipedia searches
.rs.addJsonRpcHandler("search", function(query)
{
   # First, check and see if we can get an exact match in R help
   exactMatch <- help(query, help_type = "html")
   if (length(exactMatch) == 1)
   {
      print(exactMatch)
      return()
   }
   
   # Prepare to search Wikipedia
   # Create a directory for storing Wikipedia HTML in the ai/doc/html path
   aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
   dir.create(aiDir, recursive = TRUE, showWarnings = FALSE)
   
   # Format Wikipedia URL for the query
   wikiUrl <- sprintf("https://en.wikipedia.org/wiki/%s", utils::URLencode(query, reserved = TRUE))
   
   # Download Wikipedia content
   tryCatch({
      # Download the Wikipedia page content
      wikiContent <- suppressWarnings(readLines(wikiUrl, warn = FALSE))
      wikiContent <- paste(wikiContent, collapse = "\n")
      
      # Create a sanitized filename
      safeQuery <- gsub("[^a-zA-Z0-9]", "_", query)
      filePath <- file.path(aiDir, paste0(safeQuery, ".html"))
      writeLines(wikiContent, filePath)
      
      # Return the path to the downloaded file relative to ai/doc/html
      return(sprintf("ai/doc/html/%s.html", safeQuery))
   }, error = function(e) {
      # If Wikipedia search fails, fall back to original search
      status <- .rs.tryCatch(grep(query, "", perl = TRUE))
      if (inherits(status, "error"))
         query <- .rs.escapeForRegex(query)
      
      fmt <- "ai/doc/html/Search?pattern=%s&title=1&keyword=1&alias=1"
      sprintf(fmt, utils::URLencode(query, reserved = TRUE))
   })
})

# Handle links to Wikipedia topics
.rs.addJsonRpcHandler("follow_ai_topic", function(url)
{
   if (grepl("\\.html$", url)) {
      # Extract the filename from the URL
      topic <- sub("^.*/([^/]+)\\.html$", "\\1", url)
      
      # Return the full path to the file
      aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
      filePath <- file.path(aiDir, paste0(topic, ".html"))
      
      if (file.exists(filePath)) {
         return(filePath)
      }
   }
   
   # Otherwise use the original implementation for R packages
   rx <- "^.*/ai/library/([^/]*)/ai/(.*)$"
   pkg <- sub(rx, "\\1", url)
   topic <- utils::URLdecode(sub(rx, "\\2", url))

   .rs.followAiTopic(pkg = pkg, topic = topic)
})

# Original implementation of followAiTopic for R packages
.rs.addFunction("followAiTopic", function(pkg, topic)
{
   tryCatch({
      # first look in the specified package
      file <- utils::help(topic, package = (pkg), help_type = "text", try.all.packages = FALSE)

      # TODO: copy behaviour from utils:::str2logical()
      linksToTopics <- identical(Sys.getenv("_R_HELP_LINKS_TO_TOPICS_", "TRUE"), "TRUE")
      
      # check if topic.Rd exist 
      if (!length(file) && linksToTopics) {
         aipath <- system.file("ai", package = pkg)
         if (nzchar(aipath)) {
            contents <- readRDS(sub("/ai$", "/Meta/Rd.rds", aipath, fixed = FALSE))
            aifiles <- sub("\\.[Rr]d$", "", contents$File)
            if (topic %in% aifiles) file <- file.path(aipath, topic)
         }
      }

      # next, search for topic in all installed packages
      if (!length(file)) {
         file <- utils::help(topic, help_type = "text", try.all.packages = TRUE)
      }

      as.character(file)

   }, error = function(e) character())
})

# Helper function to escape regex
.rs.addFunction("escapeForRegex", function(string)
{
   # Escape all special regex chars
   gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", string)
})

# These functions are required for the original SessionAi.R functionality
# Basic structure maintained for compatibility

.rs.addFunction("makeAiCall", function(topic,
                                         package = NULL,
                                         ai_type = "html")
{
   # build ai call
   call <- substitute(
      utils::help(TOPIC, package = PACKAGE, help_type = "html"),
      list(TOPIC = topic, PACKAGE = package)
   )
   
   # support devtools shims
   if ("devtools_shims" %in% search())
      call[[1L]] <- quote(ai)
   
   # return generated call
   call
})

.rs.addFunction("RdLoadMacros", function(file)
{
   maybePackageDir <- dirname(dirname(file))
   if (file.exists(file.path(maybePackageDir, "DESCRIPTION")) ||
       file.exists(file.path(maybePackageDir, "DESCRIPTION.in")))
   {
      # NOTE: ?loadPkgRdMacros has:
      #
      #   loadPkgRdMacros loads the system Rd macros by default
      #
      # so it shouldn't be necessary to load system macros ourselves here
      tools::loadPkgRdMacros(maybePackageDir)
   }
   else
   {
      rMacroPath <- file.path(R.home("share"), "Rd/macros/system.Rd")
      tools::loadRdMacros(rMacroPath)
   }
})

.rs.addFunction("Rd2HTML", function(file, package = "")
{
   tf <- tempfile(); on.exit(unlink(tf))
   macros <- .rs.RdLoadMacros(file)
   tools::Rd2HTML(file, out = tf, package = package, macros = macros, dynamic = TRUE)
   lines <- readLines(tf, warn = FALSE)
   lines <- sub("R Documentation</td></tr></table>", "(preview) R Documentation</td></tr></table>", lines)
   if (nzchar(package))
   {
      # replace with "dev-figure" and parameters so that the server 
      # can look for figures in `man/` of the dev package
      lines <- sub('img src="figures/([^"]*)"', sprintf('img src="dev-figure?pkg=%s&figure=\\1"', package), lines)

      # add ?dev=<topic>
      lines <- gsub('a href="../../([^/]*/ai/)([^/]*)">', 'a href="/library/\\1\\2?dev=\\2">', lines)
   }
   
   paste(lines, collapse = "\n")
})
