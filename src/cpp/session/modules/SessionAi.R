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

# Add a new JSON RPC handler for clearing the conversation data when refresh is clicked
.rs.addJsonRpcHandler("clear_ai_conversation", function()
{
   # Path for our JSON file
   aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
   jsonFilePath <- file.path(aiDir, "conversation.json")
   
   # Create the directory if it doesn't exist
   if (!dir.exists(aiDir)) {
      dir.create(aiDir, recursive = TRUE, showWarnings = FALSE)
   }
   
   # Create initial empty conversation structure
   initialJson <- list(
      messages = data.frame(
         type = character(),
         text = character(),
         timestamp = character(),
         stringsAsFactors = FALSE
      )
   )
   
   # Write the empty JSON structure to the file
   writeLines(jsonlite::toJSON(initialJson, pretty = TRUE), jsonFilePath)
   
   # Update the HTML display
   .rs.updateConversationDisplay()
   
   # Return success
   return(TRUE)
})

# Add the exported R function that will be called from C++
.rs.addFunction("clear_ai_conversation", function()
{
   # Path for our JSON file
   aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
   jsonFilePath <- file.path(aiDir, "conversation.json")
   
   # Create the directory if it doesn't exist
   if (!dir.exists(aiDir)) {
      dir.create(aiDir, recursive = TRUE, showWarnings = FALSE)
   }
   
   # Create initial empty conversation structure
   initialJson <- list(
      messages = data.frame(
         type = character(),
         text = character(),
         timestamp = character(),
         stringsAsFactors = FALSE
      )
   )
   
   # Write the empty JSON structure to the file
   writeLines(jsonlite::toJSON(initialJson, pretty = TRUE), jsonFilePath)
   
   # Update the HTML display
   .rs.updateConversationDisplay()
   
   # Return success
   return(TRUE)
})

# Environment for topics
.rs.setVar("topicsEnv", new.env(parent = emptyenv()))

# Helper function to convert JSON to string safely
.rs.addFunction("jsontostr", function(obj) {
   jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE)
})

# Function to update the conversation display HTML
.rs.addFunction("updateConversationDisplay", function() {
   # Path for our JSON and display files
   aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
   jsonFilePath <- file.path(aiDir, "conversation.json")
   displayFilePath <- file.path(aiDir, "conversation_display.html")
   
   # Read the current conversation
   conversation <- tryCatch({
      jsonlite::fromJSON(jsonFilePath)
   }, error = function(e) {
      list(messages = data.frame(
         type = character(),
         text = character(),
         timestamp = character(),
         stringsAsFactors = FALSE
      ))
   })
   
   # Create HTML to display the conversation
   html <- paste0(
      "<html><head><title>Conversation</title>",
      "<style>",
      "body { font-family: sans-serif; margin: 20px; }",
      ".message { margin-bottom: 15px; padding: 10px; font-family: sans-serif; font-size: 14px; }",
      ".user { background-color: #e6e6e6; text-align: right; border-radius: 5px; display: inline-block; float: right; max-width: 100%; word-wrap: break-word; }",
      ".assistant { background-color: transparent; text-align: left; word-wrap: break-word; max-width: 100%; }",
      ".user-container { width: 100%; overflow: hidden; text-align: right; }",
      ".text { font-family: sans-serif; font-size: 14px; line-height: 1.4; }",
      "</style>",
      "<script>",
      "window.onload = function() {",
      "  window.scrollTo(0, document.body.scrollHeight);",
      "};",
      "</script>",
      "</head><body>"
   )
   
   # Handle the messages data frame - check that it's a data frame and has rows
   if (!is.null(conversation$messages) && is.data.frame(conversation$messages) && nrow(conversation$messages) > 0) {
      for (i in 1:nrow(conversation$messages)) {
         msgType <- conversation$messages$type[i]
         msgText <- conversation$messages$text[i]
         
         if (is.null(msgType) || is.na(msgType)) msgType <- "unknown"
         if (is.null(msgText) || is.na(msgText)) msgText <- "(no text)"
         
         msgClass <- if (msgType == "user") "user" else "assistant"
         
         if (msgType == "user") {
            html <- paste0(html, 
               '<div class="user-container">',
               sprintf('<div class="message %s">', msgClass),
               sprintf('<div class="text">%s</div>', msgText),
               '</div></div>'
            )
         } else {
            html <- paste0(html, 
               sprintf('<div class="message %s">', msgClass),
               sprintf('<div class="text">%s</div>', msgText),
               '</div>'
            )
         }
      }
   }
   
   html <- paste0(html, "</body></html>")
   
   # Write the HTML to the display file
   writeLines(html, displayFilePath)
   
   return(TRUE)
})

# Search handler - this is the function that will handle Wikipedia searches
.rs.addJsonRpcHandler("search", function(query)
{
   # Create a directory for storing the JSON file in the ai/doc/html path
   aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
   dir.create(aiDir, recursive = TRUE, showWarnings = FALSE)
   
   # Path for our JSON file
   jsonFilePath <- file.path(aiDir, "conversation.json")
   
   # Create the JSON file if it doesn't exist
   if (!file.exists(jsonFilePath)) {
      # Create initial empty conversation structure
      initialJson <- list(
         messages = data.frame(
            type = character(),
            text = character(),
            timestamp = character(),
            stringsAsFactors = FALSE
         )
      )
      writeLines(.rs.jsontostr(initialJson), jsonFilePath)
   }
   
   # Read the current conversation
   conversation <- tryCatch({
      jsonlite::fromJSON(jsonFilePath)
   }, error = function(e) {
      list(messages = data.frame(
         type = character(),
         text = character(),
         timestamp = character(),
         stringsAsFactors = FALSE
      ))
   })
   
   # Add the new user message as a data frame row
   newMessage <- data.frame(
      type = "user",
      text = query,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
   )
   
   # Append the new message
   if (is.null(conversation$messages) || !is.data.frame(conversation$messages) || nrow(conversation$messages) == 0) {
      conversation$messages <- newMessage
   } else {
      conversation$messages <- rbind(conversation$messages, newMessage)
   }
   
   # Add the system's echo response
   systemResponse <- data.frame(
      type = "assistant",
      text = query,  # Echo the same text back
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
   )
   
   # Append the system response
   conversation$messages <- rbind(conversation$messages, systemResponse)
   
   # Save the updated conversation
   writeLines(jsonlite::toJSON(conversation, pretty = TRUE), jsonFilePath)
   
   # Update the HTML display
   .rs.updateConversationDisplay()
   
   # Return the path to the display file
   return(sprintf("ai/doc/html/conversation_display.html"))
})

# Handle links to Wikipedia topics
.rs.addJsonRpcHandler("follow_ai_topic", function(url)
{
   # Check if this is our conversation display
   if (grepl("conversation_display\\.html$", url)) {
      aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
      displayFilePath <- file.path(aiDir, "conversation_display.html")
      
      if (file.exists(displayFilePath)) {
         return(displayFilePath)
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

# Function to add AI responses to the conversation
.rs.addJsonRpcHandler("add_ai_response", function(response)
{
   # Path for our JSON file
   aiDir <- file.path(.libPaths()[1], "ai", "doc", "html")
   jsonFilePath <- file.path(aiDir, "conversation.json")
   
   # Read the current conversation
   conversation <- tryCatch({
      jsonlite::fromJSON(jsonFilePath)
   }, error = function(e) {
      list(messages = data.frame(
         type = character(),
         text = character(),
         timestamp = character(),
         stringsAsFactors = FALSE
      ))
   })
   
   # Add the new AI response message as a data frame row
   newMessage <- data.frame(
      type = "assistant",
      text = response,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
   )
   
   # Append the new message
   if (is.null(conversation$messages) || nrow(conversation$messages) == 0) {
      conversation$messages <- newMessage
   } else {
      conversation$messages <- rbind(conversation$messages, newMessage)
   }
   
   # Save the updated conversation
   writeLines(jsonlite::toJSON(conversation, pretty = TRUE), jsonFilePath)
   
   # Update the HTML display
   .rs.updateConversationDisplay()
   
   # Return success
   return(TRUE)
})
