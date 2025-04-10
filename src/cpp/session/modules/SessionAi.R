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
   conversationLogPath <- file.path(aiDir, "conversation_log.json")
   
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
   writeLines(.rs.jsontostr(initialJson), jsonFilePath)
   
   # Create initial conversation log with just the system instructions
   initialLog <- list(
      list(
         role = "developer",
         content = "Be a data science assistant that writes any necessary scripts in the language R. Be as concise as possible."
      )
   )
   # Write JSON preserving exact structure
   writeLines(jsonlite::toJSON(initialLog, auto_unbox = TRUE), conversationLogPath)
   
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
   conversationLogPath <- file.path(aiDir, "conversation_log.json")
   
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
   writeLines(.rs.jsontostr(initialJson), jsonFilePath)
   
   # Create initial conversation log with just the system instructions
   initialLog <- list(
      list(
         role = "developer",
         content = "Be a data science assistant that writes any necessary scripts in the language R. Be as concise as possible."
      )
   )
   # Write JSON preserving exact structure
   writeLines(jsonlite::toJSON(initialLog, auto_unbox = TRUE), conversationLogPath)
   
   # Update the HTML display
   .rs.updateConversationDisplay()
   
   # Return success
   return(TRUE)
})

# Environment for topics
.rs.setVar("topicsEnv", new.env(parent = emptyenv()))

# Helper function to convert JSON to string safely
.rs.addFunction("jsontostr", function(obj) {
   jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, force = TRUE, na = "null", null = "null")
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
      ".text { font-family: sans-serif; font-size: 14px; line-height: 1.4; white-space: pre-wrap; }",
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
         
         # Escape HTML special characters to prevent XSS and ensure proper display
         msgText <- gsub("&", "&amp;", msgText)
         msgText <- gsub("<", "&lt;", msgText)
         msgText <- gsub(">", "&gt;", msgText)
         
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
   
   # Path for conversation log file that will store the full conversation history
   conversationLogPath <- file.path(aiDir, "conversation_log.json")
   
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
   
   # Create the conversation log file if it doesn't exist
   if (!file.exists(conversationLogPath)) {
      # Create initial conversation log with just the system instructions
      initialLog <- list(
         list(
            role = "developer",
            content = "Be a data science assistant that writes any necessary scripts in the language R. Be as concise as possible."
         )
      )
      # Write JSON preserving exact structure
      writeLines(jsonlite::toJSON(initialLog, auto_unbox = TRUE), conversationLogPath)
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
   
   # Read the conversation log preserving the exact structure
   conversationLog <- tryCatch({
      jsonlite::fromJSON(conversationLogPath, simplifyVector = FALSE)
   }, error = function(e) {
      # Start with just the system instructions if log can't be read
      list(
         list(
            role = "developer",
            content = "Be a data science assistant that writes any necessary scripts in the language R. Be as concise as possible."
         )
      )
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
   
   # Add the user message to the conversation log
   conversationLog <- c(conversationLog, list(list(role = "user", content = query)))
   
   # Initialize api_response with a default value
   api_response <- "Error: API response not received"
   
   # Make API call to OpenAI
   tryCatch({
      # Get API key from environment variable
      api_key <- Sys.getenv("OPENAI_API_KEY")
      if (api_key == "") {
         stop("OPENAI_API_KEY environment variable is not set")
      }
      
      # Use the correct Chat Completions API endpoint
      response <- httr2::request("https://api.openai.com/v1/chat/completions") |>
          httr2::req_headers(
              "Content-Type" = "application/json",
              "Authorization" = paste("Bearer", api_key)
          ) |>
          httr2::req_body_json(list(
              model = "gpt-4o",
              messages = conversationLog
          )) |>
          httr2::req_perform() |>
          httr2::resp_body_json()
      
      # Extract the response text from the API response
      api_response <- response$choices[[1]]$message$content
   }, error = function(e) {
      # If API call fails, provide a fallback message with more detail
      api_response <<- paste("Error making API call:", e$message)
   })

   # Add the assistant response to the conversation log
   conversationLog <- c(conversationLog, list(list(role = "assistant", content = api_response)))
   
   # Save the updated conversation log
   writeLines(jsonlite::toJSON(conversationLog, auto_unbox = TRUE), conversationLogPath)
   
   # Extract R code blocks from the response and show them in viewer
   cleaned_response <- .rs.extractRCodeFromResponse(api_response)
   
   # Add the API response (with code blocks removed from display)
   systemResponse <- data.frame(
      type = "assistant",
      text = cleaned_response,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
   )
   
   # Append the system response
   conversation$messages <- rbind(conversation$messages, systemResponse)
   
   # Save the updated conversation
   writeLines(.rs.jsontostr(conversation), jsonFilePath)
   
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
   
   # Extract R code blocks from the response and show them in viewer
   cleaned_response <- .rs.extractRCodeFromResponse(response)
   
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
      text = cleaned_response,
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
   writeLines(.rs.jsontostr(conversation), jsonFilePath)
   
   # Update the HTML display
   .rs.updateConversationDisplay()
   
   # Return success
   return(TRUE)
})

# Function to extract R code blocks from API responses and display them in the script viewer
.rs.addFunction("extractRCodeFromResponse", function(response) {
   # Initialize the cleaned response with the original
   cleaned_response <- response
   
   if (is.null(response)) {
       return(response)
   }
   
   # Check if response contains R code blocks
   if (grepl("```R", response, fixed = TRUE) || grepl("```r", response, fixed = TRUE)) {
      # Extract code between ```R and ``` markers
       pattern <- "```[Rr]\\s*\\n([\\s\\S]*?)```"
       matches <- regmatches(response, gregexpr(pattern, response, perl = TRUE))
       
       if (length(matches) > 1) {
           stop(paste0("AI returned more than 1 code block: ", response))
       }
      
      if (matches[[1]][1] != "") {
         # Create a file with a snake_case name
         rOutputDir <- file.path(.libPaths()[1], "ai", "output")
         dir.create(rOutputDir, recursive = TRUE, showWarnings = FALSE)
         
         # Check if a previous R script exists
         mostRecentScript <- NULL
         previousScripts <- list.files(rOutputDir, pattern = "^ai_response_.*\\.R$", full.names = TRUE)
         if (length(previousScripts) > 0) {
            # Sort by modification time to find the most recent
            fileInfo <- file.info(previousScripts)
            mostRecentScript <- rownames(fileInfo)[which.max(fileInfo$mtime)]
            
            # Send API call to ask about overwriting
            api_key <- Sys.getenv("OPENAI_API_KEY")
            if (api_key != "") {
               # Get the conversation log to provide context but don't modify it
               conversationLogPath <- file.path(.libPaths()[1], "ai", "doc", "html", "conversation_log.json")
               conversationLog <- jsonlite::fromJSON(conversationLogPath, simplifyVector = FALSE)
               
               # Create a copy of the conversation log for the API call
               overwriteQuestion <- c(
                  conversationLog,
                  list(list(
                     role = "user",
                     content = "Should this be a new script or overwrite the previous script? Only answer with the single word 'New' or 'Overwrite'."
                  ))
               )
               
               # Try up to 3 times to get a clear "New" or "Overwrite" response
               maxRetries <- 3
               retryCount <- 0
               overwriteResponse <- NULL
               validResponse <- FALSE

               while (!validResponse && retryCount < maxRetries) {
                  overwriteResponse <- tryCatch({
                      overwriteResponse <- httr2::request("https://api.openai.com/v1/chat/completions") |>
                        httr2::req_headers(
                           "Content-Type" = "application/json",
                           "Authorization" = paste("Bearer", api_key)
                        ) |>
                        httr2::req_body_json(list(
                           model = "gpt-4o",
                           messages = overwriteQuestion
                        )) |>
                        httr2::req_perform() |>
                        httr2::resp_body_json()
                  
                     # Extract the response text
                      overwriteResponse$choices[[1]]$message$content
                  }, error = function(e) {
                     stop("Couldn't decide whether to overwrite the previous script.")
                  })
                  
                  # Trim whitespace and check if response starts with "New" or "Overwrite"
                  trimmedResponse <- trimws(overwriteResponse)
                  if (grepl("^[Nn]ew", trimmedResponse) || grepl("^[Oo]verwrite", trimmedResponse)) {
                     validResponse <- TRUE
                  } else {
                     # Create a new question with clearer instructions
                     overwriteQuestion <- c(
                        conversationLog,
                        list(list(
                           role = "user",
                           content = paste0("Should this be a new script or overwrite the previous script? Your previous answer was unclear. ",
                                           "Please respond with ONLY the single word 'New' or 'Overwrite' and nothing else.")
                        ))
                     )
                     retryCount <- retryCount + 1
                  }
               }

               if (!validResponse) {
                  stop("Failed to get a clear 'New' or 'Overwrite' response after multiple attempts.")
               }

               # Decide based on the response
               shouldOverwrite <- grepl("^[Oo]verwrite", trimmedResponse, perl = TRUE)
            } else {
               stop("No API key found.")
            }
         } else {
            # No previous script, always create new
            shouldOverwrite <- FALSE
         }
         
         # Generate a filename
         if (shouldOverwrite && !is.null(mostRecentScript)) {
            fileName <- mostRecentScript
         } else {
            # Generate a timestamped filename for a new file
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            fileName <- file.path(rOutputDir, paste0("ai_response_", timestamp, ".R"))
         }
         
        codeBlockWithMarkers <- matches[[1]][1]
        
        # Extract just the code without the markers
        codeBlock <- gsub("```[Rr]\\s*\\n|```", "", codeBlockWithMarkers, perl = TRUE)

         # Write the code to the file
         writeLines(codeBlock, fileName)
         
         # Use the RStudio API to display the file in the editor pane
         .rs.api.documentOpen(fileName)
         
         cleaned_response = sub(codeBlock, '', response, fixed = T)
      }
   }
   
   return(cleaned_response)
})
