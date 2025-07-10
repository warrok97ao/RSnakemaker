snkmkr <- function() {
  # List of required packages
  pkgs <- c("shiny", "callr", "later", "rstudioapi", "jsonlite", "httr", "bslib", "keyring")
  # Install any missing packages
  to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(to_install) > 0) {
    install.packages(to_install)
  }

  suppressPackageStartupMessages(library(shiny))
  suppressPackageStartupMessages(library(callr))
  suppressPackageStartupMessages(library(later))
  suppressPackageStartupMessages(library(rstudioapi))
  suppressPackageStartupMessages(library(jsonlite))
  suppressPackageStartupMessages(library(httr))
  suppressPackageStartupMessages(library(bslib))
  suppressPackageStartupMessages(library(keyring))


  # Add global variable to track history pause
  pause_history_collection <- FALSE
  assign("pause_history_collection", pause_history_collection, envir = .GlobalEnv)

  source("R/server.R")
  source("R/ui.R")

  is_port_ready <- function(port = 8080, retries = 5) {
    for (i in 1:retries) {
      tryCatch({
        con <- suppressWarnings(socketConnection("localhost", port = port,
                                                 server = FALSE,
                                                 blocking = FALSE,
                                                 timeout = 1))
        close(con)
        return(TRUE)
      }, error = function(e) {
        Sys.sleep(0.2)
        FALSE
      })
    }
    FALSE
  }


  get_r_history <- function() {
    # Check if history collection is paused
    if (get("pause_history_collection", envir = .GlobalEnv)) {
      return(character())
    }

    history_file <- tempfile(fileext = ".Rhistory")
    tryCatch({
      utils::savehistory(history_file)
      if (file.exists(history_file)) {
        hist_lines <- rev(readLines(history_file))

        # Check if unique filtering is enabled
        unique_flag_path <- "unique_r_history_flag.txt"
        if (file.exists(unique_flag_path) && readLines(unique_flag_path, n = 1, warn = FALSE) == "TRUE") {
          hist_lines <- unique(hist_lines)
        }

        filtered <- hist_lines[!grepl("snkmkr\\(\\)", hist_lines)]
        # Always create or overwrite r_history.txt
        file.create("r_history.txt")
        writeLines(filtered, "r_history.txt")
        filtered
      } else {
        # Always create r_history.txt if missing
        file.create("r_history.txt")
        "No R history available!"
      }
    }, error = function(e) {
      file.create("r_history.txt")
      paste("Error accessing R history:", e$message)
    })
  }

  get_term_history <- function() {
    # Check if history collection is paused
    if (get("pause_history_collection", envir = .GlobalEnv)) {
      return(character())
    }

    history_file <- path.expand("~/.bash_history")
    if (file.exists(history_file)) {
      history_lines <- readLines(history_file)
      unique_lines <- unique(rev(history_lines))
      # Always create or overwrite bash_history.txt
      file.create("bash_history.txt")
      writeLines(unique_lines, "bash_history.txt")
      unique_lines
    } else {
      # Always create bash_history.txt if missing
      file.create("bash_history.txt")
      "No terminal history available!"
    }
  }

  rstudioapi::registerCommandCallback("newTerminal", function(...) {
    later::later(function() {
      termId <- rstudioapi::terminalVisible()
      rstudioapi::terminalSend(termId, "PROMPT_COMMAND='history -a'\n")
    }, delay = 3)
  })

  # Create the app
  app <- shinyApp(
    ui = create_ui(
      character()
    ),
    server = server
  )

  # Start background process
  shiny_bg_process <- callr::r_bg(function(app) {
    shiny::runApp(app, port = 8080)
  }, args = list(app))
  assign("shiny_bg_process", shiny_bg_process, envir = .GlobalEnv)

  # Wait for port to be ready
  Sys.sleep(0.5)
  attempts <- 0
  while (!is_port_ready() && attempts < 10) {
    Sys.sleep(0.2)
    attempts <- attempts + 1
  }

  Sys.sleep(1)
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer("http://localhost:8080")
  }

  # Recursive monitoring function
  recursive_check <- function(interval = 0.2) {
    # Update history files only if not paused
    if (!get("pause_history_collection", envir = .GlobalEnv)) {
      get_r_history()
      get_term_history()
    }

    later::later(recursive_check, interval)
  }

  # Start recursive monitoring
  recursive_check()

  invisible(NULL)
}
