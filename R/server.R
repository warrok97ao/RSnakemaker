server <- function(input, output, session) {
  # Kill background process on close
  session$onSessionEnded(function() {
    # Create shutdown signal file for background process
    file.create("snkmkr_shutdown.flag")

    # Delete all .txt files in the working directory (temp history files)
    txt_files <- list.files(pattern = "\\.txt$", full.names = TRUE)
    try(unlink(txt_files, force = TRUE), silent = TRUE)
    try(unlink("snkmkr_shutdown.flag"))
    if (exists("shiny_bg_process", envir = .GlobalEnv)) {
      proc <- get("shiny_bg_process", envir = .GlobalEnv)
      if (inherits(proc, "r_process") && proc$is_alive()) {
        proc$kill()
      }
    }
    stopApp()
  })

  # History paths
  hist_path_r <- "r_history.txt"
  hist_path_term <- "bash_history.txt"
  selected_history_path <- "selected_history.txt"

  # State holders
  recording <- reactiveVal(TRUE)
  selectedLines <- reactiveVal(character())
  last_history <- reactiveVal(list(r = character(), term = character()))
  shown_history <- reactiveVal(list(r = character(), term = character()))
  send_selected_now <- reactiveVal(FALSE)

  # On startup, set the correct button state
  observe({
    session$sendCustomMessage("updateRecordBtn", list(recording = recording()))
  })

  # Toggle recording
  observeEvent(input$toggle_record, {
    hist_r <- if (file.exists(hist_path_r)) {
      lines <- readLines(hist_path_r, warn = FALSE)
    } else {
      character()
    }
    hist_term <- if (file.exists(hist_path_term)) readLines(hist_path_term, warn = FALSE) else character()

    last_history(list(r = hist_r, term = hist_term))

    recording(!recording())
    session$sendCustomMessage("updateRecordBtn", list(recording = recording()))
    shiny::showNotification(
      if (recording()) "Start Recording" else "Stop Recording",
      type = if (recording()) "message" else "warning",
      duration = 2
    )
  })

  # Reactive display
  historyText <- reactiveVal("")
  observe({
    invalidateLater(200, session)
    history_type <- input$history_type %||% "r"
    hist_path <- if (history_type == "r") hist_path_r else hist_path_term
    hist_key <- if (history_type == "r") "r" else "term"

    if (recording()) {
      if (file.exists(hist_path)) {
        current_hist <- readLines(hist_path, warn = FALSE)

        new_hist <- current_hist
        if (length(new_hist) == 0) new_hist <- character()
        tmp <- shown_history()
        tmp[[hist_key]] <- new_hist
        shown_history(tmp)
        historyText(paste(new_hist, collapse = "\n"))
        snapshot <- last_history()
        snapshot[[hist_key]] <- current_hist
        last_history(snapshot)
      } else {
        historyText("No history file found.")
      }
    } else {
      historyText(paste(shown_history()[[hist_key]], collapse = "\n"))
    }
  })

  # History UI
  output$selectableHistory <- renderUI({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    all_lines <- shown_history()[[hist_key]]

    # Read len from file for consistency

    history_lines <- history_lines <- all_lines

    if (length(history_lines) == 0) {
      return(shiny::div("No new history available", style = "color: #666; font-style: italic;"))
    }

    history_elements <- lapply(seq_along(history_lines), function(i) {
      line <- history_lines[i]
      line_id <- paste0("history_line_", i)
      shiny::div(
        id = line_id,
        class = "history-line",
        `data-index` = i,
        onclick = paste0("toggleHistoryLine(", i, ", '",
                         gsub("'", "\\\\'", line), "', '", line_id, "');"),
        line
      )
    })

    do.call(shiny::tagList, history_elements)
  })

  # Restore selection after UI update
  observe({
    sel <- selectedLines()
    session$sendCustomMessage("restoreSelectedLines", list(commands = sel))
  })

  # JS event for multiple selections
  output$selectableHistory <- renderUI({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    all_lines <- shown_history()[[hist_key]]

    history_lines <- all_lines

    if (length(history_lines) == 0) {
      return(shiny::div("No new history available", style = "color: #666; font-style: italic;"))
    }

    history_elements <- lapply(seq_along(history_lines), function(i) {
      line <- history_lines[i]
      line_id <- paste0("history_line_", i)

      shiny::div(
        id = line_id,
        class = "history-line",
        `data-index` = i,
        onclick = paste0("toggleHistoryLine(", i, ", '",
                         gsub("'", "\\\\'", line), "', '", line_id, "');"),
        line
      )
    })

    do.call(shiny::tagList, history_elements)
  })

  # Send all history as JSON
  observeEvent(input$send_history_json, {
    tryCatch({
      history_type <- input$history_type %||% "r"
      hist_key <- if (history_type == "r") "r" else "term"
      displayed_history <- shown_history()[[hist_key]]

      json_data <- jsonlite::toJSON(list(
        command = "push_r",
        data = displayed_history
      ), auto_unbox = TRUE)
      selected_port <- isolate(input$port_input)
      target_url <- paste0("http://localhost:", selected_port)

      result <- httr::POST(
        url = target_url,
        body = json_data,
        encode = "json",
        httr::add_headers("Content-Type" = "application/json")
      )

      if (result$status_code == 200) {
        shiny::showNotification("Rules and files correctly generated!", type = "message")
      } else if (result$status_code == 400) {
        shiny::showNotification("Snakemaker is not listening to the selected port", type = "error")
      } else if (result$status_code == 505) {
        shiny::showNotification("Something went wrong", type = "error")
      } else {
        shiny::showNotification(paste("Unexpected status code:", result$status_code), type = "error")
      }
    }, error = function(e) {
      shiny::showNotification(
        paste("Failed to send to port", isolate(input$port_input), ":", e$message),
        type = "error"
      )
    })
  })

  # Step 1: Trigger the selected send separately
  observeEvent(input$send_selected_history_json, {
    send_selected_now(TRUE)
  })

  # Step 2: Actual sender (delayed one reactive cycle)
  observeEvent(send_selected_now(), {
    req(send_selected_now())
    send_selected_now(FALSE)

    tryCatch({
      # Read from file instead of using selectedLines()
      if (file.exists(selected_history_path)) {
        cmds <- readLines(selected_history_path, warn = FALSE)
      } else {
        cmds <- character()
      }
      if (length(cmds) == 0) {
        shiny::showNotification("No selected lines to send.", type = "warning")
        return()
      }

      json_data <- jsonlite::toJSON(list(
        command = "push_r",
        data = cmds
      ), auto_unbox = TRUE)

      selected_port <- isolate(input$port_input)
      target_url <- paste0("http://localhost:", selected_port)

      result <- httr::POST(
        url = target_url,
        body = json_data,
        encode = "json",
        httr::add_headers("Content-Type" = "application/json")
      )

      if (result$status_code == 200) {
        shiny::showNotification("Rules and files correctly generated from selection!", type = "message")
      } else if (result$status_code == 400) {
        shiny::showNotification("Snakemaker is not listening to the selected port", type = "error")
      } else if (result$status_code == 505) {
        shiny::showNotification("Something went wrong", type = "error")
      } else {
        shiny::showNotification(paste("Unexpected status code:", result$status_code), type = "error")
      }
    }, error = function(e) {
      shiny::showNotification(
        paste("Failed to send to port", isolate(input$port_input), ":", e$message),
        type = "error"
      )
    })
  })

  # Save selected lines to file whenever they change
  observeEvent(input$selected_lines, {
    cmds <- character()
    if (!is.null(input$selected_lines) && length(input$selected_lines) > 0) {
      # input$selected_lines is a character vector of commands
      cmds <- vapply(input$selected_lines, as.character, character(1))
      cmds <- cmds[!is.na(cmds) & cmds != ""]
    }
    selectedLines(cmds)
    # Overwrite the file with only the command text, one per line
    try({
      if (length(cmds) == 0) {
        # If nothing is selected, create an empty file
        file.create(selected_history_path)
        writeLines(character(0), selected_history_path, useBytes = TRUE)
      } else {
        writeLines(cmds, selected_history_path, useBytes = TRUE)
      }
    }, silent = TRUE)
  })

  # Export history handler
  output$export_history_btn <- downloadHandler(
    filename = function() {
      history_type <- input$history_type %||% "r"
      if (history_type == "r") {
        "r_history_export.txt"
      } else {
        "bash_history_export.txt"
      }
    },
    content = function(file) {
      history_type <- input$history_type %||% "r"
      hist_key <- if (history_type == "r") "r" else "term"
      lines <- shown_history()[[hist_key]]
      writeLines(lines, file, useBytes = TRUE)
    }
  )
}

