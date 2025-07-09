server <- function(input, output, session) {
  # Cleanup actions when the Shiny session ends
  session$onSessionEnded(function() {
    # Delete any .txt files in the current directory
    txt_files <- list.files(pattern = "\\.txt$", full.names = TRUE)
    try(unlink(txt_files, force = TRUE), silent = TRUE)

    # Kill background process if it exists and is still running
    if (exists("shiny_bg_process", envir = .GlobalEnv)) {
      proc <- get("shiny_bg_process", envir = .GlobalEnv)
      if (inherits(proc, "r_process") && proc$is_alive()) {
        proc$kill()
      }
    }

    # Stop the app cleanly
    stopApp()
  })

  # File paths for history tracking
  hist_path_r <- "r_history.txt"
  hist_path_term <- "bash_history.txt"
  selected_history_path <- "selected_history.txt"

  # Reactive values to manage app state
  recording <- reactiveVal(TRUE)  # Whether history is being tracked
  selectedLines <- reactiveVal(character())  # Currently selected lines
  last_history <- reactiveVal(list(r = character(), term = character()))  # Last read snapshot
  shown_history <- reactiveVal(list(r = character(), term = character()))  # All shown lines
  send_selected_now <- reactiveVal(FALSE)  # Trigger to send selected lines
  hidden_line_indices <- reactiveVal(list(r = integer(), term = integer()))  # Indices of hidden lines

  # Update frontend recording button state
  observe({
    session$sendCustomMessage("updateRecordBtn", list(recording = recording()))
  })

  # Toggle recording state and capture current snapshot
  observeEvent(input$toggle_record, {
    hist_r <- if (file.exists(hist_path_r)) readLines(hist_path_r, warn = FALSE) else character()
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

  # Continuously update history text if recording is active
  historyText <- reactiveVal("")
  observe({
    invalidateLater(200, session)

    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    hist_path <- if (history_type == "r") hist_path_r else hist_path_term

    if (recording()) {
      if (file.exists(hist_path)) {
        current_hist <- readLines(hist_path, warn = FALSE)

        # Append only new lines to the shown history
        tmp <- shown_history()
        existing <- tmp[[hist_key]]
        new_lines <- setdiff(current_hist, existing)
        tmp[[hist_key]] <- c(existing, new_lines)
        shown_history(tmp)

        # Update last snapshot
        snapshot <- last_history()
        snapshot[[hist_key]] <- current_hist
        last_history(snapshot)

        historyText(paste(current_hist, collapse = "\n"))
      } else {
        historyText("No history file found.")
      }
    } else {
      # If not recording, show only previously captured lines
      historyText(paste(shown_history()[[hist_key]], collapse = "\n"))
    }
  })

  # Render selectable history (excluding hidden lines)
  output$selectableHistory <- renderUI({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"

    all_lines <- shown_history()[[hist_key]]
    hidden_indices <- hidden_line_indices()[[hist_key]]

    if (length(all_lines) == 0) {
      return(shiny::div("No history available.", style = "color: #666; font-style: italic;"))
    }

    visible_indices <- setdiff(seq_along(all_lines), hidden_indices)
    if (length(visible_indices) == 0) {
      return(shiny::div("No new history available.", style = "color: #666; font-style: italic;"))
    }

    # Generate clickable divs for each visible line
    history_elements <- lapply(visible_indices, function(i) {
      line <- all_lines[[i]]
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

  # Render archived (hidden) lines separately
  output$archivedHistory <- renderUI({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"

    all_lines <- shown_history()[[hist_key]]
    hidden_indices <- hidden_line_indices()[[hist_key]]

    if (length(hidden_indices) == 0) {
      return(shiny::div("No archived lines.", style = "color: #666; font-style: italic;"))
    }

    archived_elements <- lapply(hidden_indices, function(i) {
      if (i > 0 && i <= length(all_lines)) {
        line <- all_lines[[i]]
        shiny::div(
          class = "history-line archived-line",
          line
        )
      }
    })

    do.call(shiny::tagList, archived_elements)
  })

  # Restore selected lines on frontend
  observe({
    sel <- selectedLines()
    session$sendCustomMessage("restoreSelectedLines", list(commands = sel))
  })

  # Trigger to send selected history to backend
  observeEvent(input$send_selected_history_json, {
    send_selected_now(TRUE)
  })

  # Send selected commands to backend server
  observeEvent(send_selected_now(), {
    req(send_selected_now())
    send_selected_now(FALSE)

    tryCatch({
      cmds <- if (file.exists(selected_history_path)) readLines(selected_history_path, warn = FALSE) else character()

      if (length(cmds) == 0) {
        shiny::showNotification("No selected lines to send.", type = "warning")
        return()
      }

      json_data <- jsonlite::toJSON(list(command = "push_r", data = cmds), auto_unbox = TRUE)
      selected_port <- isolate(input$port_input)
      target_url <- paste0("http://localhost:", selected_port)

      result <- httr::POST(
        url = target_url,
        body = json_data,
        encode = "json",
        httr::add_headers("Content-Type" = "application/json")
      )

      if (result$status_code == 200) {
        # Hide the lines that were just sent
        hist_key <- input$history_type %||% "r"
        all_lines <- shown_history()[[hist_key]]
        hidden_idx <- which(all_lines %in% cmds)

        current_hidden <- hidden_line_indices()
        current_hidden[[hist_key]] <- unique(c(current_hidden[[hist_key]], hidden_idx))
        hidden_line_indices(current_hidden)

        shiny::showNotification("Rules and files correctly generated from selection!", type = "message")
      } else if (result$status_code == 400) {
        shiny::showNotification("Snakemaker is not listening to the selected port", type = "error")
      } else {
        shiny::showNotification("Unexpected error occurred.", type = "error")
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Store currently selected lines into file and reactive variable
  observe({
    cmds <- character()
    selected <- input$selected_lines
    if (!is.null(selected) && length(selected) > 0) {
      cmds <- vapply(selected, as.character, character(1))
      cmds <- cmds[!is.na(cmds) & cmds != ""]
    }
    selectedLines(cmds)

    try({
      writeLines(cmds, selected_history_path, useBytes = TRUE)
    }, silent = TRUE)
  })

  # Send all visible (non-hidden) history lines to backend
  observeEvent(input$send_history_json, {
    tryCatch({
      history_type <- input$history_type %||% "r"
      hist_key <- if (history_type == "r") "r" else "term"
      all_history <- shown_history()[[hist_key]]
      hidden_indices <- hidden_line_indices()[[hist_key]]

      visible_indices <- setdiff(seq_along(all_history), hidden_indices)
      history_to_send <- all_history[visible_indices]

      if (length(history_to_send) == 0) {
        shiny::showNotification("No new history to send.", type = "warning")
        return()
      }

      json_data <- jsonlite::toJSON(list(command = "push_r", data = history_to_send), auto_unbox = TRUE)
      selected_port <- isolate(input$port_input)
      target_url <- paste0("http://localhost:", selected_port)

      result <- httr::POST(
        url = target_url,
        body = json_data,
        encode = "json",
        httr::add_headers("Content-Type" = "application/json")
      )

      if (result$status_code == 200) {
        # Hide all visible lines after sending
        current_hidden <- hidden_line_indices()
        current_hidden[[hist_key]] <- unique(c(current_hidden[[hist_key]], visible_indices))
        hidden_line_indices(current_hidden)

        shiny::showNotification("Rules and files correctly generated from visible history!", type = "message")
      } else if (result$status_code == 400) {
        shiny::showNotification("Snakemaker is not listening to the selected port", type = "error")
      } else if (result$status_code == 505) {
        shiny::showNotification("Something went wrong", type = "error")
      } else {
        shiny::showNotification(paste("Unexpected status code:", result$status_code), type = "error")
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Clear visible history by marking all lines as hidden
  observeEvent(input$clear_history_btn, {
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    all_lines <- shown_history()[[hist_key]]
    new_hidden <- hidden_line_indices()
    new_hidden[[hist_key]] <- seq_along(all_lines)
    hidden_line_indices(new_hidden)

    shiny::showNotification("History view has been cleared.", type = "message", duration = 2)
  })

  # Allow download/export of current full history (including hidden lines)
  output$export_history_btn <- downloadHandler(
    filename = function() {
      history_type <- input$history_type %||% "r"
      if (history_type == "r") "r_history_export.txt" else "bash_history_export.txt"
    },
    content = function(file) {
      history_type <- input$history_type %||% "r"
      hist_key <- if (history_type == "r") "r" else "term"
      lines <- shown_history()[[hist_key]]
      writeLines(lines, file, useBytes = TRUE)
    }
  )

  # Display current hidden line indices (for debugging)
  output$debug_len_box <- renderUI({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    hidden <- hidden_line_indices()[[hist_key]]
    if (length(hidden) > 0) {
      shiny::div(class = "debug-box", paste("Hidden line indices:", paste(hidden, collapse = ", ")))
    }
  })
}
