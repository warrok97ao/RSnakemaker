server <- function(input, output, session) {
  # Helper to get visible (non-hidden, non-deleted) indices
  get_visible_indices <- function(hist_key) {
    all_lines <- shown_history()[[hist_key]]
    hidden <- hidden_line_indices()[[hist_key]]
    deleted <- delete_line_indices()[[hist_key]]
    setdiff(seq_along(all_lines), union(hidden, deleted))
  }

  # Cleanup actions when the Shiny session ends
  session$onSessionEnded(function() {
    txt_files <- list.files(pattern = "\\.txt$", full.names = TRUE)
    try(unlink(txt_files, force = TRUE), silent = TRUE)

    if (exists("shiny_bg_process", envir = .GlobalEnv)) {
      proc <- get("shiny_bg_process", envir = .GlobalEnv)
      if (inherits(proc, "r_process") && proc$is_alive()) {
        proc$kill()
      }
    }

    stopApp()
  })

  # File paths
  hist_path_r <- "r_history.txt"
  hist_path_term <- "bash_history.txt"
  selected_history_path <- "selected_history.txt"

  # Reactive state
  recording <- reactiveVal(TRUE)
  selectedLines <- reactiveVal(list(r = character(), term = character()))
  selected_index <- reactiveVal(list(r=character(), term= character()))
  last_history <- reactiveVal(list(r = character(), term = character()))
  shown_history <- reactiveVal(list(r = character(), term = character()))
  send_selected_now <- reactiveVal(FALSE)
  hidden_line_indices <- reactiveVal(list(r = integer(), term = integer()))
  delete_line_indices <- reactiveVal(list(r = integer(), term = integer()))

  # Track the last index at which recording was paused for each history type
  pause_start_index <- reactiveVal(list(r = 0L, term = 0L))

  observe({
    session$sendCustomMessage("updateRecordBtn", list(recording = recording()))
  })

  observeEvent(input$toggle_record, {
    hist_r <- if (file.exists(hist_path_r)) readLines(hist_path_r, warn = FALSE) else character()
    hist_term <- if (file.exists(hist_path_term)) readLines(hist_path_term, warn = FALSE) else character()

    last_history(list(r = hist_r, term = hist_term))

    # If we are pausing (i.e., going from recording to not recording), store the current length
    if (recording()) {
      # About to pause
      pause_idx <- pause_start_index()
      pause_idx$r <- length(hist_r)
      pause_idx$term <- length(hist_term)
      pause_start_index(pause_idx)
    }

    recording(!recording())
    session$sendCustomMessage("updateRecordBtn", list(recording = recording()))

    shiny::showNotification(
      if (recording()) "Start Recording" else "Stop Recording",
      type = if (recording()) "message" else "warning",
      duration = 2
    )
  })

  historyText <- reactiveVal("")
  observe({
    invalidateLater(200, session)

    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    hist_path <- if (history_type == "r") hist_path_r else hist_path_term

    if (file.exists(hist_path)) {
      current_hist <- rev(readLines(hist_path, warn = FALSE))
    } else {
      current_hist <- character()
    }

    tmp <- shown_history()
    tmp[[hist_key]] <- current_hist
    shown_history(tmp)

    if (!recording()) {
      # Only flag new lines (after pause) as deleted
      pause_idx <- pause_start_index()[[hist_key]]
      n_lines <- length(current_hist)
      if (n_lines > pause_idx) {
        new_idx <- seq.int(pause_idx + 1, n_lines)
        deleted <- delete_line_indices()
        deleted[[hist_key]] <- unique(c(deleted[[hist_key]], new_idx))
        delete_line_indices(deleted)
      }
    }

    historyText(paste(rev(current_hist), collapse = "\n"))
  })

  output$selectableHistory <- renderUI({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"

    all_lines <- shown_history()[[hist_key]]
    visible_indices <- get_visible_indices(hist_key)

    if (length(all_lines) == 0) {
      return(shiny::div("No history available.", style = "color: #666; font-style: italic;"))
    }

    if (length(visible_indices) == 0) {
      return(shiny::div("No new history available.", style = "color: #666; font-style: italic;"))
    }

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

    # After rendering, trigger scroll to bottom
    session$sendCustomMessage("scrollHistoryToBottom", list())

    do.call(shiny::tagList, history_elements)
  })

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

    do.call(shiny::tagList, unique(archived_elements))
  })

  observe({
    sel <- selectedLines()
    session$sendCustomMessage("restoreSelectedLines", list(commands = sel))
  })

  observeEvent(input$send_selected_history_json, {
    send_selected_now(TRUE)
  })

  observeEvent(send_selected_now(), {
    req(send_selected_now())
    send_selected_now(FALSE)

    tryCatch({
      hist_key <- input$history_type %||% "r"
      all_lines <- shown_history()[[hist_key]]
      sel_idx <- selectedLines()

      cmds <- if (length(sel_idx) > 0) all_lines[sel_idx] else character()

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
        hidden_idx <- sel_idx

        current_hidden <- hidden_line_indices()
        current_hidden[[hist_key]] <- unique(c(current_hidden[[hist_key]], hidden_idx))
        hidden_line_indices(current_hidden)

        # Clear selected lines visually and reactively
        selectedLines(NULL)
        session$sendCustomMessage("clearSelectedLines", list())

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


  observe({
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"
    all_lines <- shown_history()[[hist_key]]

    selected <- input$selected_lines
    indices <- integer()
    if (!is.null(selected) && length(selected) > 0) {
      # If selected contains content, map to indices
      indices <- which(all_lines %in% selected)
    }
    selectedLines(indices)

    try({
      writeLines(as.character(indices), selected_history_path, useBytes = TRUE)
    }, silent = TRUE)
  })

  observeEvent(input$send_history_json, {
    tryCatch({
      history_type <- input$history_type %||% "r"
      hist_key <- if (history_type == "r") "r" else "term"
      all_history <- shown_history()[[hist_key]]
      visible_indices <- get_visible_indices(hist_key)
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

  observeEvent(input$clear_history_btn, {
    history_type <- input$history_type %||% "r"
    hist_key <- if (history_type == "r") "r" else "term"

    selected_indices <- selectedLines()

    # Mark lines as deleted
    new_deleted <- delete_line_indices()
    new_deleted[[hist_key]] <- unique(c(new_deleted[[hist_key]], selected_indices))
    delete_line_indices(new_deleted)

    # Clear backend selection
    selectedLines(NULL)

    # Notify JS to clear visual selection
    session$sendCustomMessage("clearSelectedLines", list())

    # Remove contents from the .txt file
    try({
      writeLines(character(), selected_history_path)
    }, silent = TRUE)

    shiny::showNotification("Selected lines have been permanently deleted from view.", type = "message", duration = 2)
  })


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

  output$debug_indices <- renderPrint({
  history_type <- input$history_type %||% "r"
  hist_key <- if (history_type == "r") "r" else "term"
  all_lines <- shown_history()[[hist_key]]
  hidden <- hidden_line_indices()[[hist_key]]
  deleted <- delete_line_indices()[[hist_key]]
  visible <- setdiff(seq_along(all_lines), union(hidden, deleted))

  list(
    all_lines_indices = seq_along(all_lines),
    hidden_indices = hidden,
    deleted_indices = deleted,
    visible_indices = visible
  )
})
}
