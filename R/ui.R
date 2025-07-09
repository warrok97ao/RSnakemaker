create_ui <- function(history, term_history, archived_rules, selected_model) {
  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(HTML("
        .history-container {
          max-height: 300px;
          overflow-y: auto;
          border: 1px solid #ddd;
          padding: 10px;
          background-color: #f8f9fa;
          font-family: 'Courier New', monospace;
        }
        .history-line {
          padding: 5px 8px;
          margin: 2px 0;
          cursor: pointer;
          border-radius: 4px;
          transition: background-color 0.2s;
          border: 1px solid transparent;
        }
        .history-line:hover {
          background-color: #e9ecef;
          border-color: #dee2e6;
        }
        .history-line.selected {
          background-color: #007bff;
          color: white;
          border-color: #0056b3;
          font-weight: 500;
        }
        .debug-box {
          background-color: rgba(0,0,0,0.6);
          color: #fff;
          padding: 5px 10px;
          border-radius: 4px;
          font-size: 0.9em;
        }
        .settings-gear-btn {
          position: absolute;
          top: 15px;
          right: 10px;
          background: none;
          border: none;
          font-size: 22px;
          color: #888;
          cursor: pointer;
          z-index: 10;
        }
        .settings-gear-btn:hover {
          color: #007bff;
        }
        .settings-menu {
          display: none;
          position: absolute;
          top: 45px;
          right: 20px;
          background: #fff;
          border: 1px solid #ddd;
          border-radius: 6px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          padding: 18px 22px 16px 22px;
          z-index: 100;
          min-width: 220px;
        }
        .settings-menu.show {
          display: block;
        }
        .settings-menu .settings-title {
          font-size: 1.15em;
          font-weight: 600;
          margin-bottom: 10px;
          color: #333;
        }
        .settings-menu .form-group {
          margin-bottom: 14px;
        }
        .settings-menu label {
          font-weight: 500;
          color: #444;
          margin-bottom: 4px;
          display: block;
        }
        .snkmkr-header-row {
          display: flex;
          align-items: center;
          justify-content: space-between;
          position: relative;
          min-height: 40px;
          margin-bottom: 10px;
        }
        .snkmkr-logo {
          height: 60px;
          width: auto;
          margin-left: 2px;
          margin-top: 4px;
          margin-bottom: 2px;
        }
        .yellow-gradient-btn {
          background: linear-gradient(90deg, #ffe259 0%, #ffa751 100%);
          color: #333;
          border: none;
          border-radius: 5px;
          padding: 6px 18px;
          font-size: 0.95em;
          font-weight: 500;
          margin-top: 12px;
          margin-bottom: 8px;
          box-shadow: 0 1px 4px rgba(255, 215, 0, 0.12);
          transition: background 0.2s, box-shadow 0.2s;
        }
        .yellow-gradient-btn:hover {
          background: linear-gradient(90deg, #ffd200 0%, #ffb347 100%);
          color: #111;
          box-shadow: 0 2px 8px rgba(255, 215, 0, 0.18);
        }
        .red-btn {
          background: #e74c3c;
          color: white;
        }
        .red-btn:hover {
          background: #c0392b;
        }
        .record-btn {
          position: absolute;
          top: 17px;
          right: 62px;
          border: none;
          border-radius: 50%;
          width: 26px;
          height: 26px;
          font-size: 15px;
          display: flex;
          align-items: center;
          justify-content: center;
          cursor: pointer;
          z-index: 11;
          transition: background 0.2s, color 0.2s, box-shadow 0.2s;
          padding: 0;
        }
        .record-btn.recording {
          background: #e74c3c;
          color: #fff;
          box-shadow: 0 2px 8px rgba(231,76,60,0.13);
        }
        .record-btn.not-recording {
          background: #27ae60;
          color: #fff;
          box-shadow: 0 2px 8px rgba(39,174,96,0.13);
        }
        .record-btn:hover {
          filter: brightness(1.08);
        }
        .record-btn .icon-center {
          display: flex;
          align-items: center;
          justify-content: center;
          width: 100%;
          height: 100%;
          line-height: 1;
          font-size: 15px;
        }
        .archive-container {
          margin-top: 18px;
          background: #f3f3f3;
          border: 1px dashed #bbb;
          border-radius: 4px;
        }
        .archive-header {
          padding: 10px 15px;
          cursor: pointer;
          user-select: none;
          display: flex;
          align-items: center;
          gap: 8px;
          background: #e9ecef;
          border-radius: 4px 4px 0 0;
          transition: background-color 0.2s;
        }
        .archive-header:hover {
          background: #dee2e6;
        }
        .archive-arrow {
          transition: transform 0.2s ease;
          font-size: 12px;
          color: #666;
        }
        .archive-arrow.expanded {
          transform: rotate(90deg);
        }
        .archive-content {
          max-height: 0;
          overflow: hidden;
          transition: max-height 0.3s ease;
          padding: 0 10px;
        }
        .archive-content.expanded {
          max-height: 300px;
          padding: 10px;
          overflow-y: auto;
        }
        .archive-title {
          margin: 0;
          color: #888;
          font-weight: 500;
          font-size: 14px;
        }
      ")),
      shiny::tags$script(HTML("
        // Toggle settings menu visibility
        function toggleSettingsMenu() {
          var menu = document.getElementById('settings-menu');
          if (menu.classList.contains('show')) {
            menu.classList.remove('show');
          } else {
            menu.classList.add('show');
          }
        }
        // Hide menu when clicking outside
        document.addEventListener('click', function(e) {
          var menu = document.getElementById('settings-menu');
          var gear = document.getElementById('settings-gear-btn');
          if (!menu || !gear) return;
          if (!menu.contains(e.target) && !gear.contains(e.target)) {
            menu.classList.remove('show');
          }
        });
        // Prevent click inside menu from closing it
        document.addEventListener('DOMContentLoaded', function() {
          var menu = document.getElementById('settings-menu');
          if (menu) {
            menu.addEventListener('click', function(e) {
              e.stopPropagation();
            });
          }
        });

        // Store selected lines as an array of command strings
        window.selectedHistoryLines = window.selectedHistoryLines || [];
        window.selectedArchivedLines = window.selectedArchivedLines || [];

        // Function to toggle selection for multiple lines
        function toggleHistoryLine(index, command, id) {
          var element = document.getElementById(id);
          if (!element) return;
          var isSelected = element.classList.contains('selected');
          var foundIdx = window.selectedHistoryLines.indexOf(command);

          if (isSelected) {
            // Deselect
            element.classList.remove('selected');
            if (foundIdx !== -1) window.selectedHistoryLines.splice(foundIdx, 1);
          } else {
            // Select
            element.classList.add('selected');
            if (foundIdx === -1) window.selectedHistoryLines.push(command);
          }
          // Send all selected lines to Shiny (array of command strings)
          Shiny.setInputValue('selected_lines', window.selectedHistoryLines, {priority: 'event'});
        }

        // Function to toggle selection for archived lines
        function toggleArchivedLine(index, command, id) {
          var element = document.getElementById(id);
          if (!element) return;
          var isSelected = element.classList.contains('selected');
          var foundIdx = window.selectedArchivedLines.indexOf(command);

          if (isSelected) {
            // Deselect
            element.classList.remove('selected');
            if (foundIdx !== -1) window.selectedArchivedLines.splice(foundIdx, 1);
          } else {
            // Select
            element.classList.add('selected');
            if (foundIdx === -1) window.selectedArchivedLines.push(command);
          }
          // Send all selected archived lines to Shiny
          Shiny.setInputValue('selected_archived_lines', window.selectedArchivedLines, {priority: 'event'});
        }

        // Toggle archive visibility
        function toggleArchive() {
          var content = document.getElementById('archive-content');
          var arrow = document.getElementById('archive-arrow');
          if (!content || !arrow) return;

          var isExpanded = content.classList.contains('expanded');
          if (isExpanded) {
            content.classList.remove('expanded');
            arrow.classList.remove('expanded');
          } else {
            content.classList.add('expanded');
            arrow.classList.add('expanded');
          }
        }

        // Recording button icon and tooltip update
        Shiny.addCustomMessageHandler('updateRecordBtn', function(message) {
          var btn = document.getElementById('record-btn');
          if (!btn) return;
          btn.className = 'record-btn ' + (message.recording ? 'recording' : 'not-recording');
          btn.title = message.recording ? 'Stop Recording' : 'Start Recording';
          btn.innerHTML = message.recording
            ? '<span class=\"icon-center\"><svg width=\"13\" height=\"13\" viewBox=\"0 0 13 13\" style=\"display:block;margin:auto;\"><rect x=\"2.5\" y=\"2.5\" width=\"8\" height=\"8\" fill=\"white\"/></svg></span>'
            : '<span class=\"icon-center\"><svg width=\"13\" height=\"13\" viewBox=\"0 0 13 13\" style=\"display:block;margin:auto;\"><polygon points=\"4,2.5 10,6.5 4,10.5\" fill=\"white\"/></svg></span>';
        });
      "))
    ),
    shiny::tags$div(
      class = "snkmkr-header-row",
      shiny::tags$img(src = "logo_resources/logo.png", class = "snkmkr-logo", alt = "Snakemaker"),
      # Recording button
      shiny::tags$button(
        id = "record-btn",
        class = "record-btn recording",
        title = "Stop Recording",
        onclick = "Shiny.setInputValue('toggle_record', Math.random(), {priority: 'event'});",
        shiny::HTML('<span class="icon-center"><svg width="13" height="13" viewBox="0 0 13 13" style="display:block;margin:auto;"><rect x="2.5" y="2.5" width="8" height="8" fill="white"/></svg></span>')
      ),
      shiny::tags$button(
        id = "settings-gear-btn",
        class = "settings-gear-btn",
        title = "Settings",
        onclick = "toggleSettingsMenu();",
        shiny::icon("gear")
      ),
      shiny::tags$div(
        id = "settings-menu",
        class = "settings-menu",
        shiny::tags$div(class = "settings-title", "Settings"),
        shiny::div(class = "form-group",
          shiny::tags$label("Choose a local port:"),
          shiny::selectInput(
            "port_input",
            label = NULL,
            choices = c(
              "7007" = 7007,
              "6512" = 6512,
              "8321" = 8321,
              "6812" = 6812,
              "8924" = 8924,
              "7235" = 7235,
              "8521" = 8521,
              "7442" = 7442,
              "7621" = 7621,
              "7006" = 7006
            ),
            selected = 7007,
            width = "100%"
          )
        ),
        shiny::div(class = "form-group",
          shiny::tags$label("History to display:"),
          shiny::radioButtons(
            "history_type",
            label = NULL,
            choices = c("R History" = "r", "Terminal History" = "term"),
            selected = "r",
            inline = FALSE
          )
        )
      )
    ),
    shiny::div(
      class = "history-container",
      shiny::uiOutput("selectableHistory")
    ),
    # First row: Generate rule buttons
    shiny::div(
      style = "display: flex; justify-content: flex-end; gap: 10px;",
      shiny::actionButton(
        "send_history_json",
        "Generate rule",
        class = "yellow-gradient-btn"
      ),
      shiny::actionButton(
        "send_selected_history_json",
        "Generate rule from selection",
        class = "yellow-gradient-btn"
      )
    ),
    # Second row: Export history button
    shiny::div(
      style = "display: flex; justify-content: flex-end; gap: 10px; margin-top: 4px;",
      shiny::actionButton(
        "clear_history_btn",
        "Clear History",
        class = "yellow-gradient-btn red-btn"
      ),
      shiny::downloadButton(
        "export_history_btn",
        "Export history",
        class = "yellow-gradient-btn"
      )
    ),
    # Archive section - moved below buttons
    shiny::div(
      class = "archive-container",
      shiny::div(
        class = "archive-header",
        onclick = "toggleArchive();",
        shiny::span(id = "archive-arrow", class = "archive-arrow", "▶"),
        shiny::h5(class = "archive-title", "Archived lines")
      ),
      shiny::div(
        id = "archive-content",
        class = "archive-content",
        shiny::uiOutput("archivedHistory")
      )
    ),
    shiny::div(
      style = "position: fixed; bottom: 10px; right: 10px; z-index: 9999;",
      shiny::uiOutput("debug_len_box")
    )
  )
}

# Export create_ui to global environment
assign("create_ui", create_ui, envir = .GlobalEnv)

