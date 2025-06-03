# ── modules/file_events.R ──────────────────────────────────────────────────
# This module manages file selection, validation against the chosen source 
# (Fizz or Qualtrics), and hands off processing to the appropriate helper.

file_events <- function(input, output, session) {
  
  # ── 1) Observe file input and update reactive file_name() ────────────────
  observeEvent(input$file, {
    file_name(input$file$name)
  })
  
  # ── 2) Observe source selector (Fizz vs. Qualtrics) ──────────────────────
  observeEvent(input$source, {
    source_name(input$source)
  })
  
  # ── 3) Render the uploaded file name (or placeholder) ────────────────────
  output$file_name <- renderText({
    if (is.null(file_name())) {
      "No file selected"
    } else {
      file_name()
    }
  })
  
  # ── 4) Handle "Load Data" button click ──────────────────────────────────
  observeEvent(input$load_button, {
    
    # If no file has been chosen, show an error modal
    if (is.null(file_name())) {
      showModal(modalDialog(
        title = div(class = "blinking-error", "ERROR"),
        div(class = "message-text",
            tags$i(class = "fa fa-exclamation-circle error-icon"),
            "No file selected. Please upload a file before loading data."),
        easyClose = TRUE,
        footer = tags$button(
          type = "button",
          class = "custom-close-btn",
          "Close",
          `data-dismiss` = "modal"
        )
      ))
      
    } else {
      # Determine which source was chosen
      if (input$source == "Qualtrics") {
        file_path <- input$file$datapath
        data <- readxl::read_excel(file_path)
        
        # Validate that the first column is "StartDate" (Qualtrics format)
        if (colnames(data)[1] != "StartDate") {
          showModal(modalDialog(
            title = div(class = "blinking-error", "ERROR"),
            div(class = "message-text",
                tags$i(class = "fa fa-exclamation-circle error-icon"),
                "The file format or source is incorrect."),
            easyClose = TRUE,
            footer = tags$button(
              type = "button",
              class = "custom-close-btn",
              "Close",
              `data-dismiss` = "modal"
            )
          ))
        } else {
          # Show success and hand off to Qualtrics processing
          showModal(modalDialog(
            title = div(class = "blinking-success", "SUCCESS"),
            div(class = "message-text",
                tags$i(class = "fa fa-check-circle success-icon"),
                "The Qualtrics file has been successfully loaded."),
            easyClose = TRUE,
            footer = tags$button(
              type = "button",
              class = "custom-close-btn",
              "Close",
              `data-dismiss` = "modal"
            )
          ))
          
          file_type("qualtrics")
          source("modules/process_qualtrics.R")
          process_qualtrics_data(data)
        }
        
      } else if (input$source == "Fizz") {
        file_path <- input$file$datapath
        data <- readxl::read_excel(file_path)
        
        # Validate that the first column is "Identification" (Fizz format)
        if (colnames(data)[1] != "Identification") {
          showModal(modalDialog(
            title = div(class = "blinking-error", "ERROR"),
            div(class = "message-text",
                tags$i(class = "fa fa-exclamation-circle error-icon"),
                "The file format or source is incorrect."),
            easyClose = TRUE,
            footer = tags$button(
              type = "button",
              class = "custom-close-btn",
              "Close",
              `data-dismiss` = "modal"
            )
          ))
        } else {
          # Show success and hand off to Fizz processing
          showModal(modalDialog(
            title = div(class = "blinking-success", "SUCCESS"),
            div(class = "message-text",
                tags$i(class = "fa fa-check-circle success-icon"),
                "The Fizz file has been successfully loaded."),
            easyClose = TRUE,
            footer = tags$button(
              type = "button",
              class = "custom-close-btn",
              "Close",
              `data-dismiss` = "modal"
            )
          ))
          
          file_type("fizz")
          source("modules/process_fizz.R")
          process_fizz_data(data)
        }
        
      } else {
        # If neither Qualtrics nor Fizz is selected, show a generic error
        showModal(modalDialog(
          title = div(class = "blinking-error", "ERROR"),
          div(class = "message-text",
              tags$i(class = "fa fa-exclamation-circle error-icon"),
              "Unknown file format. Please check your file."),
          easyClose = TRUE,
          footer = tags$button(
            type = "button",
            class = "custom-close-btn",
            "Close",
            `data-dismiss` = "modal"
          )
        ))
      }
    }
  })
}
