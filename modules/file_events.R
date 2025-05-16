# 📌 file_events.R - Handles file upload, validation, and processing
# This function manages:
# - File selection and validation
# - Checking if the file matches the selected source (Fizz or Qualtrics)
# - Processing and transforming the data for further analysis

file_events <- function(input, output, session) {
  
  # Observe when a new file is uploaded
  observeEvent(input$file, {
    file_name(input$file$name)  # Update the reactive value with the uploaded file's name
  })
  
  # Observe when the source (Fizz or Qualtrics) is selected
  observeEvent(input$source, {
    source_name(input$source)  # Update the reactive value with the selected source
  })
  
  # Display the uploaded file name
  output$file_name <- renderText({
    if (is.null(file_name())) {
      "No file selected"
    } else {
      file_name()
    }
  })
  
  # Observe when the "Load Data" button is clicked
  observeEvent(input$load_button, {
    
    if (is.null(file_name())) {
      # Show an error message if no file is selected
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
      
      # If Qualtrics is selected
      if (input$source == "Qualtrics") {
        file_path <- input$file$datapath  
        data <- readxl::read_excel(file_path)  
        
        # Validate the file format
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
        
        # If Fizz is selected
      } else if (input$source == "Fizz") {
        file_path <- input$file$datapath  
        data <- readxl::read_excel(file_path)  

        # Validate the file format
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
