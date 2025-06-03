# ── modules/custom_descriptors.R ───────────────────────────────────────────
# This module enables users to rename descriptors dynamically and assign colors.
# It updates both the internal descriptor mapping and the main result data table.

custom_descriptors <- function(input, output, session) {
  
  # ── Step 1: Watch for File Load Event and Ensure `file_type()` Exists ─────
  observeEvent(file_loaded(), {
    
    # Ensure that `file_type()` is defined before proceeding
    req(file_type())
    
    # If a file has been loaded, proceed with descriptor UI and logic
    if (file_loaded()) {
      
      # ── Step 2: Retrieve Descriptor Metadata ───────────────────────────────
      # `descriptors_df_data()` holds a data frame with columns: 
      #   original_attribute, attribute, color
      all_descriptors     <- descriptors_df_data()$attribute
      num_descriptors     <- length(all_descriptors)
      num_panelists_val   <- num_panelists()       # number of panelists
      num_products_val    <- num_of_products()     # number of products
      num_descriptors_val <- num_descriptors       # count of descriptors
      
      # ── Step 3: Generate Dynamic UI Fields for Renaming Descriptors ───────
      output$descriptors_ui <- renderUI({
        # Re-fetch updated names from descriptors_df_data()
        current_names <- descriptors_df_data()$attribute
        
        # Create a grid of text inputs—one per descriptor—to allow renaming.
        tagList(
          div(
            class = "descriptors-grid",
            lapply(seq_along(current_names), function(i) {
              div(
                class = "descriptor-container",
                # Dynamically adjust width based on name length (at least 150px)
                style = paste0("width: ", max(150, nchar(current_names[i]) * 10), "px;"),
                tags$input(
                  id    = paste0("descriptor_", i),
                  type  = "text",
                  value = current_names[i],
                  class = "form-control descriptor-input"
                )
              )
            })
          ),
          # Button to save any name changes
          div(
            class = "save-button-container",
            actionButton("save_descriptors", "Save Changes", class = "save-button")
          )
        )
      })
      
      # Render a table showing old vs. new descriptor names
      output$descriptors_table <- renderTable({
        req(descriptors_df_data())
        descriptors_df_data() %>%
          select(original_attribute, attribute) %>%
          rename(
            "Old Name" = original_attribute,
            "New Name" = attribute
          )
      },
      striped = TRUE, bordered = TRUE, hover = TRUE)
      
      # ── Step 4: Handle "Save Changes" Button Click ──────────────────────────
      observeEvent(input$save_descriptors, {
        req(descriptors_df_data())
        
        # Current descriptor names in the data frame
        old_df <- descriptors_df_data()
        old_names <- old_df$attribute
        
        # If no descriptors exist, show an error modal and exit
        if (length(old_names) == 0) {
          showModal(modalDialog(
            title = div(class = "blinking-error", "ERROR"),
            div(
              class = "message-text",
              tags$i(class = "fa fa-exclamation-circle error-icon"),
              "No descriptors found. Update aborted."
            ),
            easyClose = TRUE,
            footer = tags$button(
              type          = "button",
              class         = "custom-close-btn",
              "Close",
              `data-dismiss` = "modal"
            )
          ))
          return()
        }
        
        # Collect new names entered by the user, or fallback to existing name
        new_names <- sapply(seq_along(old_names), function(i) {
          input_id <- paste0("descriptor_", i)
          if (!is.null(input[[input_id]]) && input[[input_id]] != "") {
            input[[input_id]]
          } else {
            old_names[i]
          }
        })
        
        # Update the reactive descriptors_df_data() with new names (preserving color)
        descriptors_df_data(
          data.frame(
            original_attribute = old_df$original_attribute,
            attribute          = new_names,
            color              = old_df$color,
            stringsAsFactors   = FALSE
          )
        )
        
        # Show a success modal to confirm update
        showModal(modalDialog(
          title = div(class = "blinking-success", "SUCCESS"),
          div(
            class = "message-text",
            tags$i(class = "fa fa-check-circle success-icon"),
            "Descriptors updated successfully!"
          ),
          easyClose = TRUE,
          footer = tags$button(
            type          = "button",
            class         = "custom-close-btn",
            "Close",
            `data-dismiss` = "modal"
          )
        ))
        
        # Also update the `new_name_attribute` column in the main result table
        req(result())
        old_result <- result()
        
        # If the column doesn't exist yet, create it
        if (!"new_name_attribute" %in% names(old_result)) {
          old_result$new_name_attribute <- NA_character_
        }
        
        # Match by original_attribute to replace with the new attribute name
        df_des <- descriptors_df_data()
        idx <- match(old_result$attribute, df_des$original_attribute)
        old_result$new_name_attribute <- df_des$attribute[idx]
        
        # Push the updated data frame back into the reactiveVal `result()`
        result(old_result)
      })
      
      # ── Step 5: Render Instruction Text for Descriptor Re-naming ───────────
      output$instructionText <- renderUI({
        if (file_loaded()) {
          div(
            class = "file-info-text",
            style = "text-align: left; margin-top: 10px;",
            HTML(
              "<em>Please first <strong>select a descriptor</strong>, then <strong>choose a color</strong> and
              <strong>save the association</strong>.</em>"
            )
          )
        }
      })
      
      # ── Step 6: Generate Color Picker UI for Each Descriptor ────────────────
      output$descriptor_color_ui <- renderUI({
        req(descriptors_df_data())
        df <- descriptors_df_data()
        
        tagList(
          div(
            style = "display: flex; flex-wrap: wrap; gap: 15px; justify-content: flex-start;",
            lapply(seq_len(nrow(df)), function(i) {
              desc_name  <- df$attribute[i]
              desc_color <- df$color[i]
              
              div(
                style = "width: 200px;",
                colourpicker::colourInput(
                  inputId          = paste0("desc_color_", i),
                  label            = desc_name,
                  value            = ifelse(is.na(desc_color), "#FFFFFF", desc_color),
                  showColour       = "both",
                  allowTransparent = TRUE
                )
              )
            })
          ),
          tags$br(),
          # Button to save color associations
          div(
            style = "display: flex; justify-content: center; margin-top: 15px;",
            actionButton("save_color_assoc", "Save Color Associations", class = "save-button")
          )
        )
      })
      
      # ── Step 7: Handle "Save Color Associations" Button Click ───────────────
      observeEvent(input$save_color_assoc, {
        req(descriptors_df_data())
        
        old_df <- descriptors_df_data()
        # Collect chosen colors for each descriptor
        new_colors <- sapply(seq_len(nrow(old_df)), function(i) {
          input[[paste0("desc_color_", i)]]
        })
        
        # Update the descriptors_df_data reactive with new colors
        old_df$color <- new_colors
        descriptors_df_data(old_df)
        
        # Also update the `descriptor_color` column in the main result table
        req(result())
        res_df <- result()
        
        if (!"descriptor_color" %in% colnames(res_df)) {
          res_df$descriptor_color <- NA_character_
        }
        
        df_des <- descriptors_df_data()
        idx <- match(res_df$attribute, df_des$original_attribute)
        res_df$descriptor_color <- df_des$color[idx]
        
        result(res_df)
        
        # Show a success modal to confirm color update
        showModal(modalDialog(
          title = div(class = "blinking-success", "SUCCESS"),
          div(
            class = "message-text",
            tags$i(class = "fa fa-check-circle success-icon"),
            "Colors updated successfully!"
          ),
          easyClose = TRUE,
          footer = tags$button(
            type          = "button",
            class         = "custom-close-btn",
            "Close",
            `data-dismiss` = "modal"
          )
        ))
      })
    }  # End of if (file_loaded())
  })  # End of observeEvent(file_loaded())
  
  # ── Step 8: Render File Information Text (outside the file_loaded observer) ─
  output$file_info_text <- renderUI({
    # Require that file is loaded and all counts are available
    req(
      file_loaded(),
      file_type(),
      num_panelists(),
      num_of_products(),
      num_descriptors()
    )
    
    np  <- num_panelists()
    npd <- num_of_products()
    nd  <- num_descriptors()
    
    # Determine file label (Qualtrics or Fizz)
    file_label <- if (file_type() == "qualtrics") "Qualtrics" else "Fizz"
    
    div(
      class = "file-info-text",
      style = "margin-bottom:10px;",
      # Main sentence summarizing loaded file
      HTML(sprintf(
        "You have loaded a <strong>%s</strong> file in which <strong>%d</strong> panelists have evaluated <strong>%d</strong> products using <strong>%d</strong> descriptors. You can change the descriptor names below.",
        file_label, np, npd, nd
      )),
      tags$br(),
      # Reminder in bold red to save changes
      tags$strong(
        style = "color:#CC0000;",
        "Don't forget to save your changes."
      )
    )
  })
}
