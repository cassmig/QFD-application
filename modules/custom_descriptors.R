# 📌 custom_descriptors.R - Handles descriptor customization
# This module allows users to modify descriptor names and update them dynamically.

custom_descriptors <- function(input, output, session) {
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 1: Observe File Loading and Ensure `file_type()` is Defined
  # --------------------------------------------------------------------------------------------------------
  
  # Observe file loading event to dynamically update UI elements
  observeEvent(file_loaded(), {
    
    req(file_type())  # Ensure `file_type()` is properly defined before proceeding
    
    # If a file has been loaded, proceed with descriptor processing
    if (file_loaded()) {
      
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Confirm File Type Detection
          # --------------------------------------------------------------------------------------------------------
          
          ### Print the detected file type to ensure correct file processing
          # print(paste("✅ DEBUG: File Loaded Successfully, Detected file_type =", file_type()))
      
      
      
       
      # --------------------------------------------------------------------------------------------------------
      # 📌 Step 2: Retrieve Descriptor Data and Metadata
      # --------------------------------------------------------------------------------------------------------
      
      #  Retrieve descriptors and related metadata
      descriptors <- descriptors_df_data()$attribute  # Extract descriptor names
      num_descriptors <- length(descriptors)  # Count total descriptors
      num_panelists_val <- num_panelists()  # Get number of panelists
      num_of_products_val <- num_of_products()  # Get number of products
      num_descriptors_val <- num_descriptors  # Store descriptor count
      
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Confirm Descriptor Metadata Retrieval
          # --------------------------------------------------------------------------------------------------------
          
          ### Print extracted metadata for validation
          # print("✅ DEBUG: Retrieved descriptor metadata successfully")
          # print(paste(" Number of descriptors:", num_descriptors_val))
          # print(paste(" Number of panelists:", num_panelists_val))
          # print(paste(" Number of products:", num_of_products_val))
      
      
      
      # --------------------------------------------------------------------------------------------------------
      # 📌 Step 3: Define Informational Text Based on File Type
      # --------------------------------------------------------------------------------------------------------
      

      
          
      # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Confirm File Information Text Assignment
          # --------------------------------------------------------------------------------------------------------
          
          ### Print confirmation message for debugging
          # print("✅ DEBUG: File information text assigned successfully.")
          
      
      
      
      # --------------------------------------------------------------------------------------------------------
      # 📌 Step 4: Generate Dynamic Text Input Fields for Descriptors
      # --------------------------------------------------------------------------------------------------------
      
      output$descriptors_ui <- renderUI({
        
        # Retrieve descriptor names while ensuring no duplicate "descriptor_" prefix
        descriptors <- descriptors_df_data()$attribute  # Directly using updated descriptor names
        
        # Create UI elements dynamically
        tagList(
          div(class = "descriptors-grid",  # Grid container for text inputs
              lapply(seq_along(descriptors), function(i) {
                div(
                  class = "descriptor-container",
                  style = paste0("width: ", max(150, nchar(descriptors[i]) * 10), "px;"),  # Adjust width dynamically
                  tags$input(id = paste0("descriptor_", i),
                             type = "text",
                             value = descriptors[i],  # Ensure correct names are pre-filled
                             class = "form-control descriptor-input")
                )
              })
          ),
          
          # Save button for descriptor changes
          div(class = "save-button-container",
              actionButton("save_descriptors", "Save Changes", class = "save-button"))
        )
      })      

      # Render a Table Displaying Updated Descriptor Names
      output$descriptors_table <- renderTable({
        
        req(descriptors_df_data())  # Ensure descriptor data exists
        
        descriptors_df_data() %>% 
          select(original_attribute, attribute) %>%  #  Keep only original and updated names
          rename("Old Name" = original_attribute, "New Name" = attribute)  # Rename columns for display
        
      }, striped = TRUE, bordered = TRUE, hover = TRUE)  # Table styling for better readability
      
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Confirm UI Generation
          # --------------------------------------------------------------------------------------------------------
          
          ### Print confirmation messages for debugging
          # print("✅ DEBUG: UI for descriptors generated successfully")
      
      
      
      
      # --------------------------------------------------------------------------------------------------------
      # 📌 Step 5: Observe "Save Changes" Button Click for Descriptors
      # --------------------------------------------------------------------------------------------------------
      
      observeEvent(input$save_descriptors, {
        
        # Check and retrieve the current state of `descriptors_df_data()`
        req(descriptors_df_data())  # Ensure it's not null
        
        # Gather the current descriptor names (from the `attribute` column)
        descriptors <- descriptors_df_data()$attribute
        
        # If no descriptors found, show an ERROR modal and stop
        if (length(descriptors) == 0) {
          showModal(modalDialog(
            title = div(class = "blinking-error", "ERROR"),
            div(
              class = "message-text",
              tags$i(class = "fa fa-exclamation-circle error-icon"),
              "No descriptors found. Update aborted."
            ),
            easyClose = TRUE,
            footer = tags$button(
              type = "button",
              class = "custom-close-btn",
              "Close",
              `data-dismiss` = "modal"
            )
          ))
          return()
        }
        
        # Retrieve new descriptor names input by the user.
        # For each descriptor, we look up the input "descriptor_i".
        new_descriptors <- sapply(seq_along(descriptors), function(i) {
          input_id <- paste0("descriptor_", i)  # e.g. descriptor_1, descriptor_2, ...
          if (!is.null(input[[input_id]]) && input[[input_id]] != "") {
            return(input[[input_id]])  # the user typed something
          } else {
            return(descriptors[i])      # otherwise, keep the existing name
          }
        })
        
        # Update `descriptors_df_data()` with these new names
        descriptors_df_data(
          data.frame(
            original_attribute = descriptors_df_data()$original_attribute,
            attribute = new_descriptors,  # the updated list of names
            color = descriptors_df_data()$color
          )
        )
        
        # Show a SUCCESS modal to confirm descriptors have been updated
        showModal(modalDialog(
          title = div(class = "blinking-success", "SUCCESS"),
          div(
            class = "message-text",
            tags$i(class = "fa fa-check-circle success-icon"),
            "Descriptors updated successfully!"
          ),
          easyClose = TRUE,
          footer = tags$button(
            type = "button",
            class = "custom-close-btn",
            "Close",
            `data-dismiss` = "modal"
          )
        ))
        
        # Update the `new_name_attribute` column in `result()`
        req(result())              # Ensure `result()` is not null
        old_res <- result()        # Retrieve the current table
        
        # 1) If `new_name_attribute` does not yet exist, create it as NA
        if (!"new_name_attribute" %in% names(old_res)) {
          old_res$new_name_attribute <- NA_character_
        }
        
        # 2) Match old_res$attribute (the internal key in `result()`)
        #    with descriptors_df_data()$original_attribute (the key in `descriptors_df_data()`)
        df_des <- descriptors_df_data()
        
        idx <- match(old_res$attribute, df_des$original_attribute)
        
        # 3) Replace the value of `new_name_attribute` with the new descriptor name
        old_res$new_name_attribute <- df_des$attribute[idx]
        
        # 4) Re-inject this updated table back into the reactiveVal `result()`
        result(old_res)
        
        # --------------------------------------------------------------------------------------------------------
        # 🛠 Debugging:
        # --------------------------------------------------------------------------------------------------------
        ### Display debug information in the console
        # cat("\n-----------------------------------------------------\n")
        # cat("✅ DEBUG: Descriptors_df_data() after update:\n")
        # print(head(descriptors_df_data()))
        # 
        # cat("\n✅ DEBUG: result() after updating new_name_attribute:\n")
        # print(head(result()))
        # cat("-----------------------------------------------------\n\n")
      })
      
      
      
      # --------------------------------------------------------------------------------------------------------#
      ### 📌 Step 6: Define UI Instructions Based on File Type
      # --------------------------------------------------------------------------------------------------------#


      # Render the instruction text for users
      output$instructionText <- renderUI({
        # Only display instructions if a file has been successfully loaded
        if (file_loaded()) {
          div(
            class = "file-info-text",  # Assign CSS class for styling
            HTML("<em>Please first <strong>select a descriptor</strong>, then <strong>choose a color</strong> and
      <strong>save the association</strong>.</em>"),  # Provide instructions for descriptor-color association
            style = "text-align: left; margin-top: 10px;"  # Align text and add spacing for better UI
          )
        }
      })

          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging:message to confirm successful execution of Step 6
          # --------------------------------------------------------------------------------------------------------
          # print("✅ STEP 6: File information and instructions assigned successfully")
      
      
      
      

      # --------------------------------------------------------------------------------------------------------#
      ### 📌 Step 7: Color picker per descriptor
      # --------------------------------------------------------------------------------------------------------#

      output$descriptor_color_ui <- renderUI({
        req(descriptors_df_data())
        descriptors <- descriptors_df_data()
        
        tagList(
          div(
            style = "display: flex; flex-wrap: wrap; gap: 15px; justify-content: flex-start;",
            lapply(seq_len(nrow(descriptors)), function(i) {
              desc_name <- descriptors$attribute[i]
              desc_color <- descriptors$color[i]
            
              colourpicker::colourInput(
                inputId = paste0("desc_color_", i),
                label = desc_name,
                value = ifelse(is.na(desc_color), "#FFFFFF", desc_color),
                showColour = "both",
                allowTransparent = TRUE,
                width = "200px"
              )
            })
          ),
          tags$br(),
          div(
            style = "display: flex; justify-content: center; margin-top: 15px;",
            actionButton("save_color_assoc", " Save Color Associations", class = "save-button")
          )
          )
      })
      

      # --------------------------------------------------------------------------------------------------------#
      ### 📌 Step 8: Observe "Save Descriptor Colors" Button
      # --------------------------------------------------------------------------------------------------------#
      observeEvent(input$save_color_assoc, {
        req(descriptors_df_data())
        old_desc_data <- descriptors_df_data()
        
        # Loop through all descriptors and read the color chosen
        new_colors <- sapply(seq_len(nrow(old_desc_data)), function(i) {
          input[[paste0("desc_color_", i)]]
        })
        
        # Update descriptors_df_data()
        old_desc_data$color <- new_colors
        descriptors_df_data(old_desc_data)
        
        # Also update the color in result(), in a column `descriptor_color`
        req(result())
        old_res <- result()
        
        # If `descriptor_color` doesn't exist yet, create it
        if (!"descriptor_color" %in% colnames(old_res)) {
          old_res$descriptor_color <- NA_character_
        }
        
        # Match old_res$attribute to the original_attribute in descriptors_df_data()
        df_des <- descriptors_df_data()
        idx <- match(old_res$attribute, df_des$original_attribute)
        
        old_res$descriptor_color <- df_des$color[idx]
        result(old_res)
        
        showModal(modalDialog(
          title = div(class = "blinking-success", "SUCCESS"),
          div(
            class = "message-text",
            tags$i(class = "fa fa-check-circle success-icon"),
            "Colors updated successfully!"
          ),
          easyClose = TRUE,
          footer = tags$button(
            type = "button",
            class = "custom-close-btn",
            "Close",
            `data-dismiss` = "modal"
          )
        ))
      })
      
    }  # End if(file_loaded())
  })  # End observeEvent(file_loaded())
  
  # ← ici, en-dehors de l’observeEvent(file_loaded(), …)
  output$file_info_text <- renderUI({
    req(file_loaded(), file_type(), num_panelists(), num_of_products(), num_descriptors())
    np  <- num_panelists()
    npd <- num_of_products()
    nd  <- num_descriptors()
    
    # Choisir le label du type de fichier
    file_label <- if (file_type() == "qualtrics") "Qualtrics" else "Fizz"
    
    div(
      class = "file-info-text",
      style = "margin-bottom:10px;",
      
      # 1) Phrase principale (no style particulier, tu peux ajuster si besoin)
      HTML(sprintf(
        "You have loaded a <strong>%s</strong> file in which <strong>%d</strong> panelists have evaluated <strong>%d</strong> products using <strong>%d</strong> descriptors. You can change the descriptor names below.",
        file_label, np, npd, nd
      )),
      
      # 2) Saut de ligne
      tags$br(),
      
      # 3) Rappel en gras et rouge
      tags$strong(
        style = "color:#CC0000;",
        "Don't forget to save your changes."
      )
    )
  })
  
}