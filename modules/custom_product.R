# ── modules/custom_product.R ────────────────────────────────────────────────
# This module allows users to rename products, upload product images, and 
# update them dynamically. It updates both the product mapping data frame and 
# the main result table.

custom_product <- function(input, output, session) {
  
  # ── Step 1: Watch for file_loaded() event and ensure file_type() is defined ─
  observeEvent(file_loaded(), {
    req(file_type())  # Ensure file_type() exists before proceeding
    
    if (file_loaded()) {
      # ------------------------------------------------------------------------------------------------
      # Step 2: Generate dynamic UI inputs for renaming products
      # ------------------------------------------------------------------------------------------------
      output$productRenameUI <- renderUI({
        pd <- product_df_data()  # Data frame with columns: product_code, product_name
        
        # If the file came from Fizz, sort product codes by their numeric suffix
        if (file_type() == "fizz") {
          suffixes <- as.numeric(gsub("[^0-9]", "", pd$product_code))
          pd <- pd[order(suffixes), ]
        }
        
        # Create a grid of textInput fields—one per product—so users can rename
        tagList(
          div(
            class = "descriptors-grid",
            style = "display: flex; flex-wrap: wrap; gap: 10px;",
            lapply(seq_len(nrow(pd)), function(i) {
              code <- pd$product_code[i]
              div(
                class = "descriptor-container",
                style = "width: 200px;",
                textInput(
                  inputId = paste0("product_name_", code),
                  label   = code,
                  value   = pd$product_name[i],  # Pre-fill with current name
                  width   = "100%"
                )
              )
            })
          ),
          # Centered save button below the grid
          div(
            style = "text-align: center; margin-top: 15px;",
            actionButton(
              "save_products",
              "Save Changes",
              class = "save-button",
              style = "min-width: 150px;"
            )
          )
        )
      })
      
      # ------------------------------------------------------------------------------------------------
      # Step 3: Handle “Save Changes” button click to update product names
      # ------------------------------------------------------------------------------------------------
      observeEvent(input$save_products, {
        pd <- product_df_data()  # Current product mapping
        
        # 1) Collect new names entered by the user, defaulting to code if left blank
        new_names <- vapply(
          pd$product_code,
          function(code) {
            nm <- input[[paste0("product_name_", code)]]
            if (is.null(nm) || nm == "") {
              code
            } else {
              nm
            }
          },
          FUN.VALUE = character(1)
        )
        
        # 2) Update the reactive product_df_data() with these new names
        pd$product_name <- new_names
        product_df_data(pd)
        showNotification("Product names updated successfully!", type = "message")
        
        # 3) Update result()$new_name_product in the main result table
        req(result())
        res <- result()
        
        # Initialize column if not present
        if (!"new_name_product" %in% names(res)) {
          res$new_name_product <- res$product
        }
        
        # Choose the correct key for matching based on file_type()
        if (file_type() == "qualtrics" && "old_product" %in% names(res)) {
          keys <- res$old_product
        } else {
          keys <- res$product
        }
        
        # Match and assign new names
        idx <- match(keys, pd$product_code)
        res$new_name_product <- pd$product_name[idx]
        
        # Save updated result table
        result(res)
        
        # Debug prints (uncomment for debugging)
        # cat("\n--- DEBUG: product_df_data() ---\n")
        # print(head(pd))
        # cat("--- DEBUG: result() ---\n")
        # print(head(res))
        # cat("-------------------------------\n\n")
      })
      
      # ------------------------------------------------------------------------------------------------
      # Step 4: Render instruction texts for product naming and image upload
      # ------------------------------------------------------------------------------------------------
      output$file_info_text2 <- renderUI({
        if (file_loaded()) {
          div(
            class = "file-info-text",
            HTML("<em>Please associate a <strong>name</strong> to each <strong>product</strong>.</em>"),
            style = "text-align: left; margin-top: 22px;"
          )
        }
      })
      
      output$file_info_text3 <- renderUI({
        if (file_loaded()) {
          div(
            class = "file-info-text",
            HTML("<em>Please upload an <strong>image</strong> for each <strong>product</strong>. Accepted formats are JPG and PNG.</em>"),
            style = "text-align: left; margin-top: 22px;"
          )
        }
      })
      
      # ------------------------------------------------------------------------------------------------
      # Step 5: Generate dynamic UI for product image uploads
      # ------------------------------------------------------------------------------------------------
      output$productImageUploadUI <- renderUI({
        req(product_df_data())  # Ensure product mapping is available
        product_data <- product_df_data()
        
        # If the file is from Fizz, sort products by numeric suffix if possible
        if (file_type() == "fizz") {
          numeric_suffix <- as.numeric(gsub("[^0-9]", "", product_data$product_code))
          if (all(!is.na(numeric_suffix))) {
            sorted_indices <- order(numeric_suffix)
            product_data <- product_data[sorted_indices, ]
          } else {
            showNotification(
              "Warning: Some product codes do not follow 'produit_X' format. Ordering may be incorrect.",
              type = "warning"
            )
          }
        }
        
        # Create a flex grid of image upload cells—one per product
        tagList(
          div(
            class = "image-upload-grid",
            lapply(seq_len(nrow(product_data)), function(i) {
              product_name <- ifelse(
                product_data$product_name[i] != "",
                product_data$product_name[i],
                product_data$product_code[i]
              )
              image_input <- input[[paste0("product_image_", i)]]
              
              # If user already uploaded an image, convert it to base64 for display
              if (!is.null(image_input)) {
                image_path <- image_input$datapath
                base64 <- base64enc::dataURI(file = image_path, mime = image_input$type)
              } else {
                base64 <- NULL
              }
              
              div(
                class = "image-upload-cell",
                style = "width: 200px; height: 300px; background-color: white; 
                         border-radius: 15px; padding: 10px; 
                         box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); 
                         display: flex; flex-direction: column; 
                         align-items: center; justify-content: space-between;",
                
                # Display product name at the top of the cell
                div(
                  class = "product-name",
                  style = "font-weight: bold; font-size: 16px; text-align: center; margin-top: 10px;",
                  product_name
                ),
                
                # If a base64 image exists, render it; otherwise, show a placeholder
                if (!is.null(base64)) {
                  tags$img(
                    src    = base64,
                    class  = "product-image",
                    style  = "width: 150px; height: 150px; object-fit: cover; border-radius: 10px;"
                  )
                } else {
                  div(
                    class = "image-placeholder",
                    style = "width: 150px; height: 150px; display: flex; 
                             align-items: center; justify-content: center; 
                             background-color: #f0f0f0; border-radius: 10px;",
                    tags$i(class = "fas fa-image"),
                    "No image"
                  )
                },
                
                # File input button at the bottom of the cell to upload a new image
                div(
                  class = "file-input-container",
                  style = "margin-bottom: 10px;",
                  fileInput(
                    inputId     = paste0("product_image_", i),
                    label       = NULL,
                    accept      = c("image/png", "image/jpeg"),
                    buttonLabel = "Upload Image",
                    placeholder = ""
                  )
                )
              )
            })
          ),
          # Centered save button to finalize image associations
          div(
            class = "save-button-container",
            actionButton("save_product_images", "Save Images", class = "save-button")
          )
        )
      })
      
      # ------------------------------------------------------------------------------------------------
      # Step 6: Handle “Save Images” button click to store uploaded file paths
      # ------------------------------------------------------------------------------------------------
      observeEvent(input$save_product_images, {
        req(product_df_data())
        pd <- product_df_data()
        
        # Loop through each product index, check if an image was uploaded
        for (i in seq_len(nrow(pd))) {
          file <- input[[paste0("product_image_", i)]]
          if (!is.null(file)) {
            # Store the datapath and MIME type in a reactive list `product_images`
            product_images[[pd$product_code[i]]] <- list(
              datapath = file$datapath,
              mime     = file$type
            )
          }
        }
        showNotification("Product images saved successfully!", type = "message")
      })
      
      # ------------------------------------------------------------------------------------------------
      # (Optional) Debug observer to print display names for verification
      # ------------------------------------------------------------------------------------------------
      # observe({
      #   req(file_loaded(), product_df_data(), file_type())
      #   dfp <- product_df_data()
      #   display_names <- vapply(seq_len(nrow(dfp)), function(i) {
      #     code <- dfp$product_code[i]
      #     user <- dfp$product_name[i]
      #     if (file_type() == "fizz") {
      #       if (!is.na(user) && nzchar(user)) user else code
      #     } else {
      #       if (!is.na(user) && nzchar(user)) user else paste0("produit_", code)
      #     }
      #   }, FUN.VALUE = "")
      #   cat("→ Display names:", paste(display_names, collapse = " | "), "\n")
      # })
    }
  })
}
