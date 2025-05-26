# 📌 custom_product.R - Handles product customization
# This module allows users to modify product names, upload product images, and update them dynamically.

custom_product <- function(input, output, session) {
  
  # --------------------------------------------------------------------------------------------------------# 
  ### 📌 Step 1: Observe File Loading and Ensure `file_type()` is Defined
  # --------------------------------------------------------------------------------------------------------#
  
  observeEvent(file_loaded(), {
    
    req(file_type())  # Ensure file_type() is defined before proceeding
    
    if (file_loaded()) {
      # print(paste("✅ STEP 1: File Loaded, file_type =", file_type()))
      
      # --------------------------------------------------------------------------------------------------------# 
      ### 📌 Step 2: Generate Dynamic Text Input Fields for Product Renaming
      # --------------------------------------------------------------------------------------------------------#
      
      
      output$productRenameUI <- renderUI({
        pd <- product_df_data()
        # If Fizz, sort by numeric suffix
        if (file_type() == "fizz") {
          nums <- as.numeric(gsub("[^0-9]", "", pd$product_code))
          pd <- pd[order(nums), ]
        }
        
        # Fixed cell width and centered save button
        tagList(
          div(class = "descriptors-grid",
              style = "display: flex; flex-wrap: wrap; gap: 10px; ",
              lapply(seq_len(nrow(pd)), function(i) {
                code <- pd$product_code[i]
                div(
                  class = "descriptor-container",
                  style = "width: 200px; ",
                  textInput(
                    inputId = paste0("product_name_", code),
                    label   = code,
                    value   = pd$product_name[i],
                    width   = "100%"
                  )
                )
              })
          ),
          div(
            style = "text-align: center; margin-top: 15px;",
            actionButton("save_products", "Save Changes", class = "save-button", style = "min-width: 150px;")
          )
        )
      })   
      # print("✅ STEP 2.1: UI for product renaming generated successfully")
      
  
      # --------------------------------------------------------------------------------------------------------# 
      ### 📌 Step 3: Observe "Save Changes" Button Click for Product Names
      # --------------------------------------------------------------------------------------------------------#
      
      observeEvent(input$save_products, {
        product_data <- product_df_data()
        
        pd <- product_df_data()
        # 1) Récupère les saisies
        new_names <- vapply(pd$product_code, function(code) {
          nm <- input[[paste0("product_name_", code)]]
          if (is.null(nm) || nm == "") code else nm
        }, character(1))
        
        # 2) Met à jour product_df_data()
        pd$product_name <- new_names
        product_df_data(pd)
        showNotification("Product names updated successfully!", type = "message")
        
        # 3) Met à jour result()$new_name_product
        req(result())
        res <- result()
        # Initialisation si nécessaire
        if (!"new_name_product" %in% names(res)) {
          res$new_name_product <- res$product
        }
        
        # Choix de la clé de correspondance
        if (file_type() == "qualtrics" && "old_product" %in% names(res)) {
          keys <- res$old_product
        } else {
          keys <- res$product
        }
        # Match et assignation
        idx <- match(keys, pd$product_code)
        res$new_name_product <- pd$product_name[idx]
        
        # 4) Sauvegarde
        result(res)
        
        # Debug
        cat("\n--- DEBUG: product_df_data() ---\n")
        print(head(pd))
        cat("--- DEBUG: result() ---\n")
        print(head(res))
        cat("-------------------------------\n\n")
      })
      
      
      
      
      # --------------------------------------------------------------------------------------------------------# 
      ### 📌 Step 4: Display UI Instructions for Product Naming and Image Upload
      # --------------------------------------------------------------------------------------------------------#
      
      output$file_info_text2 <- renderUI({
        if (file_loaded()) {
          div(class = "file-info-text",
              HTML("<em>Please associate a <strong>name</strong> to each <strong>product</strong>.</em>"),
              style = "text-align: left; margin-top: 22px;")
        }
      })
      
      output$file_info_text3 <- renderUI({
        if (file_loaded()) {
          div(class = "file-info-text",
              HTML("<em>Please upload an <strong>image</strong> for each <strong>product</strong>. Accepted formats are JPG and PNG.</em>"),
              style = "text-align: left; margin-top: 22px;")
        }
      })
      
      # print("✅ STEP 4: UI Instructions assigned successfully")
      
      # --------------------------------------------------------------------------------------------------------# 
      ### 📌 Step 5: Generate Dynamic UI for Product Image Upload
      # --------------------------------------------------------------------------------------------------------#
      
      output$productImageUploadUI <- renderUI({
        req(product_df_data())  # Ensure product data is available
        product_data <- product_df_data()
        
        # If the file type is "Fizz", sort products based on their numerical suffix
        if (file_type() == "fizz") {
          numeric_suffix <- as.numeric(gsub("[^0-9]", "", product_data$product_code))
          
          if (all(!is.na(numeric_suffix))) {  # Ensure all suffixes are numeric before sorting
            sorted_indices <- order(numeric_suffix)
            product_data <- product_data[sorted_indices, ]
          } else {
            showNotification("Warning: Some product names do not follow the expected format 'produit_X'. Order may be incorrect.", type = "warning")
          }
        }
        
        # print("✅ STEP 5: Generating UI for product image uploads")
        
        tagList(
          div(class = "image-upload-grid",
              lapply(1:nrow(product_data), function(i) {
                product_name <- ifelse(product_data$product_name[i] != "", product_data$product_name[i], product_data$product_code[i])
                image_input <- input[[paste0("product_image_", i)]]
                
                if (!is.null(image_input)) {
                  image_path <- image_input$datapath
                  base64 <- base64enc::dataURI(file = image_path, mime = image_input$type)
                } else {
                  base64 <- NULL
                }
                
                div(
                  class = "image-upload-cell",
                  style = "width: 200px; height: 300px; background-color: white; border-radius: 15px; padding: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); display: flex; flex-direction: column; align-items: center; justify-content: space-between;",
                  div(class = "product-name", 
                      style = "font-weight: bold; font-size: 16px; text-align: center; margin-top: 10px;",
                      product_name),
                  if (!is.null(base64)) {
                    tags$img(src = base64, class = "product-image", style ="width: 150px; height : 150px; objectif-fit: cover; border-radius: 10px;")
                  } else {
                    div(class = "image-placeholder",
                        style = "width: 150px; height: 150px; display: flex; align-items: center; justify-content: center; background-color: #f0f0f0; border-radius: 10px;",
                        tags$i(class = "fas fa-image"),
                        "No image"
                    )
                  },
                  # ✅ Upload Button (At Bottom)
                  div(class = "file-input-container",
                      style = "margin-bottom: 10px;",
                      fileInput(
                        inputId = paste0("product_image_", i),
                        label = NULL,
                        accept = c('image/png', 'image/jpeg'),
                        buttonLabel = "Upload Image",
                        placeholder = ""
                      )
                  )
                )
              })
          ),
          div(class = "save-button-container",
              actionButton("save_product_images", "Save Images", class = "save-button"))
        )
      })
      
      # print("✅ STEP 5.1: UI for product image uploads generated successfully")
      
      # --------------------------------------------------------------------------------------------------------# 
      ### 📌 Step 6: Observe "Save Images" Button Click for Product Images
      # --------------------------------------------------------------------------------------------------------#
      
      observeEvent(input$save_product_images, {
        req(product_df_data())
        pd <- product_df_data()
        for (i in seq_len(nrow(pd))) {
          file <- input[[paste0("product_image_", i)]]
          if (!is.null(file)) {
            product_images[[ pd$product_code[i] ]] <- list(
              datapath = file$datapath,
              mime     = file$type
            )
          }
        }
        showNotification("Product images saved successfully!", type = "message")
      })
      
    }  # End of if(file_loaded())
  })  # End of observeEvent(file_loaded())
  
  # ────────────────────────────────────────────────────────────────────────────
  # Observer pour afficher en console les noms à afficher pour chaque produit
  # ────────────────────────────────────────────────────────────────────────────
  # observe({
  #   req(file_loaded(), product_df_data(), file_type())
  #   dfp <- product_df_data()
  #   
  #   display_names <- vapply(seq_len(nrow(dfp)), function(i) {
  #     code <- dfp$product_code[i]
  #     user <- dfp$product_name[i]
  #     if (file_type() == "fizz") {
  #       # Fizz : default produit_X ou override
  #       if (!is.na(user) && nzchar(user)) user else code
  #     } else {
  #       # Qualtrics : default produit_<code> ou override
  #       if (!is.na(user) && nzchar(user)) user else paste0("produit_", code)
  #     }
  #   }, FUN.VALUE = "")
  #   
  #   # print dans la console
  #   cat("→ Noms affichés des produits :", paste(display_names, collapse = " | "), "\n")
  # })
  
}
