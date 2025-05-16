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
        product_data <- product_df_data()  # Retrieve the product dataset
        
        # If the file type is "Fizz", sort products based on their numerical suffix
        if (file_type() == "fizz") {
          numeric_suffix <- as.numeric(gsub("[^0-9]", "", product_data$product_code))
          sorted_indices <- order(numeric_suffix)
          product_data <- product_data[sorted_indices, ]
        }
        
        # print("✅ STEP 2: Generating UI for product renaming")
        
        tagList(
          div(class = "descriptors-grid",
              lapply(1:nrow(product_data), function(i) {
                div(
                  class = "descriptor-container",
                  style = paste0("width: ", max(150, nchar(product_data$product_code[i]) * 10), "px;"),
                  textInput(
                    inputId = paste0("product_name_", product_data$product_code[i]),
                    label = product_data$product_code[i],  # Display the product code
                    value = product_data$product_name[i]   # Use existing name or empty for Fizz
                  )
                )
              })
          ),
          div(class = "save-button-container",
              actionButton("save_products", "Save Changes", class = "save-button"))
        )
      })
      
      # print("✅ STEP 2.1: UI for product renaming generated successfully")
      
  
      # --------------------------------------------------------------------------------------------------------# 
      ### 📌 Step 3: Observe "Save Changes" Button Click for Product Names
      # --------------------------------------------------------------------------------------------------------#
      
      observeEvent(input$save_products, {
        product_data <- product_df_data()
        
        # 1️⃣ Retrieve new product names (keep original if empty)
        new_product_names <- sapply(product_data$product_code, function(code) {
          new_name <- input[[paste0("product_name_", code)]]
          if (is.null(new_name) || new_name == "") return(code)  # fallback to code
          return(new_name)
        })
        
        # 2️⃣ Update the reactive product data with safe names
        product_df_data_updated <- product_data
        product_df_data_updated$product_name <- new_product_names
        product_df_data(product_df_data_updated)
        
        # 🟩 Debug: Show the updated names
        cat("✅ STEP 3.1: Updated product names:\n")
        print(new_product_names)
        
        showNotification("Product names updated successfully!", type = "message", duration = 4)
        
        # 3️⃣ Update the column new_name_product in result()
        req(result())
        old_res <- result()
        
        # Create column if it doesn't exist yet
        if (!"new_name_product" %in% names(old_res)) {
          old_res$new_name_product <- old_res$product
          # cat("✅ INFO: new_name_product initialized with default product codes.\n")
        }
        
        # Match and update names
        df_prod <- product_df_data()
        idx <- match(old_res$product, df_prod$product_code)
        old_res$new_name_product <- df_prod$product_name[idx]
        
        # 4️⃣ Save the updated table
        result(old_res)
        
        # 🐛 Debug
        cat("\n-------------------------------------------------\n")
        cat("DEBUG: product_df_data() after name update:\n")
        print(head(product_df_data()))
        cat("DEBUG: result() after new_name_product update:\n")
        print(head(result()))
        cat("-------------------------------------------------\n\n")
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
        product_data <- product_df_data()
        
        # print("✅ STEP 6: Save button clicked, processing product image uploads...")
        
        for (i in 1:nrow(product_data)) {
          image_input <- input[[paste0("product_image_", i)]]
          
          if (!is.null(image_input)) {
            product_images[[product_data$product_code[i]]] <- image_input
          }
        }
        
        # 🔹 Display a success notification
        showNotification("Product images saved successfully!", type = "message", duration = 4)
        
        # ## Debugging: Print success confirmation
        # print("✅ STEP 6.1: Product images successfully saved")
      })
      
    }  # End of if(file_loaded())
  })  # End of observeEvent(file_loaded())
  
}
