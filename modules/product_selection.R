# 📌 product_selection.R - Manages product selection for analysis
# This module allows users to select a product from a list of available products and displays relevant information.

product_selection <- function(input, output, session) {
  
  # --------------------------------------------------------------------------------------------------------# 
  ### 📌 Step 1: Retrieve and Order Product Data Dynamically
  # --------------------------------------------------------------------------------------------------------#
  
  ordered_product_data <- reactive({
    req(product_df_data())  # Ensure product data is available
    
    product_data <- product_df_data()  # Retrieve product data
    
    # Use product names if available; otherwise, use product codes
    product_data <- product_data %>%
      mutate(display_name = ifelse(product_name != "", product_name, product_code))
    
    # Sort Fizz product codes numerically if applicable
    if (!is.null(file_type()) && file_type() == "fizz") {
      numeric_suffix <- as.numeric(gsub("[^0-9]", "", product_data$product_code))
      
      if (all(!is.na(numeric_suffix))) {
        sorted_indices <- order(numeric_suffix)
        product_data <- product_data[sorted_indices, ]
      } else {
        showNotification("Warning: Some product names do not follow the expected format 'produit_X'. Order may be incorrect.", type = "warning")
      }
    }
    
    return(product_data)  # Return sorted and formatted product data
  })
  
  # print("✅ STEP 1: Product data retrieved and sorted")
  
  # --------------------------------------------------------------------------------------------------------# 
  ### 📌 Step 2: Render the UI for Product Selection
  # --------------------------------------------------------------------------------------------------------#
  
  output$productSelectionUI <- renderUI({
    req(file_loaded(), ordered_product_data())  # Ensure data is loaded
    
    product_data <- ordered_product_data()  # Retrieve sorted product data
    
    # print("✅ STEP 2: Generating Product Selection UI")
    
    # Create product selection grid dynamically
    div(
      class = "product-selection-grid",
      lapply(1:nrow(product_data), function(i) {
        product_code <- product_data$product_code[i]  # Extract product code
        product_name <- product_data$display_name[i]  # Extract display name
        image_info <- product_images[[product_code]]  # Retrieve stored product image
        
        # Check if an image exists for the product
        if (!is.null(image_info)) {
          base64 <- base64enc::dataURI(file = image_info$datapath, mime = image_info$type)
          image_tag <- tags$img(src = base64, class = "product-card-image")  # Display uploaded image
        } else {
          image_tag <- div(class = "no-image", "No image")  # Placeholder for missing images
        }
        
        # Create a clickable product card
        actionButton(
          inputId = paste0("select_product_", i),
          label = div(
            class = "product-card",
            image_tag,  # Product Image
            div(class = "product-card-name", product_name)  # Product Name
          ),
          class = "product-card-button"
        )
      })
    )
  })
  
  # print("✅ STEP 2.1: Product Selection UI generated successfully")
  
  # --------------------------------------------------------------------------------------------------------# 
  ### 📌 Step 3: Observe Clicks on Product Cards and Update Selected Product
  # --------------------------------------------------------------------------------------------------------#
  
  observe({
    product_data <- ordered_product_data()
    
    lapply(1:nrow(product_data), function(i) {
      observeEvent(input[[paste0("select_product_", i)]], {
        product_code <- product_data$product_code[i]
        
        # Si le fichier est de type "qualtrics", on fait la correspondance
        if (!is.null(file_type()) && file_type() == "qualtrics") {
          corresponding_product <- result() %>%
            filter(old_product == product_code) %>%
            distinct(product) %>%
            pull(product)
          
          if (length(corresponding_product) == 1) {
            selected_product(corresponding_product)
          } else {
            selected_product(NULL)
            warning(paste("❌ Produit non trouvé pour code Qualtrics :", product_code))
          }
        } else {
          # Pour Fizz : on utilise directement le code
          selected_product(product_code)
        }
      })
    })
  })
  
  # print("✅ STEP 3.1: Product selection observer initialized")
  
  # --------------------------------------------------------------------------------------------------------# 
  ### 📌 Step 4: Render Dynamic Text Based on Selected Product
  # --------------------------------------------------------------------------------------------------------#
  
  output$product_info_text <- renderUI({
    req(selected_product())  # Ensure a product has been selected
    
    # Retrieve product data and find selected product name
    product_data <- product_df_data()
    selected_product_code <- selected_product()
    product_name <- product_data$product_name[product_data$product_code == selected_product_code]
    
    # Use product name if available; otherwise, use product code
    product_display_name <- ifelse(product_name == "", selected_product_code, product_name)
    
    # Generate the dynamic information text
    div(
      class = "intro-text",
      HTML(
        paste0(
          "The selected product is <strong>", product_display_name, "</strong>.<br>",
          "Below you will find the score obtained for each descriptor.<br>",
          "The score is obtained as follows: when the descriptor is cited first, it gets a score of +3; ",
          "when it's selected second, it gets +2; and when it's selected third, it gets +1.<br>"
        )
      )
    )
  })
  
  # print("✅ STEP 4: Dynamic product information text rendered")
  
  # --------------------------------------------------------------------------------------------------------# 
  ### 📌 Step 5: Render UI for Customizing the Number of Panelists
  # --------------------------------------------------------------------------------------------------------#
  
  output$file_info_text4 <- renderUI({
    if (file_loaded()) {
      tagList(
        div(
          class = "file-info-text",
          style = "text-align: center; margin-top: 22px; width: 100%;",
          HTML("<em>Customize the <strong>number of panelists</strong> below to tailor your analysis. The default is set to <strong>12 panelists</strong>.</em>"),
          
          # Display a numeric input field for panelist selection
          div(
            style = "display: flex; align-items: center; justify-content: center; margin-top: 15px;",
            tags$i(class = "fas fa-users", style = "font-size: 24px; margin-right: 10px; color: #007436;"),
            div(
              class = "numeric-input",
              numericInput(
                inputId = "num_panelists_selection",
                label = NULL,
                value = 12,  # Default number of panelists
                min = 1,      # Minimum value allowed
                width = '100px'
              )
            )
          )
        )
      )
    }
  })
  
  # print("✅ STEP 5: Panelist customization UI rendered successfully")
}
