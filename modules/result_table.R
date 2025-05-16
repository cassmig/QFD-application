# 📌 results_table.R - Manages the results table display
# This module filters and formats the results for the selected product and applies dynamic styles.


result_table <- function(input, output, session) {
  
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 1: Render UI for Panelist Selection
  # --------------------------------------------------------------------------------------------------------#
  
 # Render the panelist customization section (numeric input)
  output$file_info_text4 <- renderUI({
    
    # Display the panelist selection UI
    tagList(
      div(
        class = "file-info-text",
        style = "text-align: center; margin-top: 22px; width: 100%;",
        HTML("<em>Customize the <strong>number of panelists</strong> below to tailor your analysis. The default is set to <strong>12 panelists</strong>.</em>"),
        
        # Numeric input container
        div(
          style = "display: flex; align-items: center; justify-content: center; margin-top: 15px;",
          tags$i(class = "fas fa-users", style = "font-size: 24px; margin-right: 10px; color: #007436;"), #Icon representing panelists
          
          # Numeric input field
          div(
            class = "numeric-input",
            numericInput(
              inputId = "num_panelists_selection",
              label = NULL,
              value = 12,     # Default panelist count
              min = 1,
              width = '100px'
            )
          )
        )
      )
    )
  })
  
        # --------------------------------------------------------------------------------------------------------
        # 🛠 Debugging: Print the currently selected product
        # --------------------------------------------------------------------------------------------------------
        # observe({
        #   cat("\n[DEBUG - Step 1] selected_product() ->", selected_product(), "\n")
        # })


  
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 2: Reactive Value for Selected Number of Panelists
  # --------------------------------------------------------------------------------------------------------#
  
  # Reactive value to retrieve the number of panelists selected by the user
  selected_num_panelists <- reactive({
    req(input$num_panelists_selection)    # Ensure the input exists before proceeding
    input$num_panelists_selection         # Return the user-selected number of panelists
  })
  
        # --------------------------------------------------------------------------------------------------------
        # 🛠 Debugging: Print the selected panelist count every time it changes
        # --------------------------------------------------------------------------------------------------------
        # observe({
        #   cat("\n[DEBUG - Step 2] Selected panelist count =", selected_num_panelists(), "\n")
        # })
        
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 3: Filter Results Based on Selected Product & Adjust Scores
  # --------------------------------------------------------------------------------------------------------#
  
  filtered_result <- reactive({
    req(selected_product())  # Ensure a product is selected
    req(result())            # Ensure the result data exists
    
    res <- result()
    
    # Filter the result by the selected product code (stable key)
    filtered <- res %>%
      filter(product == selected_product())
    
    # Get initial and selected panelist counts
    initial_panels <- num_panelists()
    selected_panels <- selected_num_panelists()
    
    # Adjust score scaling if number of panelists has changed
    if (!is.null(initial_panels) && selected_panels != initial_panels) {
      ratio <- selected_panels / initial_panels
      filtered <- filtered %>%
        mutate(sum_coefficients = round(sum_coefficients * ratio))
    } else {
      filtered <- filtered %>%
        mutate(sum_coefficients = round(sum_coefficients))
    }
    
        # --------------------------------------------------------------------------------------------
        # 🛠 Debugging: Output key information during filtering process
        # --------------------------------------------------------------------------------------------
        
        # cat("\n✅ STEP 3: Filtering results for selected product ->", selected_product(), "\n")
        # 
        # if (nrow(filtered) == 0) {
        #   cat("⚠ WARNING: No results found for selected product!\n")
        #   return(tibble())  # Return empty tibble to avoid breaking UI
        # }
        # 
        # cat("\n📌 DEBUG: Filtered rows before score adjustment (first few):\n")
        # print(head(filtered))
        # 
        # cat("\n📌 DEBUG: Adjusting scores - Initial panelists:", initial_panels,
        #     "| Selected panelists:", selected_panels, "\n")
        # 
        # cat("\n📌 DEBUG: Adjusted scores after recalculation:\n")
        # print(
        #   filtered %>%
        #     select(product, attribute, sum_coefficients, count, new_name_product) %>%
        #     arrange(desc(sum_coefficients))
        # )
    
        ## Live observer for debugging: Display content of `filtered_result()` after any update
        # observe({
        #   cat("\n📌 DEBUG: Final filtered_result() content before display:\n")
        #   print(filtered_result())
        # })
    
    return(filtered)
  })
  
  # --------------------------------------------------------------------------------------------------------
  ### 📌 Step 5: Render Results Table with Styling
  # --------------------------------------------------------------------------------------------------------
  
  # observe({
  #   cat("📌 DEBUG: Contents of filtered_result() before rendering the table:\n")
  #   print(filtered_result())
  # })
  
  output$results_table <- DT::renderDataTable({
    req(filtered_result())
    req(descriptors_df_data())
    
    # Prepare the table
    table_data <- filtered_result() %>%
      left_join(descriptors_df_data(), by = c("attribute" = "original_attribute")) %>%
      mutate(
        descriptor_name = ifelse(!is.na(new_name_attribute), new_name_attribute, attribute),
        COLOR = descriptor_color
      ) %>%
      select(descriptor_name, sum_coefficients, COLOR)
    
    # Handle light/dark text
    is_light_color <- function(hex_color) {
      if (is.na(hex_color) || !grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", hex_color)) return(FALSE)
      rgb <- tryCatch(col2rgb(hex_color), error = function(e) return(c(0, 0, 0)))
      luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
      return(luminance > 0.5)
    }
    
    unique_colors <- unique(table_data$COLOR)
    text_colors <- ifelse(sapply(unique_colors, is_light_color), "black", "white")
    selected_font <- ifelse(is.null(input$font_descriptor), "Arial", input$font_descriptor)
    
    # Render the datatable
    DT::datatable(
      table_data,
      colnames = c("Descriptor", "Score", "Color"),
      options = list(
        pageLength = Inf,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Center column headers
        )
      ),
      rownames = FALSE,
      style = 'default',
      class = 'cell-border stripe',
      escape = FALSE
    ) %>%
      DT::formatStyle(
        columns = names(table_data),
        fontFamily = selected_font
      ) %>%
      DT::formatStyle(
        'sum_coefficients',
        `text-align` = 'center',
        fontWeight = 'bold'  # Make score bold
      ) %>%
      DT::formatStyle(
        'COLOR',
        backgroundColor = styleEqual(unique_colors, unique_colors),
        color = styleEqual(unique_colors, text_colors),
        `text-align` = 'center',
        fontWeight = 'bold'
      )
  })
  
  



}