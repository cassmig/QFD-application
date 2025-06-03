# 📌 results_table.R - Manages the results table display
# This module filters and formats the results for the selected product and applies dynamic styles.

result_table <- function(input, output, session) {
  
  # --------------------------------------------------------------------------------------------------------
  # Step 1: Render UI for Panelist Selection
  # This renders a numeric input allowing the user to adjust the number of panelists for scaling scores.
  # --------------------------------------------------------------------------------------------------------
  output$file_info_text4 <- renderUI({
    tagList(
      div(
        class = "file-info-text",
        style = "text-align: center; margin-top: 22px; width: 100%;",
        HTML(
          "<em>Customize the <strong>number of panelists</strong> below to tailor your analysis. ",
          "The default is set to <strong>12 panelists</strong>.</em>"
        ),
        div(
          style = "display: flex; align-items: center; justify-content: center; margin-top: 15px;",
          tags$i(
            class = "fas fa-users",
            style = "font-size: 24px; margin-right: 10px; color: #007436;"
          ),
          div(
            class = "numeric-input",
            numericInput(
              inputId = "num_panelists_selection",
              label = NULL,
              value = 12,    # Default panelist count
              min = 1,
              width = '100px'
            )
          )
        )
      )
    )
  })
  
  # --------------------------------------------------------------------------------------------------------
  # Step 2: Reactive Value for Selected Number of Panelists
  # This captures the user’s selection for how many panelists to use when scaling scores.
  # --------------------------------------------------------------------------------------------------------
  selected_num_panelists <- reactive({
    req(input$num_panelists_selection)
    input$num_panelists_selection
  })
  
  # --------------------------------------------------------------------------------------------------------
  # Step 3: Filter Results Based on Selected Product & Adjust Scores
  # - Filters the full result table by the currently selected product.
  # - If the user has changed the panelist count, scales the sum_coefficients accordingly.
  # --------------------------------------------------------------------------------------------------------
  filtered_result <- reactive({
    req(selected_product())
    req(result())
    
    # Extract the full result data.frame
    res <- result()
    
    # Filter by the chosen product
    filtered <- res %>%
      filter(product == selected_product())
    
    # Retrieve the original and user-selected panelist counts
    initial_panels  <- num_panelists()
    selected_panels <- selected_num_panelists()
    
    # If panelist count changed, apply ratio scaling to sum_coefficients
    if (!is.null(initial_panels) && selected_panels != initial_panels) {
      ratio <- selected_panels / initial_panels
      filtered <- filtered %>%
        mutate(sum_coefficients = round(sum_coefficients * ratio))
    } else {
      filtered <- filtered %>%
        mutate(sum_coefficients = round(sum_coefficients))
    }
    
    return(filtered)
  })
  
  # --------------------------------------------------------------------------------------------------------
  # Step 4: Render Results Table with Dynamic Styling
  # - Joins descriptor names and colors.
  # - Applies background colors and chooses appropriate text colors for readability.
  # --------------------------------------------------------------------------------------------------------
  output$results_table <- DT::renderDataTable({
    req(filtered_result())
    req(descriptors_df_data())
    
    # Prepare the table data by joining with descriptors_df_data
    table_data <- filtered_result() %>%
      left_join(descriptors_df_data(), by = c("attribute" = "original_attribute")) %>%
      mutate(
        descriptor_name = ifelse(!is.na(new_name_attribute), new_name_attribute, attribute),
        COLOR = descriptor_color
      ) %>%
      select(descriptor_name, sum_coefficients, COLOR)
    
    # Helper: Determine if a hex color is light (so text should be black) or dark (text white)
    is_light_color <- function(hex_color) {
      if (is.na(hex_color) || !grepl("^#(?:[0-9A-Fa-f]{3}){1,2}$", hex_color)) {
        return(FALSE)
      }
      rgb_vals <- tryCatch(col2rgb(hex_color), error = function(e) return(c(0,0,0)))
      luminance <- (0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]) / 255
      return(luminance > 0.5)
    }
    
    # Determine unique colors in the table and assign text colors accordingly
    unique_colors <- unique(table_data$COLOR)
    text_colors   <- ifelse(sapply(unique_colors, is_light_color), "black", "white")
    
    # Use the font selected by the user, default to Arial if not specified
    selected_font <- ifelse(is.null(input$font_descriptor), "Arial", input$font_descriptor)
    
    # Render the datatable
    DT::datatable(
      table_data,
      colnames = c("Descriptor", "Score", "Color"),
      options = list(
        pageLength = Inf,       # Show all rows (no pagination)
        dom = 't',              # Display only the table (no search, no pagination controls)
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Center-align all columns
        )
      ),
      rownames = FALSE,
      style = 'cell-border stripe',
      class = 'cell-border stripe',
      escape = FALSE
    ) %>%
      # Apply the user-selected font family to all cells
      DT::formatStyle(
        columns = names(table_data),
        fontFamily = selected_font
      ) %>%
      # Center-align and bold the Score column
      DT::formatStyle(
        'sum_coefficients',
        `text-align` = 'center',
        fontWeight = 'bold'
      ) %>%
      # Apply background colors from COLOR, with contrasting text colors
      DT::formatStyle(
        'COLOR',
        backgroundColor = styleEqual(unique_colors, unique_colors),
        color           = styleEqual(unique_colors, text_colors),
        `text-align`    = 'center',
        fontWeight      = 'bold'
      )
  })
  
}
