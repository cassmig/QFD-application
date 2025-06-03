# ── modules/circle_plot_module.R ────────────────────────────────────────────
# This module creates and renders a circular plot of descriptor scores for the 
# selected product. It provides UI elements to choose which axes to display, 
# customize each axis’s appearance, and adjust plot-wide settings. When any 
# relevant input changes, the plot is regenerated.

circle_plot_module <- function(input, output, session) {
  
  # ── 1) Reactive observers for plot dimensions ──────────────────────────────
  # Capture the width and height (in pixels) of the plotOutput from the client side
  ui_w_px <- reactive(session$clientData$output_circle_plot_width)
  ui_h_px <- reactive(session$clientData$output_circle_plot_height)
  
  # Store the previously selected product to detect changes (unused here but ready for extension)
  previous_product <- reactiveVal(NULL)
  
  # ── 2) UI: Checkbox group to select which axes to display ──────────────────
  output$select_puissance_axes_ui <- renderUI({
    # Require that we know how many “power” types (axes) exist
    req(num_puissance_types())
    
    # Generate a vector of axis IDs ("A1", "A2", ..., “A<n>”) and labels ("Axis 1", "Axis 2", ...)
    nb_ptypes   <- num_puissance_types()
    axis_vals   <- paste0("A", seq_len(nb_ptypes))
    axis_labels <- paste("Axis", seq_len(nb_ptypes))
    
    # Render a horizontal, scrollable checkboxGroupInput
    div(
      style = "white-space:nowrap; overflow-x:auto; max-width:100%;",
      checkboxGroupInput(
        inputId  = "selected_axes",
        label    = "Select axes to display:",
        choices  = setNames(axis_vals, axis_labels),
        selected = isolate(input$selected_axes),
        inline   = TRUE
      )
    )
  })
  
  # ── 3) UI: Customization controls for each selected axis ───────────────────
  output$custom_axis_ui <- renderUI({
    # Require that at least one axis is selected
    req(input$selected_axes)
    
    # Define a helper “null-or” operator
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    
    # Create a scrollable vertical container with one “wellPanel” per axis
    div(
      style = "max-height:400px; overflow-y:auto; padding-right:5px;",
      lapply(input$selected_axes, function(axis_id) {
        # Extract numeric part: axis_id is like "A1", so num = "1"
        num <- substring(axis_id, 2)
        
        wellPanel(
          # Heading for this axis
          h5(
            paste("Axis", num),
            style = "color:#007436; font-weight:bold;"
          ),
          fluidRow(
            # Left column: title and color picker
            column(
              6,
              textInput(
                inputId = paste0("axis_title_", axis_id),
                label   = "Title:",
                value   = isolate(input[[paste0("axis_title_", axis_id)]]) %||% paste("Power", num)
              ),
              colourpicker::colourInput(
                inputId = paste0("axis_color_", axis_id),
                label   = "Color:",
                value   = isolate(input[[paste0("axis_color_", axis_id)]]) %||% "#000000"
              )
            ),
            # Right column: horizontal offset and line thickness
            column(
              6,
              numericInput(
                inputId = paste0("axis_offset_", axis_id),
                label   = "Horizontal offset (↔):",
                value   = isolate(input[[paste0("axis_offset_", axis_id)]]) %||% 0,
                step    = 10,
                min     = -100,
                max     = 100
              ),
              numericInput(
                inputId = paste0("axis_thickness_", axis_id),
                label   = "Line thickness:",
                value   = isolate(input[[paste0("axis_thickness_", axis_id)]]) %||% 1.2,
                step    = 0.1,
                min     = 0.5,
                max     = 5
              )
            )
          )
        )
      })
    )
  })
  
  # ── 4) Internal function to generate the circular plot when inputs change ───
  generate_circle_plot <- function() {
    # Require that all necessary reactive inputs and data frames are available
    req(
      selected_product(),       # The product currently selected
      result(),                 # Data frame of descriptor scores for all products
      descriptors_df_data(),    # Mapping of descriptor names
      puissance_table(),        # Data frame of power values per axis
      input$num_descriptors,    # Number of top descriptors to display
      input$equalDescriptors,   # Whether to handle equal-score cases specially
      input$firstDoubleSecond,  # Whether to handle first-descriptor-dominance cases
      input$font_descriptor,    # Font family for descriptor labels
      input$color_descriptor,   # Color for descriptor labels
      input$font_size_descriptor,# Font size for descriptor labels
      input$plot_horizontal_shift,  # Shift for entire circular plot
      input$label_horizontal_shift  # Shift for descriptor labels
    )
    
    # Collect values into local variables
    product_val      <- selected_product()
    result_df        <- result()
    descriptors_df   <- descriptors_df_data()
    puissance_df     <- puissance_table()
    num_desc         <- input$num_descriptors
    equal_desc       <- input$equalDescriptors
    first_double     <- input$firstDoubleSecond
    font_val         <- input$font_descriptor
    color_val        <- input$color_descriptor
    font_size_val    <- input$font_size_descriptor
    plot_shift       <- input$plot_horizontal_shift
    label_shift      <- input$label_horizontal_shift
    # If no axes selected, use an empty character vector
    selected_axes    <- input$selected_axes %||% character(0)
    
    # Build a named list of axis-specific parameters for each selected axis
    axis_params_list <- lapply(selected_axes, function(ax) {
      list(
        title     = input[[paste0("axis_title_",   ax)]],
        color     = input[[paste0("axis_color_",   ax)]],
        offset    = input[[paste0("axis_offset_",  ax)]],
        thickness = input[[paste0("axis_thickness_", ax)]]
      )
    })
    names(axis_params_list) <- selected_axes
    
    # Call a pure function `make_circle_plot()` (defined elsewhere) that returns a ggplot object
    plt <- make_circle_plot(
      product                  = product_val,
      result_df                = result_df,
      descriptors_df           = descriptors_df,
      puissance_df             = puissance_df,
      num_descriptors          = num_desc,
      equalDescriptors         = equal_desc,
      firstDoubleSecond        = first_double,
      font_descriptor          = font_val,
      color_descriptor         = color_val,
      font_size_descriptor     = font_size_val,
      plot_horizontal_shift    = plot_shift,
      label_horizontal_shift   = label_shift,
      show_labels              = input$show_descriptor_labels,
      selected_axes            = selected_axes,
      axis_params_list         = axis_params_list,
      axis_margin_data         = 20,    # Fixed margin around each axis
      spacing_scale            = 1,     # Scale factor for spacing between axes
      axis_spacing_add         = 0,     # Additional spacing per axis
      value_text_size          = 4      # Size of numeric values on each axis
    )
    
    # Store the generated plot in a reactive value, then render it
    current_plot(plt)
    output$circle_plot <- renderPlot({ plt })
  }
  
  # ── 5) Observers: Regenerate plot when relevant inputs change ───────────────
  
  # When the user clicks the “Update Visualization” button:
  observeEvent(input$refresh_plot, {
    # Remove a blinking CSS class (added later) to indicate refresh
    shinyjs::removeClass(selector = "#refresh_plot", class = "blink")
    generate_circle_plot()
  })
  
  # When the selected product changes, regenerate immediately
  observeEvent(selected_product(), {
    generate_circle_plot()
  })
  
  # Add a “blink” effect to the refresh button whenever any plot parameter changes,
  # to prompt the user that they need to click “Update Visualization”
  observeEvent({
    input$num_descriptors
    input$equalDescriptors
    input$firstDoubleSecond
    input$font_descriptor
    input$color_descriptor
    input$selected_axes
    # Also watch each axis’s custom inputs
    lapply(input$selected_axes, function(id) {
      list(
        input[[paste0("axis_title_", id)]],
        input[[paste0("axis_color_", id)]],
        input[[paste0("axis_offset_", id)]],
        input[[paste0("axis_thickness_", id)]]
      )
    })
  }, {
    shinyjs::addClass(selector = "#refresh_plot", class = "blink")
  })
}
