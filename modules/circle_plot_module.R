# 📌 circle_plot_module.R - Generate circular descriptor plots
# This module builds circular visualizations based on descriptor scores

circle_plot_module <- function(input, output, session) {
  
  
  # dans circle_plot_module()  ────────────
  ui_w_px <- reactive( session$clientData$output_circle_plot_width  )
  ui_h_px <- reactive( session$clientData$output_circle_plot_height )
  
  
  previous_product <- reactiveVal(NULL)
  
  # ───────────────────────────────────────────────────
  # 1) UI : cases à cocher pour sélectionner les axes
  # ───────────────────────────────────────────────────
  output$select_puissance_axes_ui <- renderUI({
    req(num_puissance_types())
    nb_ptypes   <- num_puissance_types()
    axis_vals   <- paste0("A", seq_len(nb_ptypes))
    axis_labels <- paste("Axis", seq_len(nb_ptypes))
    div(style = "white-space:nowrap; overflow-x:auto; max-width:100%;",
        checkboxGroupInput(
          inputId  = "selected_axes",
          label    = "Select axes to display:",
          choices  = setNames(axis_vals, axis_labels),
          selected = isolate(input$selected_axes),
          inline   = TRUE
        )
    )
  })
  
  # ───────────────────────────────────────────────────
  # 2) UI : configuration pour chaque axe choisi
  # ───────────────────────────────────────────────────
  output$custom_axis_ui <- renderUI({
    req(input$selected_axes)
    `%||%` <- function(a,b) if (!is.null(a)) a else b
    div(style="max-height:400px; overflow-y:auto; padding-right:5px;",
        lapply(input$selected_axes, function(axis_id) {
          num <- substring(axis_id, 2)
          wellPanel(
            h5(paste("Axis", num), style="color:#007436; font-weight:bold;"),
            fluidRow(
              column(6,
                     textInput(inputId = paste0("axis_title_", axis_id),
                               label   = "Title:",
                               value   = isolate(input[[paste0("axis_title_", axis_id)]]) %||% paste("Power", num)),
                     colourpicker::colourInput(inputId = paste0("axis_color_", axis_id),
                                               label   = "Color:",
                                               value   = isolate(input[[paste0("axis_color_", axis_id)]]) %||% "#000000")
              ),
              column(6,
                     numericInput(inputId = paste0("axis_offset_", axis_id),
                                  label = "Horizontal offset (↔):",
                                  value = isolate(input[[paste0("axis_offset_", axis_id)]]) %||% 0,
                                  step  = 10, min = -100, max = 100),
                     numericInput(inputId = paste0("axis_thickness_", axis_id),
                                  label = "Line thickness:",
                                  value = isolate(input[[paste0("axis_thickness_", axis_id)]]) %||% 1.2,
                                  step  = .1, min = .5, max = 5)
              )
            )
          )
        })
    )
  })
  
  
  
    generate_circle_plot <- function() {
    # À chaque fois qu’un de ces éléments change, on rafraîchit la logique
    # Sécurité : on s’assure que tout est bien défini.
    req(
      selected_product(),
      result(),
      descriptors_df_data(),
      puissance_table(),
      input$num_descriptors,
      input$equalDescriptors,
      input$firstDoubleSecond,
      input$font_descriptor,
      input$color_descriptor,
      input$font_size_descriptor,
      input$plot_horizontal_shift,
      input$label_horizontal_shift
    )
    
    # 2) Récupérer les valeurs
    product_val      <- selected_product()
    result_df        <- result()
    descriptors_df   <- descriptors_df_data()
    puissance_df     <- puissance_table()
    num_desc         <- input$num_descriptors
    equal_desc       <- input$equalDescriptors
    first_double     <- input$firstDoubleSecond
    font_val         <- input$font_descriptor
    color_val        <- input$color_descriptor
    font_size_val  <- input$font_size_descriptor
    plot_shift     <- input$plot_horizontal_shift
    label_shift    <- input$label_horizontal_shift
    selected_axes    <- input$selected_axes %||% character(0)
    
    # 3) Construire la liste des paramètres pour chaque axe
    axis_params_list <- lapply(selected_axes, function(ax) {
      list(
        title     = input[[paste0("axis_title_",   ax)]],
        color     = input[[paste0("axis_color_",   ax)]],
        offset    = input[[paste0("axis_offset_",  ax)]],
        thickness = input[[paste0("axis_thickness_", ax)]]
      )
    })
    names(axis_params_list) <- selected_axes
    

    
    # 4) Appeler la fonction pure qui renvoie le ggplot
    plt <- make_circle_plot(
      product            = product_val,
      result_df          = result_df,
      descriptors_df     = descriptors_df,
      puissance_df       = puissance_df,
      num_descriptors    = num_desc,
      equalDescriptors   = equal_desc,
      firstDoubleSecond  = first_double,
      font_descriptor    = font_val,
      color_descriptor   = color_val,
      font_size_descriptor  = font_size_val,
      plot_horizontal_shift = plot_shift,
      label_horizontal_shift= label_shift,
      show_labels = input$show_descriptor_labels,
      selected_axes      = selected_axes,
      axis_params_list   = axis_params_list,
      axis_margin_data=20,
      spacing_scale=1,
      axis_spacing_add=0, 
      value_text_size=4
    )
    
    # 5) Stocker et afficher
    current_plot(plt)
    output$circle_plot <- renderPlot({ plt })
  }
  
  # Déclencheurs
  observeEvent(input$refresh_plot, {
    shinyjs::removeClass(selector = "#refresh_plot", class = "blink")
    generate_circle_plot()
  })
  
  observeEvent(selected_product(), {
    generate_circle_plot()
  })
  
  
  observeEvent({
    input$num_descriptors
    input$equalDescriptors
    input$firstDoubleSecond
    input$font_descriptor
    input$color_descriptor
    input$selected_axes
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