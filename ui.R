# 📌 ui.R - Define user interface for QFD Data Analysis application

# Load necessary JavaScript utilities
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs for dynamic UI interactions
  
  # ── Include custom CSS and Font Awesome icons ──────────────────────────────
  tags$head(
    # Font Awesome (version 6 and fallback to version 5 for compatibility)
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
    ),
    tags$link(
      rel  = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.15.3/css/all.css"
    ),
    # Google Fonts for consistent typography across browsers
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto&family=Open+Sans&family=Lato&family=Montserrat&family=Raleway&display=swap"
    ),
    # Custom CSS file (styles.css) for app-specific styling
    tags$link(
      rel  = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    # Custom JavaScript file (script.js) for client-side logic (e.g., tab handling)
    tags$script(src = "script.js")
  ),
  
  # ── Hide Shiny’s default download icon (to replace with Font Awesome icon) ──
  tags$head(
    tags$style(HTML("
      /* Hide the default fa-download icon that Shiny injects */
      #download_full_report .fa-download {
        display: none !important;
      }
    "))
  ),
  
  # ── Header Bar: Logo, Title, and Navigation Sections ───────────────────────
  div(class = "header",
      # Application logo (fixed dimensions)
      img(
        src   = "https://www.mane.com/theme/images/logo.png",
        alt   = "Logo",
        style = "height: 70px; width: 70px;"
      ),
      # Application title
      div(class = "title", "QFD Data Analysis"),
      # Navigation sections (clickable via JavaScript to switch hidden tabs)
      div(
        class = "nav-section",
        id    = "nav_data_importation",  # Unique ID for tab navigation
        tags$i(class = "fas fa-upload"),  # Icon representing data import
        "DATA IMPORTATION"
      ),
      div(
        class = "nav-section",
        id    = "nav_data_visualization",
        tags$i(class = "fas fa-chart-bar"),  # Icon representing visualization
        "DATA VISUALIZATION"
      ),
      div(
        class = "nav-section",
        id    = "nav_statistical_analysis",
        tags$i(class = "fas fa-calculator"),  # Icon representing analysis
        "STATISTICAL ANALYSIS"
      ),
      div(
        class = "nav-section",
        id    = "nav_automatic_report",
        tags$i(class = "fas fa-file-alt"),  # Icon representing reporting
        "AUTOMATIC REPORT"
      )
  ),
  
  # ── Main Tabs (Hidden TabsetPanel) ─────────────────────────────────────────
  tabsetPanel(
    id   = "main_tabs",
    type = "hidden",
    
    # ─────────────────────────────────────────────────────────────────────────
    # Tab 1: DATA IMPORTATION
    # ─────────────────────────────────────────────────────────────────────────
    tabPanel(
      title = "DATA IMPORTATION",
      value = "data_importation",
      
      # Introductory text describing app functionality and contact info
      div(class = "intro-text",
          tags$strong(
            tags$em(
              "The QFD Data Analysis application lets you import QFD data from Qualtrics or FIZZ applications. "
            )
          ),
          "You can check for calculation errors, generate rainbow graphs, customize colours, perform statistical analysis, export visuals, and much more. ",
          "If you have any questions about the application, please contact me at my email address: ",
          tags$a(href = "mailto:Cassandre.MIGLIORE@mane.com", "Cassandre.MIGLIORE@mane.com"),
          "."
      ),
      
      # ── File Selection & Upload Container ───────────────────────────────────
      div(class = "container-frame2",
          
          # Dark title bar at the top of the container
          div(class = "title-bar",
              "Select and Upload Your Excel File"
          ),
          
          # Content area: select source, choose file, and load data
          div(class = "content",
              
              # Flex container aligning source dropdown, file upload button, and load button
              div(
                style = "display: flex; align-items: center; justify-content: flex-start;",
                
                # Source selection dropdown (Fizz or Qualtrics)
                div(class = "select-source",
                    selectInput(
                      "source",
                      "Select Source:",
                      choices = c("Fizz", "Qualtrics")
                    )
                ),
                
                # Green box containing hidden file input and styled upload button
                div(class = "green-box",
                    tags$div(
                      style = "position: relative; display: flex; align-items: center; justify-content: center;",
                      # Hidden file input (accepts .xlsx files)
                      tags$input(
                        type    = "file",
                        id      = "file",
                        class   = "file-input",
                        accept  = ".xlsx"
                      ),
                      # Visible upload button with Font Awesome icon
                      tags$button(
                        id    = "upload_button",
                        class = "upload-button",
                        tags$i(class = "fas fa-upload"),
                        "Upload File"
                      ),
                      # Container to display selected file name
                      div(class = "file-name-container",
                          textOutput("file_name")
                      )
                    )
                ),
                
                # Load Data button
                div(class = "load-button-container",
                    div(class = "load-button-wrapper",
                        actionButton(
                          "load_button",
                          HTML('<strong>Load Data</strong>'),
                          class = "load-button"
                        )
                    )
                )
              ),
              
              # File status message (e.g., "No file selected")
              div(
                id    = "file_status",
                class = "alert-text",
                textOutput("no_file_message")
              )
          )
      ),
      
      # ── Rename Descriptors Container ────────────────────────────────────────
      div(
        class = "container-frame",
        style = "position: relative;",  # Required for title bar overlay
        
        # Dark title bar for renaming descriptors
        div(class = "title-bar",
            h4(
              tags$strong("Rename Descriptors"),
              style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
            )
        ),
        
        # Dynamic instruction or status based on uploaded file
        div(
          style = "margin-bottom: 5px;",
          tags$br(),
          uiOutput("file_info_text")
        ),
        
        # Content area: placeholder for UI elements to rename descriptors
        div(class = "content",
            uiOutput("descriptors_ui")
        )
      ),
      
      # ── Assign Colors to Descriptors Container ─────────────────────────────
      div(
        class = "container-frame3",
        style = "max-height: 600px; height: auto; position: relative; margin-bottom:10px;",
        
        # Title bar for color assignment
        div(class = "title-bar",
            h4(
              tags$strong("Assign Colors to Descriptors"),
              style = "margin-bottom: 10px; font-size: 16px; color: #007436;"
            )
        ),
        
        # Content area: instructions and color picker UI
        div(class = "content",
            # Instruction text appears only when a file is loaded
            uiOutput("instructionText"),
            tags$br(),
            # UI elements for selecting colors for each descriptor
            uiOutput("descriptor_color_ui")
        )
      ),
      
      # ── Rename Products Container ───────────────────────────────────────────
      div(
        class = "container-frame",
        style = "max-height: none; height: auto; position: relative; margin-bottom:10px;",
        
        # Title bar for product renaming
        div(class = "title-bar",
            h4(
              tags$strong("Rename Products"),
              style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
            )
        ),
        
        # Dynamic instruction based on uploaded file
        div(
          style = "margin-bottom: 5px;",
          tags$br(),
          uiOutput("file_info_text2")
        ),
        
        # Content area: placeholder for UI elements to rename products
        div(class = "content",
            uiOutput("productRenameUI")
        )
      ),
      
      # ── Associate Images with Products Container ────────────────────────────
      div(
        class = "container-frame",
        style = "max-height: none; height: auto; position: relative; margin-bottom:10px;",
        
        # Title bar for image association
        div(class = "title-bar",
            h4(
              tags$strong("Associate Images with Products"),
              style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
            )
        ),
        
        # Dynamic instruction based on uploaded file
        div(
          style = "margin-bottom: 5px;",
          tags$br(),
          uiOutput("file_info_text3")
        ),
        
        # Content area: placeholder for UI elements to upload images per product
        div(class = "content",
            uiOutput("productImageUploadUI")
        )
      )
    ),
    
    # ─────────────────────────────────────────────────────────────────────────
    # Tab 2: DATA VISUALIZATION
    # ─────────────────────────────────────────────────────────────────────────
    tabPanel(
      title = "DATA VISUALIZATION",
      value = "data_visualization",
      
      # Introductory HTML text for Data Visualization tab
      div(
        class = "intro-text",
        HTML(
          "Welcome to the <strong>Data Visualization</strong> tab. In this section, you can <strong>generate graphs and tables</strong> that <strong>rank descriptors</strong> based on <strong>various options</strong>. To begin, please <strong>select a product</strong> from the <strong>dropdown menu below</strong>."
        )
      ),
      
      # ── Product Selection Container ────────────────────────────────────────
      div(class = "container-frame3",
          
          # Title bar for selecting a product
          div(class = "title-bar",
              h4(
                tags$strong("Select a Product"),
                style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
              )
          ),
          
          # Content area: placeholder for UI to choose a product
          div(class = "content",
              uiOutput("productSelectionUI")
          )
      ),
      
      # Dynamic text based on the selected product
      uiOutput("product_info_text"),
      
      # ── Results Table and Circle Plot Container ───────────────────────────
      div(
        class = "container-frame3 dataviz-container",
        
        # Title bar for results table and plot
        div(class = "title-bar",
            h4(
              tags$strong("Results Table and Plot"),
              style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
            )
        ),
        
        # Content area with flexible layout for table and plot side by side
        div(
          class = "content",
          style = "width: 100%;",
          
          # Dynamic information text (e.g., instructions or summary)
          uiOutput("file_info_text4"),
          
          # Flex container: two columns (Results Table and Circle Plot)
          tags$div(
            style = "display: flex; gap: 10px; align-items: stretch; margin-top: 20px;",
            
            # ── Column 1: Results Table ───────────────────────────────────
            tags$div(
              class = "column",
              style = "flex: 1; display: flex; flex-direction: column;",
              
              # Header for the results table
              div(
                class = "plot-result-table-header",
                "Result Table",
                style = "margin-bottom: 10px;"
              ),
              
              # DataTable output for showing descriptor results
              DT::dataTableOutput("results_table")
            ),
            
            # Vertical divider between table and plot
            tags$div(
              style = "width:1px; height:100%; background:#222;"
            ),
            
            # ── Column 2: Circle Plot ────────────────────────────────────
            tags$div(
              class = "column",
              style = "flex: 1; display: flex; flex-direction: column;",
              
              # Header for the circle plot
              div(
                class = "circle-plot-header",
                "Circle Plot",
                style = "margin-bottom: 10px;"
              ),
              
              # Plot output area with fixed height
              plotOutput("circle_plot", height = "400px"),
              
              # Button to refresh/update the visualization
              actionButton("refresh_plot", "Update Visualization", icon = icon("sync"))
            )
          )
        )
      ),
      
      # ── Plot Options Container ────────────────────────────────────────────
      div(class = "container-frame3",
          
          # Title bar for plot customization options
          div(class = "title-bar",
              h4(
                tags$strong("Plot Options"),
                style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
              )
          ),
          
          # Content area: descriptive text and grouped controls
          div(class = "content",
              
              # Instructional text describing how to customize the plot
              div(
                class = "file-info-text",
                style = "text-align: left; margin-top: 10px;",
                HTML(
                  "Below, you will find all the parameters available to <strong>personalize your plot</strong>. Modify these settings to adjust<strong> the appearance and functionality</strong> of your visualization according to your needs."
                )
              ),
              tags$br(),
              
              # Flex container for three columns of plot options
              tags$div(
                style = "display: flex; gap: 20px; align-items: flex-start;",
                
                # ── Column 1: Descriptor Options ───────────────────────────
                tags$div(
                  class = "options-container",
                  style = "flex: 1; padding: 15px; background-color: rgba(240, 245, 240, 0.9); border: 1px solid #007436; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); height: 700px;",
                  
                  h5(
                    "Descriptor Options",
                    style = "color: #007436; font-size: 18px; font-weight: bold; margin-bottom: 15px; text-align: center;"
                  ),
                  
                  # Number of descriptors slider (input$num_descriptors)
                  h5(
                    "Number of Descriptors",
                    style = "color: #007436; font-size: 16px; font-weight: bold;"
                  ),
                  p(
                    "Select the number of descriptors to display in the plot.",
                    style = "font-weight: bold; font-size: 14px;"
                  ),
                  sliderInput(
                    inputId = "num_descriptors",
                    label   = NULL,
                    min     = 2,
                    max     = 5,
                    value   = 3,
                    step    = 1,
                    ticks   = FALSE,
                    animate = animationOptions(interval = 100, loop = FALSE)
                  ),
                  tags$br(),
                  
                  # Equal descriptors handling (Yes/No radio buttons)
                  h5(
                    "Equal Descriptors Handling",
                    style = "color: #007436; font-size: 16px; font-weight: bold;"
                  ),
                  p(
                    "Choose whether to represent cases where descriptors have equal scores.",
                    style = "font-weight: bold; font-size: 14px;"
                  ),
                  radioButtons(
                    inputId = "equalDescriptors",
                    label   = NULL,
                    choices = c("Yes" = "yes", "No" = "no"),
                    selected = "no",
                    inline   = TRUE
                  ),
                  tags$br(),
                  
                  # First descriptor dominance (Yes/No radio buttons)
                  h5(
                    "First Descriptor Dominance",
                    style = "color: #007436; font-size: 16px; font-weight: bold;"
                  ),
                  p(
                    "Select to represent cases where the first descriptor's score is twice as high as the second.",
                    style = "font-weight: bold; font-size: 14px;"
                  ),
                  radioButtons(
                    inputId = "firstDoubleSecond",
                    label   = NULL,
                    choices = c("Yes" = "yes", "No" = "no"),
                    selected = "no",
                    inline   = TRUE
                  )
                ),
                
                # ── Column 2: Style Options ───────────────────────────────
                tags$div(
                  class = "options-container",
                  style = "flex: 1; padding: 15px; background-color: rgba(240, 245, 240, 0.9); border: 1px solid #007436; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); height: 700px;",
                  
                  h5(
                    "Style Options",
                    style = "color: #007436; font-size: 18px; font-weight: bold; margin-bottom: 15px; text-align: center;"
                  ),
                  
                  # Font and color settings for descriptor labels
                  fluidRow(
                    column(
                      width = 6,
                      h5(
                        "Font for Descriptors",
                        style = "color: #007436; font-size: 16px; font-weight: bold;"
                      ),
                      p(
                        "Choose the font style for the descriptor names in the plot.",
                        style = "font-size: 14px; font-weight: bold;"
                      ),
                      selectizeInput(
                        inputId = "font_descriptor",
                        label   = NULL,
                        choices = c(
                          "Arial", "Calibri", "Times New Roman", "Verdana", "Georgia",
                          "Tahoma", "Trebuchet MS", "Century Gothic", "Lucida Sans Unicode",
                          "Segoe UI"
                        ),
                        selected = "Arial",
                        options = list(
                          render = I("
          {
            option: function(item, escape) {
              return '<div style=\"font-family:' + escape(item.value) + ';\">'
                     + escape(item.label) + '</div>';
            },
            item: function(item, escape) {
              return '<div style=\"font-family:' + escape(item.value) + ';\">'
                     + escape(item.label) + '</div>';
            }
          }
        ")
                        )
                      )
                    ),
                    column(
                      width = 6,
                      h5(
                        "Color for Descriptors",
                        style = "color: #007436; font-size: 16px; font-weight: bold;"
                      ),
                      p(
                        "Choose the text color for the descriptor names in the plot.",
                        style = "font-size: 14px; font-weight: bold;"
                      ),
                      selectizeInput(
                        inputId = "color_descriptor",
                        label   = NULL,
                        choices = c(
                          "Black"      = "#000000",
                          "Red"        = "#FF0000",
                          "Blue"       = "#0000FF",
                          "Green"      = "#007436",
                          "Purple"     = "#800080",
                          "Orange"     = "#FFA500",
                          "Brown"      = "#A52A2A",
                          "Dark Gray"  = "#A9A9A9",
                          "Turquoise"  = "#008080",
                          "Navy"       = "#000080"
                        ),
                        selected = "#000000",
                        options = list(
                          render = I("
          {
            option: function(item, escape) {
              return '<div style=\"color:' + escape(item.value) + ';\">'
                     + escape(item.label) + '</div>';
            },
            item: function(item, escape) {
              return '<div style=\"color:' + escape(item.value) + ';\">'
                     + escape(item.label) + '</div>';
            }
          }
        ")
                        )
                      )
                    )
                  ),
                  tags$br(),
                  
                  # Font size and toggle to show/hide descriptor labels
                  fluidRow(
                    column(
                      width = 6,
                      h5(
                        "Font Size for Descriptors",
                        style = "color: #007436; font-size: 16px; font-weight: bold;"
                      ),
                      p(
                        "Adjust the size of the descriptor text in the plot.",
                        style = "font-size: 14px; font-weight: bold;"
                      ),
                      numericInput(
                        inputId = "font_size_descriptor",
                        label   = NULL,
                        value   = 6,
                        min     = 4,
                        max     = 20,
                        step    = 1
                      )
                    ),
                    column(
                      width = 6,
                      h5(
                        "Show Descriptor Labels",
                        style = "color: #007436; font-size: 16px; font-weight: bold;"
                      ),
                      p(
                        "Toggle whether the descriptor names are displayed on the plot.",
                        style = "font-size: 14px; font-weight: bold;"
                      ),
                      selectInput(
                        inputId  = "show_descriptor_labels",
                        label    = NULL,
                        choices  = c("Show" = "show", "Hide" = "hide"),
                        selected = "show"
                      )
                    )
                  ),
                  tags$br(),
                  
                  # Horizontal shift controls for plot and labels
                  fluidRow(
                    column(
                      width = 6,
                      h5(
                        "Horizontal Plot Shift",
                        style = "color: #007436; font-size: 16px; font-weight: bold;"
                      ),
                      p(
                        "Shift the entire circular plot left/right.",
                        style = "font-size: 14px; font-weight: bold;"
                      ),
                      numericInput(
                        inputId = "plot_horizontal_shift",
                        label   = NULL,
                        value   = 0,
                        min     = -100,
                        max     = 100,
                        step    = 5
                      )
                    ),
                    column(
                      width = 6,
                      h5(
                        "Horizontal Label Shift",
                        style = "color: #007436; font-size: 16px; font-weight: bold;"
                      ),
                      p(
                        "Shift the descriptor labels left/right.",
                        style = "font-size: 14px; font-weight: bold;"
                      ),
                      numericInput(
                        inputId = "label_horizontal_shift",
                        label   = NULL,
                        value   = 12,
                        min     = -50,
                        max     = 50,
                        step    = 10
                      )
                    )
                  )
                ),
                
                # ── Column 3: Axis Options ───────────────────────────────────
                tags$div(
                  class = "options-container",
                  style = "flex: 1; padding: 15px; background-color: rgba(240, 245, 240, 0.9); border: 1px solid #007436; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); height: 700px;",
                  
                  h5(
                    "Axis Options",
                    style = "color: #007436; font-size: 18px; font-weight: bold; margin-bottom: 15px; text-align: center;"
                  ),
                  
                  # Select which intensity axes to display
                  h5(
                    "Select Axes to Display",
                    style = "color: #007436; font-size: 16px; font-weight: bold;"
                  ),
                  p(
                    "Choose which intensity axes to display in the plot.",
                    style = "font-size: 14px; font-weight: bold;"
                  ),
                  uiOutput("select_puissance_axes_ui"),
                  tags$br(),
                  
                  # Axis customization UI
                  h5(
                    "Axis Customization",
                    style = "color: #007436; font-size: 16px; font-weight: bold;"
                  ),
                  p(
                    "Customize the appearance and label for each axis.",
                    style = "font-size: 14px; font-weight: bold;"
                  ),
                  uiOutput("custom_axis_ui")
                )
              )
          )
      ),
      
      # ── Download Results Container ────────────────────────────────────────
      div(class = "container-frame3",
          
          # Title bar for download options
          div(class = "title-bar",
              h4(
                tags$strong("Download Results"),
                style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
              )
          ),
          
          # Content area: instructions and download controls
          div(class = "content",
              
              # Instructional text for downloading table and plot
              div(
                class = "file-info-text",
                style = "text-align: left; margin-top: 10px;",
                HTML(
                  "You can download the <strong>Results Table</strong> and the <strong>Circle Plot</strong> below."
                )
              ),
              tags$br(),
              
              # Fluid row dividing into two columns: table export and plot export
              fluidRow(
                
                # ── Column for Exporting Filtered Table ──────────────────────
                column(
                  width = 5,
                  h5(
                    tags$strong("Export Filtered Table"),
                    style = "font-size: 15px; color: #007436;"
                  ),
                  
                  div(
                    class = "file-info-text",
                    style = "text-align: left; margin-top: 5px; margin-bottom: 10px;",
                    HTML(
                      "You can export <strong>filtered results tables</strong> for one or multiple products. Choose the desired <strong>products</strong>, <strong>columns</strong>, and <strong>format</strong> before downloading."
                    )
                  ),
                  tags$br(),
                  # UI to select which columns to include in the export
                  uiOutput("export_column_selector"),
                  tags$br(),
                  # Format selection dropdown for table export
                  selectInput(
                    inputId = "export_table_format",
                    label   = "Select export format:",
                    choices = c("Excel (.xlsx)", "CSV (.csv)", "PNG (.png)", "PDF (.pdf)")
                  ),
                  # Download button for filtered table
                  downloadButton(
                    "export_download_table",
                    "Download Table",
                    class = "download-button"
                  )
                ),
                
                # Vertical divider between table and plot export
                column(
                  width = 1,
                  class = "p-0",
                  div(
                    style = "width:1px; height:100%; background:#222; margin:0 auto;"
                  )
                ),
                
                # ── Column for Exporting Circle Plot ─────────────────────────
                column(
                  width = 5,
                  h5(
                    tags$strong("Export Circle Plot"),
                    style = "font-size: 15px; color: #007436;"
                  ),
                  
                  # Introductory text for plot export options
                  div(
                    class = "file-info-text",
                    style = "text-align: left; margin-top: 5px; margin-bottom: 10px;",
                    HTML(
                      "Download the current <strong>visualization</strong> with options to customise your plot. <strong>PNG</strong> is recommended for on-screen use and <strong>PDF/SVG</strong> for print or vector editing."
                    )
                  ),
                  
                  # Checkbox group to select which products to include in export
                  div(
                    tags$strong("Select product(s) to export:"),
                    checkboxGroupInput(
                      inputId  = "plot_export_products",
                      label    = NULL,
                      choices  = NULL,  # Filled dynamically by server
                      selected = NULL,
                      inline   = TRUE
                    )
                  ),
                  
                  # ── Row 1: Option to add intensity axes ────────────────────
                  fluidRow(
                    column(
                      width = 6,
                      strong("Add intensity axes ?"),
                      radioButtons(
                        inputId  = "plot_show_axes",
                        label    = NULL,
                        choices  = c("Yes" = "yes", "No" = "no"),
                        selected = "no",
                        inline   = FALSE
                      )
                    ),
                    column(
                      width = 6,
                      # Conditional UI appears only if "Yes" is selected
                      conditionalPanel(
                        condition = "input.plot_show_axes == 'yes'",
                        uiOutput("export_axes_selector")
                      )
                    )
                  ),
                  
                  # ── Row 2: Export format selection for plot ───────────────
                  selectInput(
                    inputId  = "plot_export_format",
                    label    = "Select export format:",
                    choices  = c("PNG (.png)", "PDF (.pdf)", "SVG (.svg)"),
                    selected = "PNG (.png)"
                  ),
                  
                  # ── Row 3: Download button for circle plot ────────────────
                  downloadButton(
                    "download_plot",
                    "Download Plot",
                    class = "download-button"
                  )
                )
              ),
              
              # Horizontal rule separating table/plot export from full report
              tags$hr(style = "border-color: #ccc; margin: 20px 0;"),
              
              # Heading for full PDF report export
              h5(
                tags$strong("Export Full PDF Report"),
                style = "font-size:15px; color:#007436;"
              ),
              div(
                class = "file-info-text",
                style = "text-align: left; margin-bottom: 10px;",
                HTML(
                  "Generate a <strong>PDF report</strong> for each selected product, including its <strong>Circle Plot</strong> and <strong>Results Table</strong>, with all your current settings."
                )
              ),
              # Centered download button with a PDF icon
              div(
                style = "text-align: center; margin-bottom: 10px;",
                downloadButton(
                  outputId = "download_full_report",
                  label    = tagList(icon("file-pdf"), "Download Full PDF Report"),
                  class    = "download-button"
                )
              )
          )
      ),
      
      # ── Additional Feedback Container ────────────────────────────────────
      div(
        class = "container-frame3",
        
        # Title bar for additional feedback
        div(class = "title-bar",
            h4(
              tags$strong("Additional Feedback"),
              style = "margin-bottom: 3px; font-size: 16px; color: #007436;"
            )
        ),
        
        # Content area: instructions and dynamic feedback list
        div(class = "content",
            div(
              class = "file-info-text",
              style = "text-align: left; margin-top: 10px;",
              HTML("You can find <strong>additional feedbacks</strong> for each product below.")
            ),
            div(
              class = "content",
              style = "padding:12px;",
              uiOutput("feedback_list")
            )
        )
      )
    ),
    
    # ─────────────────────────────────────────────────────────────────────────
    # Tab 3: STATISTICAL ANALYSIS (empty placeholder)
    # ─────────────────────────────────────────────────────────────────────────
    tabPanel(
      title = "STATISTICAL ANALYSIS",
      value = "statistical_analysis"
    ),
    
    # ─────────────────────────────────────────────────────────────────────────
    # Tab 4: AUTOMATIC REPORT (empty placeholder)
    # ─────────────────────────────────────────────────────────────────────────
    tabPanel(
      title = "AUTOMATIC REPORT",
      value = "automatic_report"
    )
  )
)
