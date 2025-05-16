
# Définir l'interface utilisateur
ui <- fluidPage(
  useShinyjs(),  # Initialiser shinyjs
  
  # Ajouter du CSS personnalisé et charger la bibliothèque d'icônes Font Awesome
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto&family=Open+Sans&family=Lato&family=Montserrat&family=Raleway&display=swap"),
    tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.15.3/css/all.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  
  # La bande en haut avec le logo, le titre, et les sections de navigation
  div(class = "header",
      img(src = "https://www.mane.com/theme/images/logo.png", alt = "Logo", style = "height: 70px; width: 70px;"), 
      div(class = "title", "QFD Data Analysis"),
      div(class = "nav-section", id = "nav_data_importation",  # Ajout de l'id
          tags$i(class = "fas fa-upload"), 
          "DATA IMPORTATION"),
      div(class = "nav-section", id = "nav_data_visualization",  # Ajout de l'id
          tags$i(class = "fas fa-chart-bar"), 
          "DATA VISUALIZATION"),
      div(class = "nav-section", id = "nav_statistical_analysis",  # Ajout de l'id
          tags$i(class = "fas fa-calculator"), 
          "STATISTICAL ANALYSIS"),
      div(class = "nav-section", id = "nav_automatic_report",  # Ajout de l'id
          tags$i(class = "fas fa-file-alt"), 
          "AUTOMATIC REPORT")
  ),
  
  # Encapsuler les onglets dans un tabsetPanel de type 'hidden'
  tabsetPanel(id = "main_tabs", type = "hidden",
              
              tabPanel(title = "DATA IMPORTATION", value = "data_importation",
                       # Votre contenu pour l'onglet DATA IMPORTATION
                       div(class = "intro-text", 
                           tags$strong(tags$em("The QFD Data Analysis application lets you import QFD data from Qualtrics or FIZZ applications. ")),
                           "You can check for calculation errors, generate rainbow graphs, customize colours, perform statistical analysis, export visuals, and much more. ",
                           "If you have any questions about the application, please contact me at my email address: ",
                           tags$a(href = "mailto:Cassandre.MIGLIORE@mane.com", "Cassandre.MIGLIORE@mane.com"), "."
                       ),
                       
                       # ... Le reste de votre code pour cet onglet ...
                       # Conteneur principal avec encadrement foncé
                       div(class = "container-frame2",
                           
                           # Bande plus foncée en haut du conteneur
                           div(class = "title-bar",
                               "Select and Upload Your Excel File"  # Texte du titre à l'intérieur de la bande
                           ),
                           
                           # Contenu du conteneur avec espacement sous la bande
                           div(class = "content",
                               
                               # Conteneur pour la sélection de la source et le carré vert
                               div(style = "display: flex; align-items: center; justify-content: flex-start;",  # Aligne tout à gauche
                                   div(class = "select-source",
                                       selectInput("source", "Select Source:", choices = c("Fizz", "Qualtrics"))
                                   ),
                                   div(class = "green-box",
                                       tags$div(style = "position: relative; display: flex; align-items: center; justify-content: center;",
                                                tags$input(type = "file", id = "file", class = "file-input", accept = ".xlsx"),
                                                tags$button(id = "upload_button", class = "upload-button",
                                                            tags$i(class = "fas fa-upload"),
                                                            "Upload File"
                                                ),
                                                div(class = "file-name-container",
                                                    textOutput("file_name") # Afficher le nom du fichier sélectionné
                                                )
                                       )
                                   ),
                                   div(class = "load-button-container", 
                                       div(class = "load-button-wrapper",
                                           actionButton("load_button", HTML('<strong>Load Data</strong>'), class = "load-button")
                                       )
                                   )
                               ),
                               
                               # Conteneur pour le message de fichier non sélectionné
                               div(id = "file_status", class = "alert-text", textOutput("no_file_message"))
                           )
                       ),
                       
                       
                       div(class = "container-frame",
                           style = "position: relative;",  # Ajout de position: relative pour le positionnement de la bande
                           # Bande foncée en haut du conteneur
                           div(class = "title-bar",
                               h4(tags$strong("Rename Descriptors"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                           ),
                           
                           div(
                             style = "margin-bottom: 5px;",  # Réduire la marge inférieure
                             tags$br(),
                             uiOutput("file_info_text")  # Affichage du message en fonction du fichier chargé
                           ),
                           
                           # Contenu du conteneur avec un padding ajusté pour tenir compte de la bande
                           div(class = "content",
                               uiOutput("descriptors_ui"),
                           )
                       ),
                       div(class = "container-frame3",
                           style = "max-height: 600px; height: auto; position: relative; margin-bottom:10px;",  # Ajout de position: relative pour le positionnement de la bande
                           # Bande foncée en haut du conteneur
                           div(class = "title-bar",
                               h4(tags$strong("Assign Colors to Descriptors"), style = "margin-bottom: 10px; font-size: 16px; color: #007436;")
                           ),
                           # Contenu du conteneur avec un padding ajusté pour tenir compte de la bande
                           # Contenu du conteneur avec un padding ajusté pour tenir compte de la bande
                           div(class = "content",
                               # Ajout de l'instruction dans un encadré conditionné au chargement du fichier
                               uiOutput("instructionText"),  # L'encadré n'apparaît que si un fichier est chargé
                               tags$br(),
                               uiOutput("descriptor_color_ui")  # Ajout des boutons de couleur
                           )
                       ),
                       
                       div(class = "container-frame",
                           style = "max-height: none; height: auto; position: relative;margin-bottom:10px;",  # Ajustement de la hauteur
                           # Bande foncée en haut du conteneur
                           div(class = "title-bar",
                               h4(tags$strong("Rename Products"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                           ),
                           div(
                             style="margin-bottom : 5px;",#Réduire la marge inférieure 
                             tags$br(),
                             uiOutput("file_info_text2")
                           ),
                           # Contenu du conteneur pour renommer les produits
                           div(class = "content",
                               uiOutput("productRenameUI"),  # UI dynamique pour renommer les produits
                           )
                       ),
                       
                       div(class = "container-frame",
                           style = "max-height: none; height: auto; position: relative; margin-bottom:10px;",
                           # Bande foncée en haut du conteneur
                           div(class = "title-bar",
                               h4(tags$strong("Associate Images with Products"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                           ),
                           div(
                             style="margin-bottom : 5px;",#Réduire la marge inférieure 
                             tags$br(),
                             uiOutput("file_info_text3")
                           ),
                           # Contenu du conteneur pour le chargement des images
                           div(class = "content",
                               uiOutput("productImageUploadUI")
                           )
                       )
                       
              ),
              
              tabPanel(title = "DATA VISUALIZATION", value = "data_visualization",
                       # Info text
                       div(
                         class = "intro-text",
                         HTML("Welcome to the <strong>Data Visualization</strong> tab. In this section, you can <strong>generate graphs and tables</strong> that <strong>rank descriptors</strong> based on <strong>various options</strong>. To begin, please <strong>select a product</strong> from the <strong>dropdown menu below</strong>.")
                       ),
                       
                       div(
                         class = "container-frame3",  # Use an appropriate container
                         # Dark band at the top of the container
                         div(class = "title-bar",
                             h4(tags$strong("Select a Product"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                         ),
                         # Container content
                         div(class = "content",
                             uiOutput("productSelectionUI")
                         )
                       ),
                       # Dynamic text based on selected product
                       uiOutput("product_info_text"),
                       
                       # Wrap the content in a styled container
                       div(
                         class = "container-frame3 dataviz-container",
                         # Title bar
                         div(class = "title-bar",
                             h4(tags$strong("Results Table and Plot"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                         ),
                         
                         # Container content with file_info_text4
                         div(
                           class = "content",
                           style = "width: 100%;",
                           # Specific information text with numericInput included in the green box
                           uiOutput("file_info_text4"),
                           
                           # Organization in three columns
                           tags$div(
                             style = "display: flex; gap: 10px; align-items: stretch; margin-top: 20px;",
                             
                             # Second column: Results Table
                             tags$div(
                               class = "column",
                               style = "flex: 1; display: flex; flex-direction: column;",
                               
                               # Dark green band with white title and a bottom margin of 10px
                               div(
                                 class = "plot-result-table-header",
                                 "Result Table",
                                 style = "margin-bottom: 10px;"
                               ),
                               
                               DT::dataTableOutput("results_table")
                             ),
                             
                             # Third column: Circle Plot
                             tags$div(
                               class = "column",
                               style = "flex: 1; display: flex; flex-direction: column;",
                               
                               # Dark green band with white title and a bottom margin of 10px
                               div(
                                 class = "circle-plot-header",
                                 "Circle Plot",
                                 style = "margin-bottom: 10px;"
                               ),
                               
                               # Title for the plot
                               plotOutput("circle_plot", height = "400px"),
                               
                               actionButton("refresh_plot", "Update Visualization", icon = icon("sync")),

                             )
                           )
                         )
                       ),
                       
                       div(
                         class = "container-frame3",
                         # Dark band at the top of the container
                         div(class = "title-bar",
                             h4(tags$strong("Plot Options"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                         ),
                         div(
                           class = "content",
                           # English text explaining the download options
                           div(
                             class = "file-info-text",
                             style = "text-align: left; margin-top: 10px;",
                             HTML("Below, you will find all the parameters available to <strong>personalize your plot</strong>. Modify these settings to adjust<strong> the appearance and functionality</strong> of your visualization according to your needs.")
                           ),
                           tags$br(),
                           tags$div(
                             style = "display: flex; gap: 20px; align-items: flex-start;",
                             
                             tags$div(
                               class="options-container",
                               style = "flex: 1; padding: 15px; background-color: rgba(240, 245, 240, 0.9); border: 1px solid #007436; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);height: 700px;",
                               h5("Descriptor Options", style = "color: #007436; font-size: 18px; font-weight: bold; margin-bottom: 15px; text-align: center;"),
                               h5("Number of Descriptors", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                               p("Select the number of descriptors to display in the plot.", style="font-weight: bold; font-size: 14px;"),
                               sliderInput(
                                 inputId = "num_descriptors",
                                 label = NULL,
                                 min = 2,
                                 max = 5,
                                 value = 3,
                                 step = 1,
                                 ticks = FALSE,
                                 animate = animationOptions(interval = 100, loop = FALSE)
                               ),
                               tags$br(),
                               h5("Equal Descriptors Handling", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                               p("Choose whether to represent cases where descriptors have equal scores.", style="font-weight: bold; font-size: 14px;"),
                               radioButtons(
                                 inputId = "equalDescriptors",
                                 label = NULL,
                                 choices = c("Yes" = "yes", "No" = "no"),
                                 selected = "no",
                                 inline = TRUE
                               ),
                               tags$br(),
                               h5("First Descriptor Dominance", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                               p("Select to represent cases where the first descriptor's score is twice as high as the second.", style="font-weight: bold; font-size: 14px;"),
                               radioButtons(
                                 inputId = "firstDoubleSecond",
                                 label = NULL,
                                 choices = c("Yes" = "yes", "No" = "no"),
                                 selected = "no",
                                 inline = TRUE
                               )
                             ),
                             # Second column
                             tags$div(
                               class = "options-container",
                               style = "flex: 1; padding: 15px; background-color: rgba(240, 245, 240, 0.9); border: 1px solid #007436; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);height: 700px;",
                               h5("Style Options", style = "color: #007436; font-size: 18px; font-weight: bold; margin-bottom: 15px; text-align: center;"),
                               
                               
                               fluidRow(
                                 column(
                                   width = 6,
                                   h5("Font for Descriptors",
                                      style = "color: #007436; font-size: 16px; font-weight: bold;"),
                                   p("Choose the font style for the descriptor names in the plot.",
                                     style = "font-size: 14px; font-weight: bold;"),
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
                                   h5("Color for Descriptors",
                                      style = "color: #007436; font-size: 16px; font-weight: bold;"),
                                   p("Choose the text color for the descriptor names in the plot.",
                                     style = "font-size: 14px; font-weight: bold;"),
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
                               fluidRow(
                                 # Column for font size
                                   column(
                                     width = 6,
                                     h5("Font Size for Descriptors",
                                        style = "color: #007436; font-size: 16px; font-weight: bold;"),
                                     p("Adjust the size of the descriptor text in the plot.",
                                        style = "font-size: 14px; font-weight: bold;"),
                                     numericInput(
                                        inputId = "font_size_descriptor",
                                        label   = NULL,
                                        value   = 6,
                                        min     = 4,
                                        max     = 20,
                                        step    = 1
                                       )
                                    ),
                                 
                                  # Column for show/hide toggle
                                    column(
                                      width = 6,
                                      h5("Show Descriptor Labels",
                                         style = "color: #007436; font-size: 16px; font-weight: bold;"),
                                      p("Toggle whether the descriptor names are displayed on the plot.",
                                         style = "font-size: 14px; font-weight: bold;"),
                                      selectInput(
                                         inputId  = "show_descriptor_labels",
                                         label    = NULL,
                                         choices  = c("Show" = "show", "Hide" = "hide"),
                                         selected = "show"
                                        )
                                      )
                                   ),
                               tags$br(),
                               fluidRow(
                                 column(
                                   width = 6,
                                   h5("Horizontal Plot Shift", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                                   p("Shift the entire circular plot left/right.", style = "font-size: 14px; font-weight: bold;"),
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
                                   h5("Horizontal Label Shift", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                                   p("Shift the descriptor labels left/right.", style = "font-size: 14px; font-weight: bold;"),
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
                             
                             # Third column
                             tags$div(
                               class = "options-container",
                               style = "flex: 1; padding: 15px; background-color: rgba(240, 245, 240, 0.9); border: 1px solid #007436; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); height: 700px;",
                               
                               h5("Axis Options", style = "color: #007436; font-size: 18px; font-weight: bold; margin-bottom: 15px; text-align: center;"),
                               
                               h5("Select Axes to Display", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                               p("Choose which intensity axes to display in the plot.", style = "font-size: 14px; font-weight: bold;"),
                               uiOutput("select_puissance_axes_ui"), 
                               
                               tags$br(),
                               h5("Axis Customization", style = "color: #007436; font-size: 16px; font-weight: bold;"),
                               p("Customize the appearance and label for each axis.", style = "font-size: 14px; font-weight: bold;"),
                               
                               uiOutput("custom_axis_ui")  ,
                             )
                             
                           )
                         )
                       ),
                       
                       # New container for downloading results
                       div(
                         class = "container-frame3",
                         # Dark band at the top of the container
                         div(class = "title-bar",
                             h4(tags$strong("Download Results"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                         ),
                         
                         # Container content
                         div(
                           class = "content",
                           # English text explaining the download options
                           div(
                             class = "file-info-text",
                             style = "text-align: left; margin-top: 10px;",
                             HTML("You can download the <strong>Results Table</strong> and the <strong>Circle Plot</strong> below.")
                           ),
                           tags$br(),
                           
                           # Deux colonnes : TABLE / PLOT
                           fluidRow(
                             # Colonne pour le TABLEAU
                             column(
                               width = 5,
                               h5(tags$strong("Export Filtered Table"), style = "font-size: 15px; color: #007436;"),
                               
                               div(
                                 class = "file-info-text",
                                 style = "text-align: left; margin-top: 5px; margin-bottom: 10px;",
                                 HTML("You can export <strong>filtered results tables</strong> for one or multiple products. Choose the desired <strong>products</strong>, <strong>columns</strong>, and <strong>format</strong> before downloading.")
                               ),
                               tags$br(),
                               uiOutput("export_column_selector"),
                               tags$br(),
                               selectInput(
                                 inputId = "export_table_format",
                                 label = "Select export format:",
                                 choices = c("Excel (.xlsx)", "CSV (.csv)", "PNG (.png)", "PDF (.pdf)")
                               ),
                               
                               downloadButton("export_download_table", "Download Filtered Table", class = "download-button")
                             ),
                             
                             # Ligne noire verticale entre les deux colonnes
                             column(
                               width = 1,                                # ← 8tu peux descendre à 0.5 si tu veux
                               class = "p-0",                            # bootstrapadding‑left & right = 0
                               div(style = "width:1px; height:100%; background:#222; margin:0 auto;")
                             ),
                             
                             
                             # ------------------------------------------------------------------
                             # Colonne : Export Circle Plot      (remplace l’ancien contenu)
                             # ------------------------------------------------------------------
                             column(
                               width = 5,
                               h5(tags$strong("Export Circle Plot"),
                                  style = "font-size: 15px; color: #007436;"),
                               
                               # — texte introductif —
                               div(
                                 class  = "file-info-text",
                                 style  = "text-align: left; margin-top: 5px; margin-bottom: 10px;",
                                 HTML(
                                   "Download the current <strong>visualization</strong> with options to customise
       size, resolution, and background. <strong>PNG</strong> is recommended for direct
       use, <strong>PDF/SVG</strong> for print or vector editing, and <strong>.RDS</strong>
       for further changes in R."
                                 )
                               ),
                               
                               div(
                                 tags$strong("Select product(s) to export:"),
                                 checkboxGroupInput(
                                   inputId  = "plot_export_products",
                                   label    = NULL,
                                   choices  = NULL,        # ← rempli dynamiquement côté serveur
                                   selected = NULL,
                                   inline   = TRUE
                                 )
                               ),
                               
                               # ======= 1) Ligne : produits à exporter  +  choix du fond =======
                               fluidRow(
                                 # -- Choix du fond (exclusif) ----------------------------------
                                 column(
                                   width = 4,
                                   strong("Background"),
                                   radioButtons(
                                     inputId  = "png_bg_choice",
                                     label    = NULL,
                                     choices  = c("None" = "transparent",
                                                  "White" = "white"),
                                     selected = "white",
                                     inline   = FALSE
                                   )
                                   ),
                                   
                                   column(
                                     width = 4,
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
                                     width = 4,
                                     conditionalPanel(
                                       condition = "input.plot_show_axes == 'yes'",
                                       uiOutput("export_axes_selector")
                                     )
                                   )
                               ),
                               
                               
                             # ======= 3) Ligne : format d’export =============================
                               selectInput(
                                 inputId  = "plot_export_format",
                                 label    = "Select export format:",
                                 choices  = c("PNG (.png)",
                                              "PDF (.pdf)",
                                              "SVG (.svg)",
                                              "RDS (.rds)"),
                                 selected = "PNG (.png)"
                               ),
                               
                               # ======= 4) Bouton de téléchargement ============================
                               downloadButton("download_plot", "Download Plot",
                                              class = "download-button")
                             )
                           )
                         )
                       ),
                       # New container for downloading results
                       div(
                         class = "container-frame3",
                         # Dark band at the top of the container
                         div(class = "title-bar",
                             h4(tags$strong("Additional comments"), style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                         ),
                         # Container content
                         div(
                           class = "content",
                           # English text explaining the download options
                           div(
                             class = "file-info-text",
                             style = "text-align: left; margin-top: 10px;",
                             HTML("You can find <strong>additional comments </strong> for each product below.")
                           ),
                           div(
                             id="comments-container",
                             uiOutput("comment_list")
                           )
                         ))
              ),
              
              tabPanel(
                title = "STATISTICAL ANALYSIS", 
                value = "statistical_analysis",
                
                # 1) Petit texte d'introduction
                div(
                  class = "intro-text",
                  HTML("
    <strong>Welcome to the Statistical Analysis tab.</strong> 
    Here, you can <strong> run PCA with product-descriptor biplots, correlation matrices, 
    and clustering. </strong>  
    <br><em>Click <strong>‘Run Analysis’ </strong>  to generate and visualize the results.</em>
    ")
                ),
                
                div(
                  class = "container-frame3",  # ou container-frame, selon ta préférence de style
                  # Bande plus foncée (title-bar)
                  div(
                    class = "title-bar",
                    h4("Data Subset Selection", style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                  ),
                  
                  div(
                    class = "content",
                    
                    # Encadré type .file-info-text
                    div(
                      class = "file-info-text",
                      style = "margin-bottom: 10px;",
                      HTML("
        <strong>In this section, you can select which products and descriptors you wish to include in further analyses.</strong>
        <br>
        You can also choose whether to center and scale (standardize) the data before running PCA, correlation, or clustering.
        <br>
        If some descriptors are not relevant (e.g., low correlation, high p-value), you'll see an alert suggesting you remove them.
      ")
                    ),
                    
                    # Petit message au-dessus de la sélection des produits
                    div(
                      style = "font-weight: bold; margin-top: 10px; margin-bottom: 5px; color: #005F30;",
                      "Please select the products you want to include in the analysis:"
                    ),
                    
                    # Sélection des produits (coches groupées), avec un style pour un look professionnel
                    div(
                      class = "descriptor-checkbox-group",  # Classe CSS pour le style
                      uiOutput("product_selector_ui")
                    ),
                    
                    
                    # Phrase descriptive avant la sélection des descripteurs
                    div(
                      style = "font-weight: bold; margin-top : 10px; margin-bottom: 5px; color: #005F30;",
                      "Please select the descriptors you want to include in the analysis:"
                    ),
                    div(
                      class = "descriptor-checkbox-group",
                      uiOutput("descriptor_selector_ui")
                    ),
                    
                    div(
                      class = "",  # Pour garder un style cohérent
                      style = "margin-top: 15px; font-weight: bold; margin-bottom: 5px; color: #005F30;",
                      HTML("<strong>Would you like the data to be center-reduced (scaled)?</strong>"),
                      
                      radioButtons(
                        inputId = "center_scale_choice",
                        label = NULL,  # pas de label supplémentaire, on a déjà le texte au-dessus
                        choices = c("Yes" = "yes", "No" = "no"),
                        selected = "yes",  # 'Yes' coché par défaut
                        inline = TRUE       # afficher en ligne
                      )
                    )
                  )
                ),
                div(
                  class = "container-frame3",
                  div(
                    class = "title-bar",
                    h4("Principal Component Analysis (PCA)", 
                       style = "margin-bottom: 3px; font-size: 16px; color: #007436;")
                  ),
                  div(
                    class = "content",
                    
                    # 1) GRAND MESSAGE, SUR TOUTE LA LARGEUR
                    div(
                      class = "file-info-text",
                      style = "width: 100%; margin-bottom: 10px;",  # 100% pour prendre toute la largeur
                      HTML("
        <strong>In this section, you can run an advanced PCA</strong> 
        with various options: 
        choose how many principal components to keep, 
        whether to display a scree plot, 
        and set up clustering on the resulting PCA space. 
        You can also define the cluster color, opacity, fill style, etc. 
        <br><em>Please configure the options on the left, 
        then click <strong>Run PCA</strong> to visualize the results on the right.</em>
      ")
                    ),
                    
                    # 2) DEUX COLONNES : A GAUCHE (OPTIONS), A DROITE (PLOT)
                    fluidRow(
                      # --- COLONNE GAUCHE (OPTIONS)
                      column(
                        width = 4,
                        
                        # Bloc pour les options purement ACP
                        div(
                          class = "file-info-text",
                          style = "margin-bottom: 10px;",
                          h5("PCA Options", style="font-weight:bold; color:#005F30;"),
                          
                          radioButtons(
                            inputId = "npc_method",
                            label = "Number of principal components:",
                            choices = c("Auto (Kaiser > 1)" = "kaiser", 
                                        "Auto (>=80% variance)" = "variance80",
                                        "Manual" = "manual"),
                            selected = "kaiser",  # par défaut on fait Kaiser
                            inline = TRUE
                          ),
                          
                          
                          # Afficher un scree plot ?
                          checkboxInput(
                            inputId = "show_scree",
                            label = "Display Scree Plot?",
                            value = FALSE
                          ),
                          
                          # Afficher un cercle de corrélation ?
                          checkboxInput(
                            inputId = "show_corr_circle",
                            label = "Show Correlation Circle?",
                            value = FALSE
                          ),
                          
                          # Afficher un biplot ?
                          checkboxInput(
                            inputId = "show_biplot",
                            label = "Show Biplot (Products + Descriptors)?",
                            value = TRUE
                          )
                        ),
                        
                        # Bloc pour l’option d’affichage des produits (images, noms, etc.)
                        div(
                          class = "file-info-text",
                          style = "margin-bottom: 10px;",
                          h5("Product Display", style="font-weight:bold; color:#005F30;"),
                          radioButtons(
                            inputId = "pca_display_option",
                            label = "How to represent products on the plot?",
                            choices = c("Names Only" = "names", 
                                        "Images Only" = "images", 
                                        "Names + Images" = "both"),
                            selected = "names"
                          )
                        ),
                        
                        # Bloc pour les options de Clustering
                        div(
                          class = "file-info-text",
                          style = "margin-bottom: 10px;",
                          h5("Clustering Options", style="font-weight:bold; color:#005F30;"),
                          
                          # Afficher les clusters ou pas
                          checkboxInput(
                            inputId = "enable_clusters",
                            label = "Display Clusters?",
                            value = FALSE
                          ),
                          
                          # Choisir nombre de cluster ou auto
                          radioButtons(
                            inputId = "cluster_choice",
                            label = "Number of Clusters:",
                            choices = c("Fixed k" = "fixed", "Auto" = "auto"),
                            selected = "fixed",
                            inline = TRUE
                          ),
                          
                          # Valeur de k
                          numericInput(
                            inputId = "num_clusters",
                            label = "k (if Fixed):",
                            value = 3, 
                            min = 2,
                            step = 1
                          ),
                          
                          # ICI on ajoute un uiOutput dynamique pour chaque cluster
                          uiOutput("cluster_colors_ui"),
                          
                          # Opacité
                          sliderInput(
                            inputId = "cluster_opacity",
                            label = "Cluster Opacity (alpha):",
                            min = 0, max = 1, 
                            value = 0.5, 
                            step = 0.05
                          ),
                          
                          # Remplissage ou pas
                          checkboxInput(
                            inputId = "fill_clusters",
                            label = "Fill Clusters?",
                            value = TRUE
                          )
                        ),
                        
                        # Bouton final
                        actionButton(
                          inputId = "run_pca",
                          label = "Run PCA",
                          class = "save-button", 
                          style = "margin-top: 10px;"
                        )
                      ),
                      
                      # --- COLONNE DROITE (PLOT)
                      column(
                        width = 8,
                        # Plot output pour afficher le biplot ACP + clusters
                        plotOutput("pca_plot", height = "500px")
                      )
                    )
                  )
                )
              ),
              tabPanel(title = "AUTOMATIC REPORT", value = "automatic_report",
                       # Votre contenu pour l'onglet AUTOMATIC REPORT
                       h2("Contenu du rapport automatique")
              )
  )
  
)