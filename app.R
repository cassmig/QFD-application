# Chargement des packages
library(shiny)

# Interface utilisateur (UI)
ui <- fluidPage(
  
  # Lien vers le fichier CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  ),
  
  # Contenu de l'application
  titlePanel("Mon Application QFD"),
  sidebarLayout(
    sidebarPanel(
      actionButton("btn", "Clique-moi !")
    ),
    mainPanel(
      textOutput("texte")
    )
  )
)

# Serveur (Server)
server <- function(input, output) {
  output$texte <- renderText({
    "Bienvenue dans mon application QFD !"
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
