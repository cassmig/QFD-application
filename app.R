# Charger les fichiers global, ui et server
source("global.R")
source("ui.R")                                                                                                                                                               
source("server.R")
                                                                                                                                                                                                                        
# Lancer l'application Shiny
shinyApp(ui = ui, server = server)


