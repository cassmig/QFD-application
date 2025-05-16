# 📌 global.R - Chargement des packages et variables globales

# Installer les packages si absents
list.of.packages <- c(
  "shiny", "shinydashboard", "tidyverse", "readxl", "DT", "shinyjs", 
  "plotly", "openxlsx", "base64enc", "gridExtra", "officer", 
  "chromote", "webshot2", "htmlwidgets", "showtext", "shinyWidgets",
  "scales", "grid", "colourpicker","Cairo", "ggforce", "ragg")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Charger les packages
lapply(list.of.packages, library, character.only = TRUE)

# Options globales
options(shiny.maxRequestSize = 50*1024^2) # Limite upload fichier à 50MB

# à mettre dans global.R ou au début de votre module
windowsFonts(
  Arial             = windowsFont("Arial"),
  Calibri           = windowsFont("Calibri"),
  `Times New Roman` = windowsFont("Times New Roman"),
  Verdana           = windowsFont("Verdana"),
  Georgia           = windowsFont("Georgia"),
  Tahoma            = windowsFont("Tahoma"),
  `Trebuchet MS`    = windowsFont("Trebuchet MS"),
  `Century Gothic`  = windowsFont("Century Gothic"),
  `Lucida Sans Unicode` = windowsFont("Lucida Sans Unicode"),
  `Segoe UI`        = windowsFont("Segoe UI")
)

