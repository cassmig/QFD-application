# 📌 global.R - Load packages and define global variables

# ── global.R ────────────────────────────────────────────────────────────

# 1) List of packages required for the application
.required_pkgs <- c(
  "shiny",        "shinydashboard", "tidyverse",    "readxl",
  "DT",           "shinyjs",        "plotly",       "openxlsx",
  "base64enc",    "gridExtra",      "officer",      "chromote",
  "webshot2",     "htmlwidgets",    "showtext",     "shinyWidgets",
  "scales",       "colourpicker",   "ggforce",      "ragg", "grid",
  "gridtext", "Cairo", "dplyr", "stringr", "tibble", "tidytext", "stringdist", "tidyr", "glue", "purrr"
)

# 2) Load each package immediately so that their functions are available
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(DT)
library(shinyjs)
library(plotly)
library(openxlsx)
library(base64enc)
library(gridExtra)
library(officer)
library(chromote)
library(webshot2)
library(htmlwidgets)
library(showtext)
library(shinyWidgets)
library(scales)
library(colourpicker)
library(ggforce)
library(ragg)
library(gridtext)
library(Cairo)
library(grid)
library(dplyr)
library(stringr)
library(tibble)
library(tidytext)
library(stringdist)
library(tidyr)
library(glue)
library(purrr)

# 3) Check and load each required package, or stop execution if any are missing
invisible(lapply(.required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Missing package: ‘", pkg,
      "’. Please install it before deploying (install.packages('", pkg, "')).",
      call. = FALSE
    )
  }
  library(pkg, character.only = TRUE)
}))

# 4) Global options
#    Increase maximum file upload size to 50 MB
options(shiny.maxRequestSize = 50 * 1024^2)

# 5) Font configuration for Windows (to avoid errors on Linux)
if (.Platform$OS.type == "windows") {
  # If running on Windows, register common TrueType fonts explicitly
  windowsFonts(
    Arial               = windowsFont("Arial"),
    Calibri             = windowsFont("Calibri"),
    `Times New Roman`   = windowsFont("Times New Roman"),
    Verdana             = windowsFont("Verdana"),
    Georgia             = windowsFont("Georgia"),
    Tahoma              = windowsFont("Tahoma"),
    `Trebuchet MS`      = windowsFont("Trebuchet MS"),
    `Century Gothic`    = windowsFont("Century Gothic"),
    `Lucida Sans Unicode` = windowsFont("Lucida Sans Unicode"),
    `Segoe UI`          = windowsFont("Segoe UI")
  )
} else {
  # ─────────────────────────────────────────────────────────────────────────────
  # On Linux/macOS: attempt to register generic font families
  # (Arial, Verdana, etc.) by mapping them to DejaVu or Liberation fonts
  # if those font files exist on the system.
  # ─────────────────────────────────────────────────────────────────────────────
  
  # Utility function: add a font family if the font file is present
  safe_font_add <- function(family, path_regular, path_bold = NULL, path_italic = NULL, path_bolditalic = NULL) {
    if (file.exists(path_regular)) {
      args <- list(family = family, regular = path_regular)
      if (!is.null(path_bold)       && file.exists(path_bold))       args$bold       <- path_bold
      if (!is.null(path_italic)     && file.exists(path_italic))     args$italic     <- path_italic
      if (!is.null(path_bolditalic) && file.exists(path_bolditalic)) args$bolditalic <- path_bolditalic
      do.call(font_add, args)
    } else {
      message(sprintf("⚠️  Font file not found for '%s': %s", family, path_regular))
    }
  }
  
  # Typical directories for DejaVu and Liberation fonts on RHEL/CentOS/Ubuntu
  dejavu_dir     <- "/usr/share/fonts/dejavu"
  liberation_dir <- "/usr/share/fonts/liberation"
  
  # Map common Windows fonts to DejaVu equivalents
  safe_font_add(
    family       = "Arial",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  safe_font_add(
    family       = "Verdana",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  safe_font_add(
    family       = "Tahoma",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  safe_font_add(
    family       = "Times New Roman",
    path_regular = file.path(dejavu_dir, "DejaVuSerif.ttf")
  )
  safe_font_add(
    family       = "Georgia",
    path_regular = file.path(dejavu_dir, "DejaVuSerif.ttf")
  )
  safe_font_add(
    family       = "Trebuchet MS",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  safe_font_add(
    family       = "Century Gothic",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  safe_font_add(
    family       = "Lucida Sans Unicode",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  safe_font_add(
    family       = "Segoe UI",
    path_regular = file.path(dejavu_dir, "DejaVuSans.ttf")
  )
  # Map Calibri to LiberationSans if available
  safe_font_add(
    family       = "Calibri",
    path_regular = file.path(liberation_dir, "LiberationSans-Regular.ttf")
  )
  
  # Enable the use of these added fonts for plotting via showtext
  showtext_auto()
}
# ───────────────────────────────────────────────────────────────────────── 

# 6) Disable Packrat/renv for RStudio Connect
options(rsconnect.packrat = FALSE)

# 7) Set a stable CRAN mirror (in case dependencies need to be installed at runtime)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# 8) Source UI and server definitions
source("ui.R")
source("server.R")

# 9) Launch the Shiny application
shiny::shinyApp(ui = ui, server = server)
