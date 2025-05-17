# QFD-App


> **QFD-App** is an interactive Shiny dashboard that automates the post-processing of QFM sensory tests exported from **Qualtrics™** or **Fizz™**.  
> The app ranks descriptors, normalises results to a standard panel size, assigns colours, allows renaming, generates **circle plots**, summarises comments, and exports results in several formats.  
> Upcoming releases will add advanced statistics and automatic report generation.

---

## Table of Contents
1. [Features](#features)  
2. [Prerequisites](#prerequisites)  
3. [Installation](#installation)  
4. [Usage](#usage)  
5. [Project Structure](#project-structure)  
6. [Roadmap](#roadmap)  
7. [Contributing](#contributing)  
8. [License](#license)

---

## Features

- **Automatic descriptor ranking** from raw QFM tables  
- **Panel size normalisation** to a fixed number of panellists  
- **Colour assignment & renaming** of descriptors on the fly  
- Publication-ready **circle plots** (interactive and static)  
- **Comment synthesis** for quick qualitative insights  
- **Multi-format export** (Excel, PowerPoint, HTML widgets, images)  
- Built-in **module system** for future statistical analysis and automated reporting (coming soon)

---

## Prerequisites

| Requirement | Version / Notes                                  |
|-------------|--------------------------------------------------|
| **R**       | 4.0 or higher                                    |
| **RStudio** | *recommended*                                    |
| **R packages** | `shiny`, `shinydashboard`, `tidyverse`, `readxl`, `DT`, `shinyjs`, `plotly`, `openxlsx`, `base64enc`, `gridExtra`, `officer`, `chromote`, `webshot2`, `htmlwidgets`, `showtext`, `shinyWidgets`, `scales`, `grid`, `colourpicker`, `Cairo`, `ggforce`, `ragg` |

---

## Installation

```bash
# 1. Clone the repository
git clone https://gitlab2-bsl.emea.sesam.mane.com/Cassandre.MIGLIORE/QFD-App.git
cd QFD-App

# 2. Install required R packages
Rscript -e "install.packages(c(
  'shiny','shinydashboard','tidyverse','readxl','DT','shinyjs',
  'plotly','openxlsx','base64enc','gridExtra','officer',
  'chromote','webshot2','htmlwidgets','showtext','shinyWidgets',
  'scales','grid','colourpicker','Cairo','ggforce','ragg'
))"
