# 📌 server.R - Load modularized code and define server logic

# ── Load Modularized Code ───────────────────────────────────────────────────

# Each `source()` call imports a script that defines a Shiny module. 
# These modules encapsulate specific pieces of server functionality,
# keeping the codebase organized and maintainable.

source("modules/navigation_module.R")    # Manages tab navigation
source("modules/reactive_values.R")      # Defines shared reactive values
source("modules/file_events.R")          # Handles file uploads and source selection
source("modules/custom_descriptors.R")   # Contains logic for descriptor customization
source("modules/custom_product.R")       # Contains logic for product customization
source("modules/product_selection.R")    # Manages product selection for data visualization
source("modules/result_table.R")         # Renders the results table for descriptors
source("modules/utils_circle_plot.R")    # Utility functions for circle plot generation
source("modules/circle_plot_module.R")   # Generates and renders the circle plot
source("modules/download_result.R")      # Handles downloading of tables and plots
source("modules/additional_feedback.R")  # Captures and displays additional feedback

# ── Server Function Definition ──────────────────────────────────────────────

# The `server` function initializes each module by passing `input`, `output`, and `session`.
# Each module registers its own observers, reactive expressions, and output renderers.
# The order of initialization is important when modules share reactive values or depend on one another.

server <- function(input, output, session) {
  
  # Initialize navigation module
  # Controls which tab is currently active based on user clicks
  navigation_module(input, output, session)
  
  # Initialize file events module
  # Watches for file uploads and source-selection events from the UI
  file_events(input, output, session)
  
  # Initialize descriptor customization module
  # Provides UI and server logic for renaming and configuring descriptors
  custom_descriptors(input, output, session)
  
  # Initialize product customization module
  # Provides UI and server logic for renaming and configuring products
  custom_product(input, output, session)
  
  # Initialize product selection module
  # Allows the user to choose products for data visualization
  product_selection(input, output, session)
  
  # Initialize result table module
  # Generates and displays an interactive DataTable of descriptor results
  result_table(input, output, session)
  
  # Initialize circle plot module
  # Creates an interactive circle plot based on descriptor scores
  circle_plot_module(input, output, session)
  
  # Initialize download result module
  # Enables users to download result tables or plots in various formats
  download_result(input, output, session)
  
  # Initialize additional feedback module
  # Displays any extra user feedback associated with each product
  additional_feedback(input, output, session)
}
