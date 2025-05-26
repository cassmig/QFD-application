# 📌 Load Modularized Code
source("modules/navigation_module.R")  # Manage tab navigation
source("modules/reactive_values.R")  # Store all reactive variables
source("modules/file_events.R")  # Handle file & source selection
source("modules/custom_descriptors.R")  # Handle descriptor customization
source("modules/custom_product.R")  # Handle product customization
source("modules/product_selection.R")  # Handle product selection for data visualization
source("modules/result_table.R")  # Handle result table for descriptors
source("modules/utils_circle_plot.R")  # Handle result table for descriptors
source("modules/circle_plot_module.R")  # Handle result table for descriptors
source("modules/download_result.R")  # Handle result table for descriptors
source("modules/additional_feedback.R")  # Handle result table for descriptors



server <- function(input, output, session) {
  

  # Initialize Modules
  navigation_module(input, output, session)
  file_events(input, output, session)
  custom_descriptors(input, output, session)  # Load descriptor customization
  custom_product(input, output, session)  # Load descriptor customization
  product_selection(input, output, session)  # Load product selection 
  result_table(input, output, session)  # Print Result table for descriptors
  circle_plot_module(input, output, session)  # Generate circle_plot
  download_result(input, output, session)  # Generate circle_plot
  additional_feedback(input, output, session)  # Generate circle_plot
  
  
}
