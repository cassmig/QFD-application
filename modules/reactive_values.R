# 📌 reactive_values.R - Stores all reactive values for the app

# This script defines all the reactive values used throughout the application.
# Keeping them in a separate file improves code organization and makes debugging easier.

# Variable to track if a file has been successfully loaded
file_loaded <- reactiveVal(FALSE)

# Reactive value to store the data frame of descriptors (attributes)
descriptors_df_data <- reactiveVal(data.frame(
  original_attribute = character(0),
  attribute = character(0),
  color = character(0)
))

# Reactive value to track if the descriptor table is displayed
show_descriptor_table <- reactiveVal(FALSE)

# Reactive value to store the top descriptors (most used ones)
top_descriptors <- reactiveVal(NULL)

# Number of descriptors
num_descriptors <- reactiveVal(NULL)

# Reactive value to store product information (product codes and names)
product_df_data <- reactiveVal(data.frame(
  product_code = character(0),
  product_name = character(0)
))

# Reactive value to track if the product table is displayed
show_product_table <- reactiveVal(FALSE)

# Number of products in the dataset
num_of_products <- reactiveVal(NULL)

# Reactive value to store the processed result table
result <- reactiveVal(NULL)

# Number of panelists who participated in the evaluation
num_panelists <- reactiveVal(NULL)

# Reactive values to store additional comments and descriptors
additional_descriptors <- reactiveVal(data.frame())
additional_comments <- reactiveVal(data.frame())

# Reactive value to store the power table (for Fizz files)
puissance_table <- reactiveVal(NULL)
num_puissance_types <- reactiveVal(NULL)

# Reactive value to store the file type (Fizz or Qualtrics)
file_type <- reactiveVal()

# Reactive values to store file name and source
file_name <- reactiveVal(NULL)
source_name <- reactiveVal(NULL)

# Reactive value to store the selected product
selected_product <- reactiveVal(NULL)

# Reactive values to store product images
product_images <- reactiveValues()

# Reactive values to store the selected color
selected_color <- reactiveVal(NULL)

# Reactive values to store the selected descriptor
selected_descriptor <- reactiveVal(NULL)

# contiendra toujours la dernière version du graphique
current_plot <- reactiveVal(NULL)




