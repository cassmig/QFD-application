# 📌 reactive_values.R - Stores all reactive values for the app
#
# This script defines all the reactive values used throughout the application.
# Keeping them in a separate file improves code organization and simplifies debugging.

# ------------------------------------------------------------------------------
# Indicates whether a valid data file has been loaded (Fizz or Qualtrics).
# Initially FALSE until the user successfully loads and processes a file.
# ------------------------------------------------------------------------------
file_loaded <- reactiveVal(FALSE)

# ------------------------------------------------------------------------------
# Holds the descriptors metadata as a data.frame with columns:
#   - original_attribute: the key used in the raw data
#   - attribute: the current (possibly renamed) descriptor
#   - color: the color assigned to each descriptor
# Initialized as an empty data.frame; populated after processing input.
# ------------------------------------------------------------------------------
descriptors_df_data <- reactiveVal(
  data.frame(
    original_attribute = character(0),
    attribute          = character(0),
    color              = character(0),
    stringsAsFactors   = FALSE
  )
)

# ------------------------------------------------------------------------------
# Controls whether the UI should display the descriptor mapping table.
# ------------------------------------------------------------------------------
show_descriptor_table <- reactiveVal(FALSE)

# ------------------------------------------------------------------------------
# Stores the top descriptors (e.g., most frequently cited or highest-scoring).
# May hold a data.frame or list, depending on downstream usage.
# ------------------------------------------------------------------------------
top_descriptors <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Tracks the total number of unique descriptors identified in the dataset.
# ------------------------------------------------------------------------------
num_descriptors <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Holds product metadata as a data.frame with columns:
#   - product_code: internal code for each product (e.g., "produit_1")
#   - product_name: user-defined display name (empty until the user renames)
# Initialized empty; populated during file processing.
# ------------------------------------------------------------------------------
product_df_data <- reactiveVal(
  data.frame(
    product_code = character(0),
    product_name = character(0),
    stringsAsFactors = FALSE
  )
)

# ------------------------------------------------------------------------------
# Controls whether the UI should display the product mapping table.
# ------------------------------------------------------------------------------
show_product_table <- reactiveVal(FALSE)

# ------------------------------------------------------------------------------
# Tracks the number of distinct products in the loaded dataset.
# ------------------------------------------------------------------------------
num_of_products <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Holds the processed results table, typically a data.frame summarizing
# descriptor scores (sum_coefficients, count, etc.) for each product.
# ------------------------------------------------------------------------------
result <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Tracks how many panelists/respondents contributed data (for scaling or filtering).
# ------------------------------------------------------------------------------
num_panelists <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Stores additional descriptors supplied by panelists (Qualtrics-specific).
# Typically a data.frame or tibble with columns like product_code & descriptors list.
# ------------------------------------------------------------------------------
additional_descriptors <- reactiveVal(data.frame())

# ------------------------------------------------------------------------------
# Stores additional comments supplied by panelists (Fizz or Qualtrics).
# Typically a data.frame or tibble with columns like product_code & comments list.
# ------------------------------------------------------------------------------
additional_comments <- reactiveVal(data.frame())

# ------------------------------------------------------------------------------
# For Fizz files: holds a data.frame of average "puissance" (intensity) scores
# per product and intensity type. Populated after processing the "Puissance" columns.
# ------------------------------------------------------------------------------
puissance_table <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Tracks the number of distinct "puissance" (intensity) types detected in a Fizz file.
# ------------------------------------------------------------------------------
num_puissance_types <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Indicates which file type was loaded: either "fizz" or "qualtrics".
# Used to branch logic specific to each format.
# ------------------------------------------------------------------------------
file_type <- reactiveVal()

# ------------------------------------------------------------------------------
# Stores the name of the uploaded file (for display purposes in the UI).
# ------------------------------------------------------------------------------
file_name <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Stores the selected source from the UI: "Fizz" or "Qualtrics".
# ------------------------------------------------------------------------------
source_name <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Holds the internal code of the currently selected product for analysis (e.g., "produit_2").
# ------------------------------------------------------------------------------
selected_product <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# A reactiveValues list to store uploaded images for each product_code. Example:
#   product_images$produit_1 <- list(datapath = "/tmp/abcd.png", mime = "image/png")
# ------------------------------------------------------------------------------
product_images <- reactiveValues()

# ------------------------------------------------------------------------------
# Tracks the color currently chosen in the UI for a descriptor (used when assigning colors).
# ------------------------------------------------------------------------------
selected_color <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Holds the internal key/name of the currently selected descriptor (used in color assignment, etc.).
# ------------------------------------------------------------------------------
selected_descriptor <- reactiveVal(NULL)

# ------------------------------------------------------------------------------
# Always contains the most recent ggplot object for the circle plot. This allows
# downloading or re-rendering the plot at any time without recomputation.
# ------------------------------------------------------------------------------
current_plot <- reactiveVal(NULL)
