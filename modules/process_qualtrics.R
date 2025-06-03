# ── modules/process_qualtrics.R ────────────────────────────────────────────
# This function cleans and transforms Qualtrics survey data into a format
# suitable for downstream QFD analysis.

process_qualtrics_data <- function(data) {
  
  # ── 1) Mark file type as "qualtrics" ─────────────────────────────────────
  file_type("qualtrics")
  
  # ── 2) Select only the relevant columns from the raw Qualtrics export ────
  selected_columns <- data %>%
    select(
      contains("ID"),      # Unique participant identifier
      contains("Q6"),      # Number of products each participant rated
      contains("DOM"),     # First dominant attribute (DOM)
      contains("SEC"),     # Second dominant attribute (SEC)
      contains("THIRD"),   # Third dominant attribute (THIRD)
      contains("scale"),   # Any scale or intensity rating columns
      contains("Q11"),     # Additional comments (free text)
      contains("Q13")      # Additional descriptors provided by panelists
    ) %>%
    select(-ResponseId, -contains("Duration"))  # Drop metadata columns
  
  # ── 3) Count the number of unique panelists by their ID column ────────────
  if ("ID" %in% colnames(data)) {
    num_panelists_val <- length(unique(data$ID))
    num_panelists(num_panelists_val)
    # ⧼ DEBUGGING: Print number of unique panelists ⧽
    # print(paste("DEBUG: Number of unique panelists detected:", num_panelists_val))
  } else {
    # ⧼ ERROR: Missing ID column ⧽
    print("ERROR: Column 'ID' not found in Qualtrics data.")
    # ⧼ DEBUGGING: List available columns ⧽
    # print(colnames(data))
  }
  
  # ── 4) Initialize an empty tibble to accumulate cleaned data ───────────────
  cleaned_data <- tibble(
    ID                        = character(),
    product_code              = character(),
    product_presentation_order = character(),
    attribute_dominance_order = character(),
    attribute                 = character()
  )
  # ⧼ DEBUGGING: Ensure structure ⧽
  # print("DEBUG: Initialized empty cleaned_data with columns:")
  # print(colnames(cleaned_data))
  
  # ── 5) Determine the number of products by counting Q6 columns ────────────
  q6_columns <- selected_columns %>% select(contains("Q6"))
  num_of_products_val <- ncol(q6_columns)
  num_of_products(num_of_products_val)
  # ⧼ DEBUGGING: Print number of products detected ⧽
  # print(paste("DEBUG: Number of products detected:", num_of_products_val))
  
  # ── 6) Loop through each product to assign descriptor scores ────────────────
  for (i in seq_len(num_of_products_val)) {
    # Identify columns for this product: ID plus DOM/SEC/THIRD columns for product i
    df <- selected_columns %>%
      select(ID, contains(str_glue("{i}")))
    
    # Pivot from wide to long: columns 3:5 correspond to DOM, SEC, THIRD for this product
    current_data <- df %>%
      pivot_longer(
        cols = 3:5,
        names_to = c("product_presentation_order", "attribute_dominance_order"),
        names_sep = "_",
        values_to = "attribute"
      ) %>%
      # Rename the Q6 column to "product_code" for clarity
      rename(product_code = str_glue("{i}_Q6")) %>%
      # Assign a numeric score to each attribute based on dominance order
      mutate(valeur_coeff = case_when(
        attribute_dominance_order == "DOM"   ~ 3,
        attribute_dominance_order == "SEC"   ~ 2,
        attribute_dominance_order == "THIRD" ~ 1,
        TRUE                                 ~ NA_real_
      ))
    
    # Append this product's processed rows to the main cleaned_data tibble
    cleaned_data <- bind_rows(cleaned_data, current_data)
    
    # ⧼ DEBUGGING: Check transformation for product i ⧽
    # print(paste("DEBUG: Processed data for product", i))
    # print(head(current_data))
  }
  
  # ── 7) Aggregate descriptor scores and counts per product ─────────────────
  result_df <- cleaned_data %>%
    group_by(product_code, attribute) %>%
    summarise(
      sum_coefficients = sum(valeur_coeff, na.rm = TRUE),
      count            = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(sum_coefficients), desc(count))
  
  # Map the original product_code values (e.g., Qualtrics labels) to "produit_1", "produit_2", etc.
  unique_codes <- unique(result_df$product_code)
  mapped_codes <- paste0("produit_", seq_along(unique_codes))
  product_mapping <- tibble(
    old_product = unique_codes,
    product     = mapped_codes
  )
  
  # Merge this mapping into the result_df and relocate for readability
  result_df <- result_df %>%
    left_join(product_mapping, by = c("product_code" = "old_product")) %>%
    relocate(product, .before = product_code) %>%
    rename(old_product = product_code)
  
  # ── 8) Extract additional comments (Q11 columns) and group by product ─────
  raw_comments <- map_df(seq_len(num_of_products_val), function(i) {
    code_col <- paste0(i, "_Q6")
    com_col  <- paste0(i, "_Q11")
    tibble(
      index        = i,
      product_code = as.character(selected_columns[[code_col]]),
      comment      = selected_columns[[com_col]]
    ) %>%
      filter(!is.na(comment), str_trim(comment) != "")
  })
  
  # ⧼ DEBUGGING: View raw comments ⧽
  # print("DEBUG: raw_comments:")
  # print(raw_comments)
  
  additional_comments_data <- raw_comments %>%
    left_join(product_mapping, by = c("product_code" = "old_product")) %>%
    filter(!is.na(product)) %>%
    group_by(product) %>%
    summarise(comments = list(comment), .groups = "drop") %>%
    rename(product_code = product)
  
  # ⧼ DEBUGGING: View grouped additional comments ⧽
  # print("DEBUG: additional_comments_data:")
  # print(additional_comments_data)
  
  additional_comments(additional_comments_data)
  
  # ── 9) Extract any additional descriptors (Q13 columns) and group by product ─
  raw_descriptors <- map_df(seq_len(num_of_products_val), function(i) {
    code_col <- paste0(i, "_Q6")
    des_col  <- paste0(i, "_Q13")
    tibble(
      index        = i,
      product_code = as.character(selected_columns[[code_col]]),
      descriptor   = selected_columns[[des_col]]
    ) %>%
      filter(!is.na(descriptor), str_trim(descriptor) != "")
  })
  
  # ⧼ DEBUGGING: View raw descriptors ⧽
  # print("DEBUG: raw_descriptors:")
  # print(raw_descriptors)
  
  additional_descriptors_data <- raw_descriptors %>%
    left_join(product_mapping, by = c("product_code" = "old_product")) %>%
    filter(!is.na(product)) %>%
    group_by(product) %>%
    summarise(descriptors = list(descriptor), .groups = "drop") %>%
    rename(product_code = product)
  
  # ⧼ DEBUGGING: View grouped additional descriptors ⧽
  # print("DEBUG: additional_descriptors_data:")
  # print(additional_descriptors_data)
  
  additional_descriptors(additional_descriptors_data)
  
  # ── 10) Extract unique attributes (descriptors) and store reactively ──────
  unique_descriptors <- unique(cleaned_data$attribute)
  num_descriptors_val <- length(unique_descriptors)
  num_descriptors(num_descriptors_val)
  
  unique_descriptors <- as.character(unique_descriptors)
  descriptors_df_data(data.frame(
    original_attribute = unique_descriptors,
    attribute          = unique_descriptors,
    color              = rep("#FFFFFF", num_descriptors_val),
    stringsAsFactors   = FALSE
  ))
  
  # ── 11) Initialize any missing columns in result_df ───────────────────────
  if (!"new_name_product" %in% names(result_df)) {
    result_df$new_name_product <- result_df$product
  }
  if (!"new_name_attribute" %in% names(result_df)) {
    result_df$new_name_attribute <- result_df$attribute
  }
  if (!"descriptor_color" %in% names(result_df)) {
    result_df$descriptor_color <- "#FFFFFF"
  }
  
  # ── 12) Store the final summary table into the reactive variable `result()` ─
  result(result_df)
  
  # ⧼ DEBUGGING: Confirm result_df content ⧽
  # print("DEBUG: Final result_df:")
  # print(head(result_df))
  
  # ── 13) Initialize `product_df_data()` from unique product codes ───────────
  product_df_data(data.frame(
    product_code = unique(result_df$old_product),
    product_name = rep("", length(unique(result_df$old_product))),
    stringsAsFactors = FALSE
  ))
  
  # ── 14) Finalize by marking the file as successfully loaded ────────────────
  file_loaded(TRUE)
}
