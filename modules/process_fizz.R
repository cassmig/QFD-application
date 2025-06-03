# ── modules/process_fizz.R ───────────────────────────────────────────────────
# This function cleans and transforms Fizz data into a standardized format
# suitable for downstream QFD analysis.

process_fizz_data <- function(data) {
  
  # ── 1) Mark file type as "Fizz" ────────────────────────────────────────────
  file_type("fizz")
  
  # ── 2) Count the number of products in the Fizz file ───────────────────────
  #    - Each product has two "Descripteur 1" columns (one for each dimension)
  column_names <- colnames(data)
  num_of_products <- sum(str_detect(column_names, "Descripteur 1"))
  num_of_products_val <- num_of_products / 2
  num_of_products(num_of_products_val)
  
  #    - Generate product codes "produit_1", "produit_2", etc.
  product_codes <- paste0("produit_", seq_len(num_of_products_val))
  #    - Initialize product_df_data() with blank names for later editing
  product_df_data(data.frame(
    product_code = product_codes,
    product_name = rep("", num_of_products_val),
    stringsAsFactors = FALSE
  ))
  
  # ── 3) Extract the first row to use as column headers ──────────────────────
  header_row <- data[1, ]
  header_vector <- as.character(header_row)
  header_df <- tibble(header = header_vector)
  
  # ── 4) Identify and process "Puissance" columns ────────────────────────────
  #    Regex pattern to match columns like "P1_P{product}_A{powerType}"
  puissance_pattern <- "^P\\d+_P(\\d+)_A(\\d+)$"
  puissance_cols <- grep(puissance_pattern, header_row, ignore.case = TRUE)
  
  if (length(puissance_cols) > 0) {
    #    a) Extract numerical values and power types from header
    puissance_data <- data[-1, puissance_cols]
    puissance_info <- str_match(header_row[puissance_cols], puissance_pattern)
    product_numbers <- puissance_info[, 2]
    power_types <- as.numeric(puissance_info[, 3])
    
    #    b) Determine number of unique power types and store reactively
    num_puissance_types_val <- max(power_types, na.rm = TRUE)
    num_puissance_types(num_puissance_types_val)
    
    #    c) Convert puissance data columns to numeric
    puissance_data <- as.data.frame(lapply(puissance_data, function(x) as.numeric(as.character(x))))
    #    d) Drop any rows with NA in puissance data (cleanup)
    puissance_data_clean <- na.omit(puissance_data)
    
    #    e) Build a mapping table associating each column to product and type
    column_mapping <- data.frame(
      column  = colnames(puissance_data_clean),
      product = paste0("produit_", product_numbers),
      type    = paste0("A", power_types),
      stringsAsFactors = FALSE
    )
    
    #    f) Pivot to long format and join with mapping to get product & type
    puissance_long <- puissance_data_clean %>%
      pivot_longer(
        cols       = everything(),
        names_to   = "column",
        values_to  = "puissance"
      ) %>%
      left_join(column_mapping, by = "column")
    
    # ── 5) Compute average "puissance" per product and type ───────────────────
    average_puissance_scores <- puissance_long %>%
      group_by(product, type) %>%
      summarise(
        avg_puissance = mean(puissance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(product, type)
    
    puissance_table(average_puissance_scores)
    
  } else {
    # If no "Puissance" columns are found, set reactive values to NULL or zero
    puissance_table(NULL)
    num_puissance_types(0)
  }
  
  # ── 6) Remove all "Puissance" columns and reset headers ────────────────────
  cols_to_keep <- !grepl("Puissance|P\\d+_P\\d+_A\\d+", header_row, ignore.case = TRUE)
  data_filtered <- data[, cols_to_keep]
  colnames(data_filtered) <- data_filtered[1, ]
  data_filtered <- data_filtered[-1, ]
  
  # ── 7) Extract panelist data and count valid panelists ────────────────────
  panelist_data <- data_filtered %>%
    select(CJ, matches("AT_[1-3]$"))
  
  panelists_with_data <- panelist_data %>%
    filter(if_any(-CJ, ~ !is.na(.) & . != ""))
  
  num_panelists_val <- nrow(panelists_with_data)
  num_panelists(num_panelists_val)
  
  # ── 8) Transform panelist data to long format and assign scores ──────────
  long_table <- panelists_with_data %>%
    pivot_longer(
      cols         = matches("AT_[1-3]$"),
      names_to     = c("product", "AT"),
      names_pattern = "P1_P(\\d+)_AT_(\\d+)",
      values_to    = "attribute"
    ) %>%
    mutate(product = paste0("produit_", product)) %>%
    mutate(Score = case_when(
      AT == "1" ~ 3,
      AT == "2" ~ 2,
      AT == "3" ~ 1,
      TRUE      ~ 0
    ))
  
  # ── 9) Compute descriptor scores and format attribute names ───────────────
  result_df <- long_table %>%
    mutate(attribute = paste0("descriptor_", attribute)) %>%
    group_by(product, attribute) %>%
    summarise(
      sum_coefficients = sum(Score, na.rm = TRUE),
      count            = n(),
      .groups          = "drop"
    ) %>%
    arrange(desc(sum_coefficients), desc(count))
  
  # ── 10) Determine unique descriptors and store in reactive dataframe ──────
  descriptors <- unique(result_df$attribute)
  num_descriptors_val <- length(descriptors)
  num_descriptors(num_descriptors_val)
  
  descriptors <- as.character(descriptors)
  descriptors <- sort(descriptors)
  descriptors_df_data(data.frame(
    original_attribute = descriptors,
    attribute          = descriptors,
    color              = rep("#FFFFFF", num_descriptors_val),
    stringsAsFactors   = FALSE
  ))
  
  # ── 11) Initialize columns in result_df for new names and colors if absent ─
  if (!"new_name_product" %in% names(result_df)) {
    result_df$new_name_product <- result_df$product
  }
  if (!"new_name_attribute" %in% names(result_df)) {
    result_df$new_name_attribute <- result_df$attribute
  }
  if (!"descriptor_color" %in% names(result_df)) {
    result_df$descriptor_color <- "#FFFFFF"
  }
  
  # ── 12) Save processed result into reactive variable `result()` ────────────
  result(result_df)
  
  # ── 13) Extract and aggregate any additional comments ──────────────────────
  additional_comments_data <- data_filtered %>%
    select(matches("P1_P\\d+_TE_\\d+"))
  
  if (ncol(additional_comments_data) > 0) {
    long_comments <- additional_comments_data %>%
      pivot_longer(
        cols         = everything(),
        names_to     = c("product_number", "comment_number"),
        names_pattern = "P1_P(\\d+)_TE_(\\d+)",
        values_to    = "comment"
      ) %>%
      mutate(product_code = paste0("produit_", product_number)) %>%
      filter(!is.na(comment), comment != "") %>%
      select(product_code, comment)
    
    grouped_comments <- long_comments %>%
      group_by(product_code) %>%
      summarise(comments = list(comment), .groups = "drop")
    
    additional_comments(grouped_comments)
  } else {
    additional_comments(NULL)
  }
  
  # ── 14) Mark file loading as complete ─────────────────────────────────────
  file_loaded(TRUE)
}
