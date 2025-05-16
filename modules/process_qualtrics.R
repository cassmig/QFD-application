# 📌 process_qualtrics.R - Process Qualtrics data
# This function cleans and transforms Qualtrics data into a usable format.

process_qualtrics_data <- function(data) {
  
  file_type("qualtrics")  # ✅ Set file type as "Fizz"
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 1: Select Only Relevant Columns from the Qualtrics File
  # --------------------------------------------------------------------------------------------------------
  
  ### Select columns relevant for analysis
  selected_columns <- data %>% 
    select(
      contains("ID"),       # Unique participant identifier
      contains("Q6"),       # Number of products rated by each participant
      contains("DOM"),      # First dominant attribute
      contains("SEC"),      # Second dominant attribute
      contains("THIRD"),    # Third dominant attribute
      contains("scale"),    # Scale-related columns (e.g., intensity ratings)
      contains("Q11"),      # Additional comments
      contains("Q13")       # Additional descriptors provided by panelists
    ) %>%
    select(-ResponseId, -contains("Duration"))  # Exclude unnecessary metadata columns
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠️ Debugging: Validate Column Selection
          # --------------------------------------------------------------------------------------------------------
          
          ### Print selected column names for verification
          # print("✅ DEBUG: Selected Columns from Qualtrics Data")
          # print(colnames(selected_columns))
          
          ### Check if the expected columns exist in the dataset
          # if (ncol(selected_columns) == 0) {
          #   print(" ERROR: No matching columns found in the dataset!")
          #   print(" Available columns in the raw dataset:")
          #   print(colnames(data))  # Print all available columns for reference
          # } else {
          #   print(" Column selection successful!")
          # }
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 2: Count the Number of Unique Panelists
  # --------------------------------------------------------------------------------------------------------
  
  # Check if the "ID" column exists before proceeding
  if ("ID" %in% colnames(data)) {
    # Count the number of unique panelists
    num_panelists_val <- length(unique(data$ID))  
    
    # Store the count in a reactive variable
    num_panelists(num_panelists_val)  
    
    ### ✅ Debugging: Print the number of unique panelists
    # print(paste("✅ DEBUG: Number of unique panelists detected:", num_panelists_val))
    
  } else {   
    # ❌ If "ID" column is missing, print a warning message for debugging
    print("❌ ERROR: Column 'ID' not found in the dataset!")
    
    ### Display all available columns to help identify potential naming issues
    # print("Available columns in dataset:")
    # print(colnames(data))
    
  }
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 3: Initialize an Empty Dataframe for Cleaned Data
  # --------------------------------------------------------------------------------------------------------
  
  # Create an empty dataframe with predefined column structure
  data <- data.frame(
    "ID" = character(0),   # Unique identifier for each panelist
    "product_code" = character(0),   # Product identifier
    "product_presentation_order" = character(0),  # Order in which the product was presented
    "attribute_dominance_order" = character(0),   # Dominance level of the attribute (DOM, SEC, THIRD)
    "attribute" = character(0)   # Descriptor name
  )
  
        # --------------------------------------------------------------------------------------------------------
        # 🛠 Debugging: Verify Dataframe Initialization
        # --------------------------------------------------------------------------------------------------------
        
        # ## Print column names to confirm correct structure
        # print("✅ DEBUG: Dataframe initialized successfully with the following columns:")
        # print(colnames(data))
         
        # ## Print dataframe structure to ensure correct data types
        # print("✅ DEBUG: Dataframe structure:")
        # str(data)
         
        # ## Print a preview of the empty dataframe (should be empty at this stage)
        # print("✅ DEBUG: Dataframe preview (expected to be empty):")
        # print(head(data))
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 4: Count the Number of Products in the Dataset
  # --------------------------------------------------------------------------------------------------------
  
  # Extract columns that contain product-related data (Q6 columns)
  q6_columns <- selected_columns %>% select(contains("Q6"))  
  
  # Compute the number of unique products based on the number of Q6 columns
  num_of_products_val <- ncol(q6_columns)  
  
  # Store the number of products in a reactive value for later use
  num_of_products(num_of_products_val)  
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Verify Product Count Extraction
          # --------------------------------------------------------------------------------------------------------
          
          ## Print the computed number of detected products
          # print(paste("✅ DEBUG: Number of products detected:", num_of_products_val))
          
          ## Print the column names that match "Q6" to ensure correct selection
          # print("✅ DEBUG: Columns related to product count (Q6):")
          # print(colnames(q6_columns))
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 5: Assign Scores to Each Descriptor Based on Its Dominance Order
  # --------------------------------------------------------------------------------------------------------
  
  # Loop through each product in the dataset
  for (i in 1:num_of_products_val) {
    
    # Select relevant columns for the current product (i)
    df <- selected_columns %>%
      select(ID, contains(str_glue("{i}")))
    
    # Reshape data from wide format to long format
    current_data <- df %>%
      pivot_longer(
        cols = 3:5,  # Assuming the 3rd to 5th columns contain attribute dominance levels (DOM, SEC, THIRD)
        names_sep = "_",
        names_to = c("product_presentation_order", "attribute_dominance_order"),
        values_to = "attribute"
      ) %>%
      
      # Rename product column for clarity
      rename("product_code" = str_glue("{i}_Q6")) %>%
      
      # Assign coefficient scores based on attribute dominance order
      mutate(valeur_coeff = case_when(
        attribute_dominance_order == "DOM" ~ 3,   # Most dominant descriptor gets the highest score
        attribute_dominance_order == "SEC" ~ 2,   # Second most dominant gets medium score
        attribute_dominance_order == "THIRD" ~ 1, # Least dominant gets the lowest score
        TRUE ~ NA_real_                           # Assign NA if no valid dominance order is found
      ))
    
    
            # --------------------------------------------------------------------------------------------------------
            # 🛠 Debugging: Check Data Processing for Each Product
            # --------------------------------------------------------------------------------------------------------
            
            ### Print transformed data for verification
            # print(paste("📌 DEBUG: Processed data for product", i, ":"))
            # print(head(current_data))
            
            ### Print structure of extracted columns to ensure correct selection
            # print(paste("📌 DEBUG: Selected columns for product", i, ":"))
            # print(colnames(df))
    
    # Append processed data to the main dataset
    data <- bind_rows(data, current_data)
  }
  
            # --------------------------------------------------------------------------------------------------------
            # 🛠 Debugging: Verify the Final Processed DataFrame
            # --------------------------------------------------------------------------------------------------------
            
            ### Print summary of the final descriptor scoring table
            # print("📌 DEBUG: Final descriptor scoring table:")
            # print(head(data))
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 6: Aggregating Descriptor Scores and Counting Occurrences 
  # --------------------------------------------------------------------------------------------------------
  
  # Create the final summary table by grouping descriptors per product
  result <- data %>%
    group_by(product_code, attribute) %>%
    summarise(
      sum_coefficients = sum(valeur_coeff, na.rm = TRUE),  # Sum descriptor scores across panelists
      count = n(),  # Count how many times each descriptor was mentioned
      .groups = "drop"
    ) %>%
    arrange(desc(sum_coefficients), desc(count))  # Sort in descending order based on importance

  # 🧩 Étape intermédiaire : créer correspondance entre codes réels et codes Fizz-like
  unique_codes <- unique(result$product_code)
  mapped_codes <- paste0("produit_", seq_along(unique_codes))
  
  # Créer un dataframe de mapping entre les anciens codes et les nouveaux
  product_mapping <- tibble(
    old_product = unique_codes,
    product = mapped_codes
  )
  
  # Fusionner la correspondance dans le tableau result
  result <- result %>%
    left_join(product_mapping, by = c("product_code" = "old_product")) %>%
    relocate(product, .before = product_code)  # Mettre 'product' juste avant 'product_code'
  
  # On garde le code original sous 'old_product'
  result <- result %>%
    rename(old_product = product_code)
  
  
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Verify the Aggregated Data
          # --------------------------------------------------------------------------------------------------------
          
          ### Print top 10 rows of the resulting summary to check correct aggregation
          # print("📌 DEBUG: Resulting summary data (Top 10 rows)")
          # print(head(result, 10))
          
          ### Print unique values of product_code to check for missing or incorrect product assignments
          # print("📌 DEBUG: Unique product codes in result")
          # print(unique(result$product_code))
          
          ### Print unique descriptors (attributes) to confirm correctness
          # print("📌 DEBUG: Unique attributes in result")
          # print(unique(result$attribute))
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 7: Extract and Structure Additional Comments Per Product
  # --------------------------------------------------------------------------------------------------------
  
  # Extract additional comments from the dataset and reshape them into a structured format
  additional_comments_data <- data %>%
    select(ID, product_code, contains("Q11")) %>%  # Select panelist ID, product code, and Q11 comment fields
    pivot_longer(
      cols = contains("Q11"), 
      values_to = "comment", 
      names_to = "comment_source"
    ) %>% 
    filter(!is.na(comment) & comment != "") %>%  # Remove empty or NA comments
    group_by(product_code) %>%
    summarise(comments = list(unique(comment)), .groups = "drop")  #  Store as a list per product
  
  # Store the processed comments in a reactive value
  additional_comments(additional_comments_data)
  
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Verify Processed Additional Comments
          # --------------------------------------------------------------------------------------------------------
          
          ### Print structured comments grouped by product
          # print("✅ DEBUG: Processed Additional Comments (Grouped by Product)")
          # print(additional_comments_data)
          
          ### Print comments for each product separately to confirm correct processing
          # for (i in 1:nrow(additional_comments_data)) {
          #   product <- additional_comments_data$product_code[i]
          #   comments <- additional_comments_data$comments[[i]]
          #   
          #   print(paste("✅ DEBUG: Comments for Product", product, ":"))
          #   print(comments)
          # }
  
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 8: Extract and Structure Additional Descriptors Per Product
  # --------------------------------------------------------------------------------------------------------
  
  # Extract additional descriptors from the dataset and reshape them into a structured format
  additional_descriptors_data <- data %>%
    select(ID, product_code, contains("Q13")) %>%  # Select panelist ID, product code, and Q13 descriptor fields
    pivot_longer(
      cols = contains("Q13"), 
      values_to = "descriptor", 
      names_to = "descriptor_source"
    ) %>%
    filter(!is.na(descriptor) & descriptor != "") %>%  # Remove empty or NA descriptors
    group_by(product_code) %>%
    summarise(descriptors = list(unique(descriptor)), .groups = "drop")  # Store as a list per product
  
  # Store the processed descriptors in a reactive value
  additional_descriptors(additional_descriptors_data)
  
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Verify Processed Additional Descriptors
          # --------------------------------------------------------------------------------------------------------
          
          ### Print structured descriptors grouped by product
          # print("✅ DEBUG: Processed Additional Descriptors (Grouped by Product)")
          # print(additional_descriptors_data)
          
          ### Print descriptors for each product separately to confirm correct processing
          # for (i in 1:nrow(additional_descriptors_data)) {
          #   product <- additional_descriptors_data$product_code[i]
          #   descriptors <- additional_descriptors_data$descriptors[[i]]
          #   
          #   print(paste("✅ DEBUG: Descriptors for Product", product, ":"))
          #   print(descriptors)
          # }
  
  
  

  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 9: Process Scale Columns & Compute Average Scores per Power Type
  # --------------------------------------------------------------------------------------------------------
  
  # Extract only scale-related columns (columns containing "scale")
  scale_columns <- selected_columns %>% select(ID, contains("scale"))
  
  # Reshape data: Convert from wide format to long format for easier processing
  scale_data_long <- scale_columns %>%
    pivot_longer(
      cols = contains("scale"), 
      names_to = c("product", "type"),  # Extract product number & scale type
      names_sep = "_scale_", 
      values_to = "puissance"
    ) %>%
    mutate(
      product = as.character(product),  # Ensure product number is a character
      puissance = as.numeric(puissance)  # Convert puissances to numeric for calculations
    ) %>%
    filter(!is.na(puissance))  # Remove NA values to ensure clean data
  
  
  # Compute the Number of Unique Power Types
  num_power_types <- length(unique(scale_data_long$type))  # Count unique power types
  num_puissance_types(num_power_types)  # Store in reactive variable
  
  
  # Compute Average puissances per Product & Scale Type
  avg_puissance_scores <- scale_data_long %>%
    group_by(product, type) %>%  # Group data by product & scale type
    summarise(
      avg_puissance = mean(puissance, na.rm = TRUE),  # Compute average puissance
      .groups = "drop"
    ) %>%
    arrange(product, type)  # Arrange by product and scale type
  
  avg_puissance_scores <- avg_puissance_scores %>%
    mutate(
      product = paste0("produit_", product),  # 🔁 transformation cruciale
      type = paste0("A", type)
    )
  
  # Store computed results in a reactive variable for further use
  puissance_table(avg_puissance_scores)
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Verify Processed Scale Scores
          # --------------------------------------------------------------------------------------------------------
          
          ### Print processed scale scores for debugging
          # print("✅ DEBUG: Processed Scale Scores (Average per Product & Scale Type)")
          # print(avg_puissance_scores)
        
          ### Loop through each product and print its scale scores for verification
          # for (i in unique(avg_puissance_scores$product)) {
          #   product_scales <- avg_puissance_scores %>%
          #     filter(product== i)
          #   
          #   print(paste("✅ DEBUG: Scale Scores for Product", i, ":"))
          #   print(product_scales)
          # }
        
          ###  Print the number of detected power types
          # print(paste("✅ DEBUG: Number of unique power types detected:", num_power_types))

  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 10: Process Unique Descriptors
  # --------------------------------------------------------------------------------------------------------
  
  # Extract unique descriptor names from the processed dataset
  descriptors <- unique(data$attribute)  
  
  # Count the number of unique descriptors
  num_descriptors_val <- length(descriptors)
  
  # Store the count in a reactive variable for further use
  num_descriptors(num_descriptors_val)  
  
  # Store descriptors in a reactive dataframe with default color assignment
  descriptors_df_data(data.frame(
    original_attribute = descriptors,  # Store original descriptor name
    attribute = descriptors,  # This column allows for descriptor renaming
    color = rep('#FFFFFF', length(descriptors))  # Assign default white color to each descriptor
  ))
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠 Debugging: Verify Descriptor Processing
          # --------------------------------------------------------------------------------------------------------
          
          # ## Print descriptor names to verify correct extraction
          # print("✅ DEBUG: Processed Unique Descriptors")
          # print(head(descriptors, 10))  # Display first 10 descriptors
          # 
          # ## Print the total count of unique descriptors
          # print(paste("✅ DEBUG: Total unique descriptors detected:", num_descriptors_val))
          
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 11: Process Unique Product Codes
  # --------------------------------------------------------------------------------------------------------
  
  # Extract unique product codes from the processed dataset
  code <- unique(data$product_code)
  
  # Create a dataframe to store product codes with an empty column for product names
  product_df_data(data.frame(
    product_code = code,
    product_name = rep('', length(code))  # Empty product names for now (to be updated later)
  ))
  

  
  # Initialize `new_name_product` with default product codes
  if (!"new_name_product" %in% names(result)) {
    result$new_name_product <- result$product
    # cat("✅ INFO: new_name_product initialized with default product codes.\n")
  }
  
    # Initialize `new_name_attribute` with default descriptor names
  if (!"new_name_attribute" %in% names(result)) {
    result$new_name_attribute <- result$attribute
    # cat("✅ INFO: new_name_attribute initialized with default attribute values.\n")
  }
  
  # Initialize `descriptor_color` with default white color
  if (!"descriptor_color" %in% names(result)) {
    result$descriptor_color <- "#FFFFFF"
    # cat("✅ INFO: descriptor_color initialized with default white (#FFFFFF) for all descriptors.\n")
  }
  
  # Save final result into the global reactiveVal
  result(result)
  

          # --------------------------------------------------------------------------------------------------------
          # 🛠  Debugging: Verify Processed Product Data
          # --------------------------------------------------------------------------------------------------------
          
          ## Print processed product data to verify correctness
          print("✅ DEBUG: Processed Unique Product Codes")
          print(paste("✅ DEBUG: Total unique products detected:", length(code)))
          ## Ensure the result is stored correctly
          print("✅ DEBUG: result() updated in process_qualtrics_data()")
          print(head(result))
  
          head(result()) %>% select(product, old_product)
          

  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 12: Finalization
  # --------------------------------------------------------------------------------------------------------
  
  # Mark the file as successfully loaded for further processing
  file_loaded(TRUE)
  
  # Retrieve descriptors from `descriptors_df_data()` for further processing or display
  descriptors <- descriptors_df_data()$attribute  
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠  Debugging: Confirm Successful Data Processing
          # --------------------------------------------------------------------------------------------------------
          
          # ## Print confirmation message indicating successful completion of processing
          # print(" Data Processing Completed Successfully! )
  
  
}
