

# 📌 process_Fizz.R - Process Fizz data
# This function cleans and transforms Fizz data into a usable format.

process_fizz_data <- function(data) {
  
  file_type("fizz")  # Set file type as "Fizz"

  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 1: Count the Number of Products in the Fizz File
  # --------------------------------------------------------------------------------------------------------# 
  
  # Extract column names from the dataset
  column_names <- colnames(data)  
  
  # Identify the columns containing "Descripteur 1" to count the number of products
  num_of_products <- sum(str_detect(column_names, "Descripteur 1"))  
  
  # Compute the actual number of products (each product has 2 descriptor columns)
  num_of_products_val <- num_of_products / 2  
  
  # Store the computed number of products in a reactive variable
  num_of_products(num_of_products_val)  
  
  #️ Generate product codes in the format "produit_i"
  product_code <- paste0("produit_", seq_len(num_of_products_val))
  
  # Store the product codes in a dataframe with an empty placeholder for product names
  product_df_data(data.frame(
    product_code = product_code,
    product_name = rep("", num_of_products_val)  # Product names will be updated later
  ))
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Verify Correct Extraction and Counting of Products
          # --------------------------------------------------------------------------------------------------------# 
  
          ### Print the renamed product codes for verification
          # print("✅ DEBUG: Renamed product codes:")
          # print(product_code)
          
          ### Print the total number of products detected
          # print(paste("✅ DEBUG: Number of products detected:", num_of_products_val))
  
  

  
  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 2: Extract First Row as Header
  # --------------------------------------------------------------------------------------------------------# 
  
  # Extract the first row of the dataset to use as column headers
  header_row <- data[1, ]  
  
  # Convert the extracted row into a character vector
  header_vector <- as.character(header_row)  
  
  #️ Store the header information in a structured tibble (data frame)
  header_df <- tibble(header = header_vector)  
  
  
        # --------------------------------------------------------------------------------------------------------# 
        # 🛠️ Debugging: Verify Correct Header Extraction
        # --------------------------------------------------------------------------------------------------------# 
        
        ### Print the extracted header row for verification
        # print("✅ DEBUG: Extracted Header Row:")
        # print(header_vector)
        
  
  

  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 3: Extract and Process "Puissance" Columns
  # --------------------------------------------------------------------------------------------------------# 
  
  # Define the regex pattern to identify "Puissance" columns in the dataset
  puissance_pattern <- "^P\\d+_P(\\d+)_A(\\d+)$"  
  
  # Identify column indices that match the "Puissance" pattern
  puissance_cols <- grep(puissance_pattern, header_row, ignore.case = TRUE)  
  
  #️ Proceed only if "Puissance" columns exist in the dataset
  if (length(puissance_cols) > 0) {
    
    # Extract the relevant "Puissance" data (excluding the first row)
    puissance_data <- data[-1, puissance_cols]  
    
    # Extract product numbers and power types from column names using regex
    puissance_info <- str_match(header_row[puissance_cols], puissance_pattern)  
    product_numbers <- puissance_info[, 2]  
    power_types <- as.numeric(puissance_info[, 3])  # Convert extracted power types to numeric
    
    # Determine the number of unique power types
    num_puissance_types_val <- max(power_types, na.rm = TRUE)  
    num_puissance_types(num_puissance_types_val)  # Store in reactive value
    
    # Convert puissance data to numeric format
    puissance_data <- as.data.frame(lapply(puissance_data, function(x) as.numeric(as.character(x))))  
    
    # Remove any rows containing NA values (optional cleanup step)
    puissance_data_clean <- na.omit(puissance_data)  
    
    # Create Mapping Table for Product and Power Type Association
    column_mapping <- data.frame(
      column = colnames(puissance_data_clean),   # Column names (original)
      product = paste0("produit_", product_numbers),  # Assign product labels
      type = paste0("A", power_types),  # Assign power type labels
      stringsAsFactors = FALSE
    )
    
    # Transform Puissance Data into Long Format
    puissance_long <- puissance_data_clean %>%
      pivot_longer(
        cols = everything(),  # Convert all columns to long format
        names_to = "column",  # Store column names under "column"
        values_to = "puissance"  # Store values under "puissance"
      ) %>%
      left_join(column_mapping, by = "column")  # Merge with mapping table to associate product & type
  }
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Verify Extracted Data
          # --------------------------------------------------------------------------------------------------------# 
          
          ### Print extracted Puissance data for validation
          # print("✅ DEBUG: Extracted Puissance Data:")
          # print(head(puissance_long))
          
          ### Print number of unique power types detected
          # print(paste("✅ DEBUG: Number of Puissance Types Detected:", num_puissance_types_val))
  
  
  
  
  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 4: Compute Average Puissance Scores (Summary Table)
  # --------------------------------------------------------------------------------------------------------# 
  
  # Ensure that puissance_long contains valid data before proceeding
  if (exists("puissance_long") && nrow(puissance_long) > 0) {
    
    # Compute the average puissance score per product and type
    average_puissance_scores <- puissance_long %>%
      group_by(product, type) %>%  # Group by product and power type
      summarise(
        avg_puissance = mean(puissance, na.rm = TRUE),  # Calculate the mean score, ignoring NA values
        .groups = "drop"
      ) %>%
      arrange(product, type)  # Sort results for better readability
    
    # Store the computed puissance scores in a reactive table
    puissance_table(average_puissance_scores)  
    
  } else {
    
    # Handle the case where no "Puissance" columns were found
    print("\n⚠ No 'Puissance' columns found in the Fizz file.\n")
    
    # Store NULL values to indicate missing data
    puissance_table(NULL)  
    num_puissance_types(0)  
  }
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Verify Computed Average Scores
          # --------------------------------------------------------------------------------------------------------# 
          
          ### Print summarized puissance table for validation
          # print("✅ DEBUG: Summarized Puissance Scores:")
          # print(average_puissance_scores)


  
  
  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 5: Filter Out "Puissance" Columns
  # --------------------------------------------------------------------------------------------------------# 
  
  #️ Define a regex pattern to identify "Puissance" columns
  cols_to_keep <- !grepl("Puissance|P\\d+_P\\d+_A\\d+", header_row, ignore.case = TRUE)
  
  # Filter out the "Puissance" columns from the dataset
  data_filtered <- data[, cols_to_keep]  
  
  # Set the first row as column headers
  colnames(data_filtered) <- data_filtered[1, ]  
  
  # Remove the first row (as it's now the header)
  data_filtered <- data_filtered[-1, ]  
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Verify Filtered Data
          # --------------------------------------------------------------------------------------------------------# 
          
          ### Print a preview of the cleaned dataset without "Puissance" columns
          # print("✅ DEBUG: Filtered Data (Without Puissance Columns):")
          # print(head(data_filtered))
          
          ### Print column names to confirm "Puissance" columns are removed
          # print("✅ DEBUG: Column Names After Filtering:")
          # print(colnames(data_filtered))
  

  
  
  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 6: Extract Panelist Data & Count Panelists
  # --------------------------------------------------------------------------------------------------------# 
  
  # Select only relevant columns: Panelist ID (CJ) & descriptor columns (AT_1, AT_2, AT_3)
  panelist_data <- data_filtered %>%
    select(CJ, matches("AT_[1-3]$"))  
  
  # Filter panelists who provided at least one valid response (non-empty & non-NA)
  panelists_with_data <- panelist_data %>% 
    filter(if_any(-CJ, ~ !is.na(.) & . != "")) 
  
  # Count the number of panelists who provided valid responses
  num_panelists_val <- nrow(panelists_with_data)
  
  # Store the panelist count in a reactive value for later use
  num_panelists(num_panelists_val)  
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Verify Extracted Panelist Data
          # --------------------------------------------------------------------------------------------------------# 
          
          ### Print the total number of panelists detected
          # print(paste("✅ DEBUG: Number of Panelists with Valid Data:", num_panelists_val))
          
          ### Print a preview of the panelist data
          # print("✅ DEBUG: Panelist Data (First Few Rows):")
          # print(head(panelists_with_data))
          
          ### Print column names to confirm correct extraction
          # print("✅ DEBUG: Columns in Panelist Data:")
          # print(colnames(panelist_data))
          
  
  

  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 7: Transform Panelist Data into Long Format & Assign Scores
  # --------------------------------------------------------------------------------------------------------# 
  
  # Convert the panelist data from wide format to long format
  long_table <- panelists_with_data %>%
    pivot_longer(
      cols = matches("AT_[1-3]$"),  # Select only descriptor columns (AT_1, AT_2, AT_3)
      names_to = c("product", "AT"),  # Create two new columns: "product" & "AT"
      names_pattern = "P1_P(\\d+)_AT_(\\d+)",  # Extract product number and AT rank
      values_to = "attribute"  # Store descriptor names in the "attribute" column
    ) %>%
    mutate(product = paste0("produit_", product))  # Convert product numbers into "produit_i" format
  
  # Assign scores to each descriptor based on AT ranking
  long_table <- long_table %>%
    mutate(Score = case_when(
      AT == "1" ~ 3,  # First-ranked descriptor gets score 3
      AT == "2" ~ 2,  # Second-ranked descriptor gets score 2
      AT == "3" ~ 1   # Third-ranked descriptor gets score 1
    ))
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Verify Transformed Data
          # --------------------------------------------------------------------------------------------------------# 
          
          ### Print the first few rows to confirm correct transformation
          # print("✅ DEBUG: Descriptor Data in Long Format:")
          # print(head(long_table))
          
          ### Print unique product codes to confirm transformation
          # print("✅ DEBUG: Unique Product Codes in Long Table:")
          # print(unique(long_table$product))
          
          ### Print unique descriptor values to ensure correct extraction
          # print("✅ DEBUG: Unique Attributes in Long Table:")
          # print(unique(long_table$attribute))
          
          ### Check score assignment
          # print("✅ DEBUG: Score Assignment in Long Table:")
          # print(table(long_table$Score))
  

  
  
  
  # --------------------------------------------------------------------------------------------------------# 
  # 📌 Step 8: Compute Descriptor Scores and Format Descriptors
  # --------------------------------------------------------------------------------------------------------# 
  
  # Convert attribute names to "descriptor_i" format and aggregate scores
  result <- long_table %>%
    mutate(attribute = paste0("descriptor_", attribute)) %>%  # Convert descriptors to "descriptor_i" format
    group_by(product, attribute) %>%  # Group data by product and descriptor
    summarise(
      sum_coefficients = sum(Score, na.rm = TRUE),  # Sum the descriptor scores
      count = n(),  # Count how many times each descriptor was mentioned
      .groups = "drop"
    ) %>%
    arrange(desc(sum_coefficients), desc(count))  # Sort in descending order of score
  
          # --------------------------------------------------------------------------------------------------------# 
          # 🛠️ Debugging: Validate Computed Scores and Descriptor Formatting
          # --------------------------------------------------------------------------------------------------------# 
          
          ### Verify that attribute names are correctly formatted as "descriptor_i"
          # print("✅ DEBUG: Unique values in `attribute` after conversion to descriptor_i format")
          # print(unique(result$attribute))
          
          ### Print first few rows of the processed result data to verify structure
          # print("✅ DEBUG: Computed Descriptor Scores (Top Rows)")
          # print(head(result))
          
          ### Check the range of summed coefficients and counts
          # print("✅ DEBUG: Summary Statistics for Computed Scores")
          # print(summary(result$sum_coefficients))
          # print(summary(result$count))
          
  


  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 9: Update and Store Processed Descriptors
  # --------------------------------------------------------------------------------------------------------
  
  # Extract unique descriptor names from the computed result table
  descriptors <- unique(result$attribute)
  
  # Compute the total number of unique descriptors
  num_descriptors_val <- length(descriptors)
  
  # Store the descriptor count in a reactive variable (for further processing)
  num_descriptors(num_descriptors_val)
  
  # Ensure descriptors are stored as character data (avoiding factor/numeric issues)
  descriptors <- as.character(descriptors)
  
  # Sort the descriptors alphabetically to maintain a consistent ordering (optional)
  descriptors <- sort(descriptors)
  
  # Store the descriptors in a structured dataframe for later reference
  descriptors_df_data(data.frame(
    original_attribute = descriptors,  # Stores the original descriptor name
    attribute = descriptors,  # Allows for renaming in further steps
    color = rep('#FFFFFF', length(descriptors))  # Assigns default white color to all descriptors
  ))
  
  # Initialize `new_name_product` column with default product codes
  
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
  
  
  # Store the processed result in the global reactive variable `result()`
  result(result)
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠️ Debugging: Validate Descriptor Updates
          # --------------------------------------------------------------------------------------------------------
          
          ### Print the total number of descriptors detected
          # print(paste("✅ DEBUG: Number of detected descriptors:", num_descriptors_val))
         
          ### Display the original extracted descriptors
          # print("✅ DEBUG: Original List of Descriptors:")
          # print(descriptors)
        
          ### Show the renamed descriptor mapping for verification
          # print("✅ DEBUG: Renamed Descriptors Mapping:")
          # print(renamed_descriptors)
  
          ## Ensure the result is stored correctly
          # print("✅ DEBUG: result() updated in process_fizz_data()")
          # print(head(result))
  
          
  
  

  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 10: Extract & Aggregate Additional Comments
  # --------------------------------------------------------------------------------------------------------
  
  # Select all columns that match the pattern for additional comments (e.g., P1_P{i}_TE_{j})
  additional_comments_data <- data_filtered %>% 
    select(matches("P1_P\\d+_TE_\\d+"))
  
  # Check if additional comment columns exist before processing
  if (ncol(additional_comments_data) > 0) {
    
    # Convert wide format to long format
    long_comments <- additional_comments_data %>%
      pivot_longer(
        cols = everything(),
        names_to = c("product_number", "comment_number"),  # Extract product and comment index
        names_pattern = "P1_P(\\d+)_TE_(\\d+)",  
        values_to = "comment"
      ) %>%
      mutate(product_code = paste0("produit_", product_number)) %>%  # Standardize product names
      filter(!is.na(comment), comment != "") %>%  # Remove empty or NA comments
      select(product_code, comment)  # Keep only relevant columns
    
    #️ Group comments by product and store them in a list
    grouped_comments <- long_comments %>%
      group_by(product_code) %>%
      summarise(comments = list(comment), .groups = "drop")
    
    # Store in reactive variable for further use
    additional_comments(grouped_comments)
    
            # # --------------------------------------------------------------------------------------------------------
            # # 🛠️ Debugging: Validate Additional Comments Extraction
            # # --------------------------------------------------------------------------------------------------------
          
            ### Print the structured additional comments data
            # print("✅ DEBUG: Aggregated Additional Comments by Product:")
            # print(grouped_comments)
          
            ### Print comments for each product
            # for (row in 1:nrow(grouped_comments)) {
            #   product <- grouped_comments$product_code[row]
            #   comments_list <- grouped_comments$comments[[row]]
            #   print(paste0("✅ DEBUG: Product - ", product))
            #   print("✅ DEBUG: Comments:")
            #   print(comments_list)
            #   print("---------------------")
            # }
    
  } else {
    print("⚠️ WARNING: No additional comments found in the Fizz file.")
    additional_comments(NULL)
  }
  
  # --------------------------------------------------------------------------------------------------------
  # 📌 Step 11: Finalization
  # --------------------------------------------------------------------------------------------------------
  
  # Mark the file as successfully loaded
  file_loaded(TRUE)  
  
  # Retrieve updated descriptor names for further processing
  descriptors <- descriptors_df_data()$attribute  
  
          # --------------------------------------------------------------------------------------------------------
          # 🛠️ Debugging: Validation Checks Before Finalization
          # --------------------------------------------------------------------------------------------------------
          
          ### Print confirmation message
          #print("✅ DEBUG: Data processing successfully completed! ")
          
          ### Validate that descriptors have been updated correctly
          # print("✅ DEBUG: Final list of descriptors after processing:")
          # print(descriptors)
          
          # Confirm the file loading status
          # print("✅ DEBUG: File loaded status ->")
          # print(file_loaded())
  
  


}
