# ── modules/product_selection.R ────────────────────────────────────────────
# This module allows users to select a product for analysis and displays
# contextual information (e.g., scoring rules and number of panelists).

product_selection <- function(input, output, session) {
  
  # ── Step 1: Retrieve and Order Product Data ───────────────────────────────
  ordered_product_data <- reactive({
    req(product_df_data())          # Ensure product_df_data() is available
    df <- product_df_data()
    
    # Build a display_name for each product:
    # - If a custom product_name exists, use it
    # - If Qualtrics, prefix code with "produit_"
    # - Else (Fizz), use product_code directly
    df <- df %>%
      mutate(
        display_name = case_when(
          nzchar(product_name)        ~ product_name,
          file_type() == "qualtrics"  ~ paste0("produit_", product_code),
          TRUE                         ~ product_code
        )
      )
    # DEBUG: inspect the first few rows of product mapping
    print(head(df %>% select(product_code, product_name, display_name)))
    
    # If Fizz data, sort products by their numeric suffix (e.g., "product_3" -> 3)
    if (file_type() == "fizz") {
      nums <- as.numeric(gsub("\\D", "", df$product_code))
      if (all(!is.na(nums))) {
        df <- df[order(nums), ]
      }
    }
    df
  })
  
  # ── Step 2: Render UI for Product Selection ─────────────────────────────────
  output$productSelectionUI <- renderUI({
    req(file_loaded(), ordered_product_data())
    pd <- ordered_product_data()
    
    # Create a grid of "cards" for each product
    div(
      class = "product-selection-grid",
      lapply(seq_len(nrow(pd)), function(i) {
        code <- pd$product_code[i]
        name <- pd$display_name[i]
        img  <- product_images[[code]]
        
        # If an image has been uploaded for this product, encode it as base64
        image_tag <- if (!is.null(img) && file.exists(img$datapath)) {
          uri <- base64enc::dataURI(file = img$datapath, mime = img$mime)
          tags$img(src = uri, class = "product-card-image")
        } else {
          # Placeholder if no image is available
          div(class = "no-image", "No image")
        }
        
        # Wrap the image and product name in an actionButton
        actionButton(
          inputId = paste0("select_product_", i),
          label   = div(
            class = "product-card",
            image_tag,
            div(class = "product-card-name", name)
          ),
          class = "product-card-button"
        )
      })
    )
  })
  
  # ── Step 3: Observe Product Card Clicks ────────────────────────────────────
  observe({
    req(file_loaded(), ordered_product_data())
    pd <- ordered_product_data()
    
    # Dynamically create an observer for each product button
    lapply(seq_len(nrow(pd)), function(i) {
      observeEvent(input[[paste0("select_product_", i)]], {
        sel_code <- pd$product_code[i]
        
        # For Qualtrics, internal IDs differ: map back from internal 'product' to old_product
        if (file_type() == "qualtrics") {
          cp <- result() %>%
            filter(old_product == sel_code) %>%
            distinct(product) %>%
            pull(product)
          if (length(cp) == 1) {
            selected_product(cp)
          } else {
            selected_product(NULL)
          }
        } else {
          # For Fizz, simply set the reactive selected_product to the code
          selected_product(sel_code)
        }
      })
    })
  })
  
  # ── Step 4: Render Dynamic Text Based on Selected Product ─────────────────
  output$product_info_text <- renderUI({
    req(selected_product(), product_df_data(), result())
    sel_internal <- selected_product()
    dfp          <- product_df_data()
    res          <- result()
    
    # Determine the display name for the selected product
    if (file_type() == "qualtrics") {
      old_idx  <- match(sel_internal, res$product)
      old_code <- if (!is.na(old_idx)) res$old_product[old_idx] else NA_character_
      pidx     <- match(old_code, dfp$product_code)
      user     <- if (!is.na(pidx)) dfp$product_name[pidx] else ""
      
      disp_name <- if (nzchar(user)) {
        # Use custom name if provided
        user
      } else if (!is.na(old_code)) {
        # Otherwise, prefix with "produit_"
        paste0("produit_", old_code)
      } else {
        # Fallback to internal code
        sel_internal
      }
      
    } else {
      # For Fizz: display_name was precomputed
      pd  <- ordered_product_data()
      idx <- match(sel_internal, pd$product_code)
      disp_name <- if (!is.na(idx)) pd$display_name[idx] else sel_internal
    }
    
    # DEBUG: print final display name to console
    cat(">> DEBUG: final disp_name =", disp_name, "\n")
    
    div(
      class = "intro-text",
      HTML(paste0(
        "The selected product is <strong>", disp_name, "</strong>.<br>",
        "Below you will find the score obtained for each descriptor.<br>",
        "Scoring rules: if a descriptor is ranked first, it scores +3; ",
        "second, +2; third, +1."
      ))
    )
  })
  
  # ── Step 5: Render UI for Customizing Number of Panelists ──────────────────
  output$file_info_text4 <- renderUI({
    req(file_loaded())
    tagList(
      div(
        class = "file-info-text",
        style = "text-align:center; margin-top:22px; width:100%;",
        HTML("<em>Customize the <strong>number of panelists</strong> below to tailor your analysis. The default is <strong>12 panelists</strong>.</em>"),
        div(
          style = "display:flex; align-items:center; justify-content:center; margin-top:15px;",
          tags$i(class = "fas fa-users", style = "font-size:24px; margin-right:10px; color:#007436;"),
          numericInput(
            inputId = "num_panelists_selection",
            label   = NULL,
            value   = 12,
            min     = 1,
            width   = "100px"
          )
        )
      )
    )
  })
}
