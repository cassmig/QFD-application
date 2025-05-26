# 📌 product_selection.R - Manages product selection for analysis
# This module allows users to select a product from a list of available products and displays relevant information.

product_selection <- function(input, output, session) {
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 1: Retrieve and Order Product Data Dynamically
  # --------------------------------------------------------------------------------------------------------#
  ordered_product_data <- reactive({
    req(product_df_data())
    df <- product_df_data()
    
    # Build display_name for UI cards (Fizz & Qualtrics share same "display_name")
    df <- df %>% mutate(
      display_name = dplyr::case_when(
        nzchar(product_name) ~ product_name,
        file_type() == "qualtrics"   ~ paste0("produit_", product_code),
        TRUE                          ~ product_code
      )
    )
    # Debug
    print(head(df %>% select(product_code, product_name, display_name)))
    
    # If Fizz, sort numerically on the code suffix
    if (file_type() == "fizz") {
      nums <- as.numeric(gsub("\\D", "", df$product_code))
      if (all(!is.na(nums))) df <- df[order(nums), ]
    }
    df
  })
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 2: Render the UI for Product Selection
  # --------------------------------------------------------------------------------------------------------#
  output$productSelectionUI <- renderUI({
    req(file_loaded(), ordered_product_data())
    pd <- ordered_product_data()
    div(class = "product-selection-grid",
        lapply(seq_len(nrow(pd)), function(i) {
          code <- pd$product_code[i]
          name <- pd$display_name[i]
          img  <- product_images[[code]]
          image_tag <- if (!is.null(img) && file.exists(img$datapath)) {
            uri <- base64enc::dataURI(
              file = img$datapath,
              mime = img$mime
            )
            tags$img(src = uri, class = "product-card-image")
          } else {
            div(class = "no-image", "No image")
          }
          actionButton(
            inputId = paste0("select_product_", i),
            label   = div(class = "product-card", image_tag,
                          div(class = "product-card-name", name)),
            class   = "product-card-button"
          )
        })
    )
  })
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 3: Observe Clicks on Product Cards and Update Selected Product
  # --------------------------------------------------------------------------------------------------------#
  observe({
    req(file_loaded(), ordered_product_data())
    pd <- ordered_product_data()
    lapply(seq_len(nrow(pd)), function(i) {
      observeEvent(input[[paste0("select_product_", i)]], {
        sel_code <- pd$product_code[i]
        if (file_type() == "qualtrics") {
          cp <- result() %>% filter(old_product == sel_code) %>%
            distinct(product) %>% pull(product)
          if (length(cp) == 1) selected_product(cp)
          else {
            selected_product(NULL)
          }
        } else {
          selected_product(sel_code)
        }
      })
    })
  })
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 4: Render Dynamic Text Based on Selected Product
  # --------------------------------------------------------------------------------------------------------#
  output$product_info_text <- renderUI({
    req(selected_product(), product_df_data(), result())
    sel_internal <- selected_product()
    dfp          <- product_df_data()
    res          <- result()
    
    # For Qualtrics: map back to original code, then override if user changed, else produit_<code>
    if (file_type() == "qualtrics") {
      old_idx <- match(sel_internal, res$product)
      old_code <- if (!is.na(old_idx)) res$old_product[old_idx] else NA_character_
      # see if user provided override
      pidx <- match(old_code, dfp$product_code)
      user  <- if (!is.na(pidx)) dfp$product_name[pidx] else ""
      if (nzchar(user)) {
        disp_name <- user
      } else if (!is.na(old_code)) {
        disp_name <- paste0("produit_", old_code)
      } else {
        disp_name <- sel_internal
      }
    } else {
      # Fizz: simpler, use product_df_data() mapping
      pd  <- ordered_product_data()
      idx <- match(sel_internal, pd$product_code)
      if (!is.na(idx)) disp_name <- pd$display_name[idx] else disp_name <- sel_internal
    }
    
    # Debug
    cat(">> DEBUG: final disp_name =", disp_name, "\n")
    
    div(class = "intro-text",
        HTML(paste0(
          "The selected product is <strong>", disp_name, "</strong>.<br>",
          "Below you will find the score obtained for each descriptor.<br>",
          "The score is obtained as follows: when the descriptor is cited first, it gets a score of +3; ",
          "when it's selected second, it gets +2; and when it's selected third, it gets +1.<br>"
        ))
    )
  })
  
  # --------------------------------------------------------------------------------------------------------#
  ### 📌 Step 5: Render UI for Customizing the Number of Panelists
  # --------------------------------------------------------------------------------------------------------#
  output$file_info_text4 <- renderUI({
    req(file_loaded())
    tagList(
      div(class = "file-info-text", style = "text-align:center; margin-top:22px; width:100%;",
          HTML("<em>Customize the <strong>number of panelists</strong> below to tailor your analysis. The default is set to <strong>12 panelists</strong>.</em>"),
          div(style = "display:flex; align-items:center; justify-content:center; margin-top:15px;",
              tags$i(class = "fas fa-users", style = "font-size:24px; margin-right:10px;color:#007436;"),
              numericInput("num_panelists_selection", NULL, value = 12, min = 1, width = '100px')
          )
      )
    )
  })
}
