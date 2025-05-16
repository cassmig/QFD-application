

add_missing_export_cols <- function(df, wanted_cols) {
  if ("color" %in% wanted_cols && !"color" %in% names(df)) {
    df <- df %>% 
      dplyr::left_join(
        descriptors_df_data() %>% 
          dplyr::select(original_attribute, color),
        by = c("attribute" = "original_attribute")
      )
  }
  if ("new_name_attribute" %in% wanted_cols && !"new_name_attribute" %in% names(df)) {
    df <- df %>% 
      dplyr::left_join(
        descriptors_df_data() %>% 
          dplyr::select(original_attribute, attribute) %>% 
          dplyr::rename(new_name_attribute = attribute),
        by = c("attribute" = "original_attribute")
      )
  }
  df
}


download_result <- function(input, output, session) {
  
  ui_w_px <- reactive( session$clientData$output_circle_plot_width  )
  ui_h_px <- reactive( session$clientData$output_circle_plot_height )
  

# UI dynamique pour les colonnes exportables
  output$export_column_selector <- renderUI({
    req(result())
    
    # ----- 1) mapping ----------------------------------------------------------
    all_cols <- c("attribute", "sum_coefficients",
                  "count", "color",
                  "new_name_attribute")
    col_labels <- c("attribute"           = "Original Descriptor",
                    "sum_coefficients"    = "Score",
                    "count"               = "Frequency",
                    "color"               = "Color",
                    "new_name_attribute"  = "Renamed Descriptor")
    
    tagList(
      # -------- Ligne 1 : colonnes -------------------------------------------
      tags$strong("Select columns to include:"),
      div(
        checkboxGroupInput(
          inputId  = "export_table_columns",
          label    = NULL,
          choices  = setNames(all_cols, col_labels),
          selected = all_cols,
          inline   = TRUE        
        )
      ),
      # -------- Ligne 2 : produits -------------------------------------------
      tags$strong("Select product(s) to export:"),
      checkboxGroupInput(
        inputId  = "export_selected_tables",
        label    = NULL,
        choices  = sort(unique(result()$product)),
        selected = sort(unique(result()$product)),
        inline   = TRUE           # affiche en ligne ; wrap auto si trop long
      )
    )
  })
  
  output$export_download_table <- downloadHandler(
    filename = function() {
      ext <- switch(input$export_table_format,
                    "Excel (.xlsx)" = ".xlsx",
                    "CSV (.csv)" = ".zip",
                    "PNG (.png)" = "zip",   # PNG multiple → zip
                    "PDF (.pdf)" = "pdf")
      
      paste0("QFD_Results_", Sys.Date(),".", ext)
    },
    
    content = function(file) {
      req(result(), input$export_table_columns, input$export_selected_tables)
      
      if (input$export_table_format == "Excel (.xlsx)") {
        library(openxlsx)
        wb <- createWorkbook()
        
        for (prod in input$export_selected_tables) {
          df <- result() %>% 
            filter(product == prod)
          
          # 🔧 Ajouter les colonnes manquantes si besoin
          if ("color" %in% input$export_table_columns && !"color" %in% colnames(df)) {
            df <- df %>%
              left_join(descriptors_df_data() %>% select(original_attribute, color),
                        by = c("attribute" = "original_attribute"))
          }
          
          if ("new_name_attribute" %in% input$export_table_columns && !"new_name_attribute" %in% colnames(df)) {
            df <- df %>%
              left_join(descriptors_df_data() %>%
                          select(original_attribute, attribute) %>%
                          rename(new_name_attribute = attribute),
                        by = c("attribute" = "original_attribute"))
          }
          
          df <- df %>% select(all_of(input$export_table_columns))
          sheet_name <- substr(prod, 1, 31)
          
          addWorksheet(wb, sheetName = sheet_name)
          writeData(wb, sheet = sheet_name, df)
          
          # 🎨 Mise en couleur
          if ("color" %in% input$export_table_columns) {
            color_col_index <- which(input$export_table_columns == "color")
            for (i in seq_len(nrow(df))) {
              cell <- paste0(LETTERS[color_col_index], i + 1)
              fill_color <- df$color[i]
              if (!is.na(fill_color)) {
                addStyle(wb, sheet = sheet_name,
                         style = createStyle(fgFill = fill_color),
                         rows = i + 1, cols = color_col_index,
                         gridExpand = FALSE, stack = TRUE)
              }
            }
          }
        }
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
      
      else if (input$export_table_format == "CSV (.csv)") {
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        for (prod in input$export_selected_tables) {
          df <- result() %>%
            filter(product == prod)
          
          # Ajout éventuel des colonnes manquantes
          if ("color" %in% input$export_table_columns && !"color" %in% colnames(df)) {
            df <- df %>%
              left_join(descriptors_df_data() %>% select(original_attribute, color),
                        by = c("attribute" = "original_attribute"))
          }
          if ("new_name_attribute" %in% input$export_table_columns && !"new_name_attribute" %in% colnames(df)) {
            df <- df %>%
              left_join(descriptors_df_data() %>%
                          select(original_attribute, attribute) %>%
                          rename(new_name_attribute = attribute),
                        by = c("attribute" = "original_attribute"))
          }
          
          df <- df %>% select(all_of(input$export_table_columns))
        csv_path <- file.path(temp_dir, paste0(gsub("\\s+", "_", prod), ".csv"))
        readr::write_csv(df, csv_path)
      }
      
      zip::zipr(zipfile = file, files = list.files(temp_dir, full.names = TRUE), recurse = FALSE)
      }
      
      else if (input$export_table_format == "PNG (.png)") {
        
        tmp_dir <- tempdir()
        files   <- character(0)
        
        for (prod in input$export_selected_tables) {
          
          ## --- a) data.frame préparé ------------------------------------------
          df <- result() %>%
            dplyr::filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            dplyr::select(all_of(input$export_table_columns))
          
          ## --- b) matrice de couleurs pour la colonne "color" ------------------
          n_row <- nrow(df); n_col <- ncol(df)
          fill_mat <- matrix("white", n_row, n_col)
          if ("color" %in% names(df)) {
            col_idx <- which(names(df) == "color")
            fill_mat[, col_idx] <- ifelse(is.na(df$color) | df$color == "", "white", df$color)
          }
          
          ## --- c) thème + grob -------------------------------------------------
          tt  <- gridExtra::ttheme_minimal(
            core    = list(bg_params = list(fill = fill_mat, col = NA),
                           fg_params = list(col = "black", fontsize = 10)),
            colhead = list(bg_params = list(fill = "grey90"),
                           fg_params = list(fontface = "bold"))
          )
          tbl <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
          
          ## --- d) taille en pouces puis en pixels -----------------------------
          h_in <- grid::convertHeight(sum(tbl$heights), "in", valueOnly = TRUE)
          w_in <- grid::convertWidth (sum(tbl$widths ), "in", valueOnly = TRUE)
          dpi  <- 150
          
          width_px  <- ceiling(w_in * dpi) + 40   # marge
          height_px <- ceiling(h_in * dpi) + 40
          
          ## Limite raisonnable (10px) pour éviter l’overflow mémoire
          limit_px <- 10000
          scale_fac <- max(width_px  / limit_px,
                           height_px / limit_px, 1)
          width_px  <- round(width_px  / scale_fac)
          height_px <- round(height_px / scale_fac)
          
          ## --- e) périphérique CairoPNG ---------------------------------------
          png_path <- file.path(tmp_dir,
                                paste0(gsub("[^a-zA-Z0-9]", "_", prod), ".png"))
          
          Cairo::CairoPNG(
            filename = png_path,
            width  = width_px,
            height = height_px,
            bg     = "white",   # fond blanc (le PNG gère bien la transparence aussi)
            res    = dpi
          )
          grid::grid.draw(tbl)
          dev.off()
          
          files <- c(files, png_path)
        }
        
        zip::zip(zipfile = file, files = files, mode = "cherry-pick")
      }
      
      else if (input$export_table_format == "PDF (.pdf)") {
        
        ## ------------------------------------------------------------------
        ## 0. déterminer la taille max des tables
        ## ------------------------------------------------------------------
        dims <- purrr::map(input$export_selected_tables, function(prod) {
          df <- result() %>%
            dplyr::filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            dplyr::select(all_of(input$export_table_columns))
          
          n_row <- nrow(df); n_col <- ncol(df)
          fill_mat <- matrix("white", n_row, n_col)
          if ("color" %in% colnames(df)) {
            col_idx <- which(colnames(df) == "color")
            fill_mat[, col_idx] <- ifelse(is.na(df$color) | df$color == "", "white", df$color)
          }
          
          tt  <- gridExtra::ttheme_minimal(
            core    = list(bg_params = list(fill = fill_mat, col = NA)),
            colhead = list(bg_params = list(fill = "grey90"),
                           fg_params = list(fontface = "bold"))
          )
          tbl <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
          
          list(tbl = tbl,
               w   = grid::convertWidth (sum(tbl$widths ), "in", valueOnly = TRUE),
               h   = grid::convertHeight(sum(tbl$heights), "in", valueOnly = TRUE))
        })
        
        ## ------------------------------------------------------------------
        ## 1. taille du device PDF
        ## ------------------------------------------------------------------
        margin_in      <- 0.25      # marge extérieure
        title_space_in <- 0.45      # hauteur réservée au titre
        pdf_w <- max(purrr::map_dbl(dims, "w")) + 2 * margin_in
        pdf_h <- max(purrr::map_dbl(dims, "h")) + title_space_in + 2 * margin_in
        
        cairo_pdf(
          file,
          width   = pdf_w,
          height  = pdf_h,
          onefile = TRUE
        )        
        ## ------------------------------------------------------------------
        ## 2. ordre des pages : produit_1, produit_2, …
        ## ------------------------------------------------------------------
        prod_list <- input$export_selected_tables[
          order(as.numeric(gsub("\\D", "", input$export_selected_tables)))
        ]
        
        ## ------------------------------------------------------------------
        ## 3. une page par produit
        ## ------------------------------------------------------------------
        for (prod in prod_list) {
          
          tbl <- dims[[which(input$export_selected_tables == prod)]]$tbl
          grid::grid.newpage()
          
          ## --- titre centré, à title_space_in - 0.1o du haut -------------
          grid::grid.text(
            paste("Result Table for", prod),
            x = grid::unit(0.5, "npc"),
            y = grid::unit(pdf_h - margin_in - 0.1, "in"),   # 0.1sous la marge
            gp = grid::gpar(fontsize = 14, fontface = "bold"),
            just = "centre"
          )
          
          ## --- viewport pour le tableau sous le titre ----------------------
          grid::pushViewport(
            grid::viewport(
              x      = grid::unit(margin_in, "in"),
              y      = grid::unit(margin_in, "in"),
              just   = c("left", "bottom"),
              width  = grid::unit(pdf_w - 2 * margin_in, "in"),
              height = grid::unit(pdf_h - 2 * margin_in - title_space_in, "in")
            )
          )
          grid::grid.draw(tbl)
          grid::popViewport()
        }
        
        dev.off()
      }
      
  }
)
  
  output$export_axes_selector <- renderUI({
    req(num_puissance_types())
    axis_vals   <- paste0("A", seq_len(num_puissance_types()))
    axis_labels <- paste("Axis", seq_len(num_puissance_types()))
    checkboxGroupInput(
      inputId  = "plot_axes_selected",
      label    = "Choose axis type(s):",
      choices  = setNames(axis_vals, axis_labels),
      selected = axis_vals,
      inline   = FALSE
    )
  })
  
  # opérateur “coalesce” pratique
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  output$download_plot <- downloadHandler(
    
    ## ── 1) NOM DE FICHIER ───────────────────────────────────────────────
    filename = function() {
      prods <- input$plot_export_products
      if (is.null(prods) || length(prods) == 0)
        prods <- unique(result()$product)
      
      ext <- if (length(prods) > 1) ".zip" else ".png"
      paste0("circle_plot_", Sys.Date(), ext)
    },
    
    ## ── 2) CONTENU ──────────────────────────────────────────────────────
    content = function(file) {
      req(current_plot(), ui_w_px(), ui_h_px())
      
      w_px <- ui_w_px()*2
      h_px <- ui_h_px()*2
      bg   <- input$png_bg_choice %||% "white"
      screen_dpi <- 72
      
      scale_fac <- 2
      
      
      ## axes d’intensité éventuels
      axes_sel <- if (identical(input$plot_show_axes, "yes"))
        (input$plot_axes_selected %||% character(0)) else character(0)
      
      
      ## liste de produits à exporter
      prods <- input$plot_export_products
      if (is.null(prods) || length(prods) == 0)
        prods <- unique(result()$product)
      
      ## =========== CAS A) PLUSIEURS PRODUITS → ZIP ======================
      if (length(prods) > 1) {
        
        tmp_dir <- tempfile(); dir.create(tmp_dir)
        outputs <- character()
        
        for (prod in prods) {
          
          ## 1) construire axis_params_list avec valeurs par défaut
          axis_params_list <- lapply(axes_sel, function(ax) {
            list(
              title     = input[[paste0("axis_title_",   ax)]] %||% ax,
              color     = input[[paste0("axis_color_",   ax)]] %||% "#000000",
              offset    = input[[paste0("axis_offset_",  ax)]] %||% 0,
              thickness  = (input[[paste0("axis_thickness_", ax)]] %||% 1) * scale_fac,
              title_size = 5 * scale_fac
            )
          })
          axis_params_list <- setNames(axis_params_list, axes_sel)
          
          ## 2) générer le plot pour ce produit
          plt <- make_circle_plot(
            product                 = prod,
            result_df               = result(),
            descriptors_df          = descriptors_df_data(),
            puissance_df            = puissance_table(),
            num_descriptors         = input$num_descriptors,
            equalDescriptors        = input$equalDescriptors,
            firstDoubleSecond       = input$firstDoubleSecond,
            font_descriptor         = input$font_descriptor,
            color_descriptor        = input$color_descriptor,
            font_size_descriptor    = input$font_size_descriptor *2,
            plot_horizontal_shift   = input$plot_horizontal_shift,
            label_horizontal_shift  = input$label_horizontal_shift,
            show_labels             = input$show_descriptor_labels,
            selected_axes           = axes_sel,
            axis_params_list        = axis_params_list 
          )
          
          ## 3) sauvegarder le PNG
          png_path <- file.path(
            tmp_dir,
            paste0(gsub("\\s+", "_", prod), ".png")
          )
          ragg::agg_png(png_path,
                        width = w_px, height = h_px,
                        units = "px", res = screen_dpi,
                        background = bg)
          print(plt); dev.off()
          outputs <- c(outputs, png_path)
        }
        
        zip::zipr(zipfile = file, files = outputs, recurse = FALSE)
      }
      
      ## =========== CAS B) UN SEUL PRODUIT → PNG DIRECT ==================
      else {
        ragg::agg_png(file,
                      width = w_px, height = h_px,
                      units = "px", res = screen_dpi,
                      background = bg)
        print(current_plot())
        dev.off()
      }
    }
  )
  
  
  
  ## ------------------------------------------------------------
  ## UI: remplir la liste des produits pour l’export du plot
  ## ------------------------------------------------------------
  observeEvent(result(), ignoreNULL = TRUE, {
    prods <- sort(unique(result()$product))
    
    default_sel <- selected_product()

    ## ⇢ ici on coche TOUS les produits par défaut
    default_sel <- prods  
    
    updateCheckboxGroupInput(
      session,
      inputId = "plot_export_products",
      choices = prods,
      selected = default_sel,
      inline=TRUE
    )
  })
  
  
  

  
  
}
