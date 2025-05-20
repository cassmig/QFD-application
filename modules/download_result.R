

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
  # ─────────────────────────────────────────────────────────────────────────────
  # Helpers
  # ─────────────────────────────────────────────────────────────────────────────
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Renvoie pour chaque code produit (ex. "produit_1") son display name
  make_display_names <- function(prods) {
    tbl <- result()  # data.frame reactive contenant colonnes product et new_name_product et old_product
    vapply(prods, function(p) {
      # 1) si l'utilisateur a fourni un nouveau nom, on l'affiche
      newnm <- tbl$new_name_product[match(p, tbl$product)]
      if (!is.na(newnm) && nzchar(newnm) && newnm != p) {
        return(newnm)
      }
      # 2) sinon, si c'est Qualtrics on affiche "produit_<old_product>"
      if (file_type() == "qualtrics") {
        oldp <- tbl$old_product[match(p, tbl$product)]
        return(paste0("produit_", oldp))
      }
      # 3) sinon (Fizz), on renvoie p tel quel ("produit_1", etc.)
      p
    }, character(1))
  }
  
  
  # ─────────────────────────────────────────────────────────────────────────────
  # UI dynamique pour l’export du tableau de résultats
  # ─────────────────────────────────────────────────────────────────────────────
  ui_w_px <- reactive( session$clientData$output_circle_plot_width  )
  ui_h_px <- reactive( session$clientData$output_circle_plot_height )
  


  # 1) Colonnes disponibles
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
    
    # 2) Produits dispo & leurs display names
    prods <- sort(unique(result()$product))
    disp  <- make_display_names(prods)
    
    
    tagList(
      
      # -------- Ligne 2 : produits -------------------------------------------
      tags$strong("Select product(s) to export:"),
      checkboxGroupInput(
        inputId  = "export_selected_tables",
        label    = NULL,
        choices  = setNames(prods, disp),
        selected = prods,
        inline   = TRUE
      ),
      # -------- Ligne 1 : colonnes -------------------------------------------
      tags$strong("Select columns to include:"),
      checkboxGroupInput(
          inputId  = "export_table_columns",
          label    = NULL,
          choices  = setNames(all_cols, col_labels),
          selected = all_cols,
          inline   = TRUE        
        )
    )
  })
  
  # On rafraîchit aussi dynamiquement en cas de changement de result()
  observeEvent(result(), ignoreNULL = TRUE, {
    prods <- sort(unique(result()$product))
    disp  <- make_display_names(prods)
    updateCheckboxGroupInput(
      session,
      inputId  = "export_selected_tables",
      choices  = setNames(prods, disp),
      selected = prods
    )
  })
  
  # ─────────────────────────────────────────────────────────────────────────────
  # Download handler pour le tableau de résultats
  # ─────────────────────────────────────────────────────────────────────────────
  
  
  
  
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

      # Cas Excel
      if (input$export_table_format == "Excel (.xlsx)") {
        library(openxlsx)
        wb <- createWorkbook()
        
        # Récupère les produits sélectionnés et leurs display names
        prods      <- input$export_selected_tables
        disp_names <- make_display_names(prods)
        
        # Boucle par indice pour associer prod ↔ disp_name
        for (i in seq_along(prods)) {
          prod       <- prods[i]
          sheet_name <- substr(disp_names[i], 1, 31)  # Tronque à 31 caractères
          
          # Prépare le data.frame
          df <- result() %>%
            filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            select(all_of(input$export_table_columns))
          
          # Ajout de la feuille et écriture des données
          addWorksheet(wb, sheetName = sheet_name)
          writeData(wb, sheet = sheet_name, df)
          
          # 🎨 Mise en couleur si colonne "color" sélectionnée
          if ("color" %in% input$export_table_columns) {
            color_col_index <- which(input$export_table_columns == "color")
            for (j in seq_len(nrow(df))) {
              fill_color <- df$color[j]
              if (!is.na(fill_color) && nzchar(fill_color)) {
                addStyle(
                  wb, sheet = sheet_name,
                  style = createStyle(fgFill = fill_color),
                  rows = j + 1, cols = color_col_index,
                  gridExpand = FALSE, stack = TRUE
                )
              }
            }
          }
        }
        
        # Sauvegarde
        saveWorkbook(wb, file, overwrite = TRUE)
      }
      
      else if (input$export_table_format == "CSV (.csv)") {
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        # Récupère les produits sélectionnés et leurs display names
        prods      <- input$export_selected_tables
        disp_names <- make_display_names(prods)
        
        for (i in seq_along(prods)) {
          prod      <- prods[i]
          disp_name <- disp_names[i]
          # Prépare le df avec colonnes manquantes
          df <- result() %>%
            filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            select(all_of(input$export_table_columns))
          
          # Génère un nom de fichier sûr
          safe_name <- gsub("[^[:alnum:] ]", "_", disp_name)
          safe_name <- gsub("\\s+", "_", safe_name)
          csv_path  <- file.path(temp_dir, paste0(safe_name, ".csv"))
          
          readr::write_csv(df, csv_path)
        }
        
        # Zippe tous les CSV générés
        zip::zipr(zipfile = file, files = list.files(temp_dir, full.names = TRUE), recurse = FALSE)
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
        ## 2. ordre des pages
        ## ------------------------------------------------------------------
        prod_list  <- input$export_selected_tables
        disp_names <- make_display_names(prod_list)
        # si besoin de trier selon un suffixe numérique, on peut :
        # idx_order <- order(as.numeric(gsub("\\D", "", prod_list)))
        # prod_list  <- prod_list[idx_order]
        # disp_names <- disp_names[idx_order]
        
        ## ------------------------------------------------------------------
        ## 3. une page par produit
        ## ------------------------------------------------------------------
        for (i in seq_along(prod_list)) {
          prod      <- prod_list[i]
          disp_name <- disp_names[i]
          tbl       <- dims[[which(prod_list == prod)]]$tbl
          
          grid::grid.newpage()
          
          ## --- titre centré -----------------------------------------------
          grid::grid.text(
            disp_name,
            x  = grid::unit(0.5, "npc"),
            y  = grid::unit(pdf_h - margin_in - 0.1, "in"),
            gp = grid::gpar(fontsize = 14, fontface = "bold"),
            just = "centre"
          )
          
          ## --- viewport pour le tableau -------------------------------
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
  
  
  
  # Votre download_handler
  output$download_plot <- downloadHandler(
    
    ## ── 1) NOM DE FICHIER ───────────────────────────────────────────────
    filename = function() {
      prods <- input$plot_export_products
      if (is.null(prods) || length(prods) == 0) {
        prods <- unique(result()$product)
      }
      if (length(prods) > 1) {
        return(paste0("circle_plot_", Sys.Date(), ".zip"))
      }

      # Un seul produit → PNG
      base <- make_display_names(prods)
      safe <- gsub("[^[:alnum:] ]", "_", base)
      paste0(safe, ".png")
    },
    
    ## ── 2) CONTENU ──────────────────────────────────────────────────────
    content = function(file) {
      req(current_plot(), ui_w_px(), ui_h_px())
      
      w_px      <- ui_w_px() * 2
      h_px      <- ui_h_px() * 2
      bg        <- input$png_bg_choice %||% "white"
      screen_dpi <- 72
      scale_fac <- 2
      
      # Quels axes afficher ?
      axes_sel <- if (identical(input$plot_show_axes, "yes")) {
        input$plot_axes_selected %||% character(0)
      } else {
        character(0)
      }
      
      # Liste des produits à exporter
      prods <- input$plot_export_products
      if (is.null(prods) || length(prods) == 0) {
        prods <- unique(result()$product)
      }
      
      # On construit les display names pour tous les produits
      file_bases <- make_display_names(prods)
      
      ## =========== CAS A) PLUSIEURS PRODUITS → ZIP ======================
      if (length(prods) > 1) {
        
        tmp_dir <- tempfile(); dir.create(tmp_dir)
        outputs <- character()
        
        for (i in seq_along(prods)) {
          prod      <- prods[i]
          base_name <- file_bases[i]
          
          # 1) axis_params_list par défaut
          axis_params_list <- lapply(axes_sel, function(ax) {
            list(
              title      = input[[paste0("axis_title_", ax)]]   %||% ax,
              color      = input[[paste0("axis_color_", ax)]]   %||% "#000000",
              offset     = input[[paste0("axis_offset_", ax)]]  %||% 0,
              thickness  = (input[[paste0("axis_thickness_", ax)]] %||% 1) * scale_fac,
              title_size = 4 * scale_fac
            )
          })
          names(axis_params_list) <- axes_sel
          
          # 2) générer le plot
          plt <- make_circle_plot(
            product               = prod,
            result_df             = result(),
            descriptors_df        = descriptors_df_data(),
            puissance_df          = puissance_table(),
            num_descriptors       = input$num_descriptors,
            equalDescriptors      = input$equalDescriptors,
            firstDoubleSecond     = input$firstDoubleSecond,
            font_descriptor       = input$font_descriptor,
            color_descriptor      = input$color_descriptor,
            font_size_descriptor  = input$font_size_descriptor * 2,
            plot_horizontal_shift = input$plot_horizontal_shift,
            label_horizontal_shift= input$label_horizontal_shift,
            show_labels           = input$show_descriptor_labels,
            selected_axes         = axes_sel,
            axis_params_list      = axis_params_list,
            axis_margin_data      = 10,
            spacing_scale         = 0.5,
            axis_spacing_add      = 30,
            value_text_size       = 6
          )
          
          # 3) sauvegarder le PNG
          fname    <- paste0(gsub("[^[:alnum:] ]", "_", base_name), ".png")
          png_path <- file.path(tmp_dir, fname)
          ragg::agg_png(
            filename   = png_path,
            width      = w_px, height = h_px,
            units      = "px", res    = screen_dpi,
            background = bg
          )
          print(plt); dev.off()
          outputs <- c(outputs, png_path)
        }
        
        zip::zipr(zipfile = file, files = outputs, recurse = FALSE)
      }
      
      ## =========== CAS B) UN SEUL PRODUIT → PNG DIRECT ==================
      else {
        # on reconstruit axis_params_list
        axis_params_list <- lapply(axes_sel, function(ax) {
          list(
            title      = input[[paste0("axis_title_", ax)]]   %||% ax,
            color      = input[[paste0("axis_color_", ax)]]   %||% "#000000",
            offset     = input[[paste0("axis_offset_", ax)]]  %||% 0,
            thickness  = (input[[paste0("axis_thickness_", ax)]] %||% 1) * scale_fac,
            title_size = 4 * scale_fac
          )
        })
        names(axis_params_list) <- axes_sel
        
        plt_png <- make_circle_plot(
          product               = prods,
          result_df             = result(),
          descriptors_df        = descriptors_df_data(),
          puissance_df          = puissance_table(),
          num_descriptors       = input$num_descriptors,
          equalDescriptors      = input$equalDescriptors,
          firstDoubleSecond     = input$firstDoubleSecond,
          font_descriptor       = input$font_descriptor,
          color_descriptor      = input$color_descriptor,
          font_size_descriptor  = input$font_size_descriptor * 2,
          plot_horizontal_shift = input$plot_horizontal_shift,
          label_horizontal_shift= input$label_horizontal_shift,
          show_labels           = input$show_descriptor_labels,
          selected_axes         = axes_sel,
          axis_params_list      = axis_params_list,
          axis_margin_data      = 10,
          spacing_scale         = 0.5,
          axis_spacing_add      = 30,
          value_text_size       = 6
        )
        
        ragg::agg_png(
          filename   = file,
          width      = w_px, height = h_px,
          units      = "px", res    = screen_dpi,
          background = bg
        )
        print(plt_png); dev.off()
      }
    }
  )
  
  
  
  
  ## ------------------------------------------------------------
  ## UI: remplir la liste des produits pour l’export du plot
  ## ------------------------------------------------------------
  observeEvent(result(), ignoreNULL = TRUE, {
    prods <- sort(unique(result()$product))
    disp  <- make_display_names(prods)
    updateCheckboxGroupInput(
      session,
      inputId  = "plot_export_products",
      choices  = setNames(prods, disp),
      selected = prods,
      inline   = TRUE
    )
  })  
  

  
  
}
