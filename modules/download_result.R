# ── modules/download_result.R ───────────────────────────────────────────────
# This module provides dynamic UI and server logic for exporting the results
# table and circle plot. It supports multiple formats (Excel, CSV, PNG, PDF, SVG)
# and handles combining multiple products into zip archives when necessary.

# ── Helper: Add missing columns to a data frame for export ─────────────────
# Ensures that if the user requests "color" or "new_name_attribute" but those
# columns are not present, we join them from descriptors_df_data().
add_missing_export_cols <- function(df, wanted_cols) {
  # If "color" is requested and df lacks it, join from descriptors mapping
  if ("color" %in% wanted_cols && !"color" %in% names(df)) {
    df <- df %>%
      dplyr::left_join(
        descriptors_df_data() %>%
          dplyr::select(original_attribute, color),
        by = c("attribute" = "original_attribute")
      )
  }
  
  # If "new_name_attribute" is requested and df lacks it, join from descriptors mapping
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


# ── Main module: download_result ────────────────────────────────────────────
# Defines UI elements for selecting products and columns to export, and handlers
# for exporting the results table and circle plot in different formats.
download_result <- function(input, output, session) {
  # ───────────────────────────────────────────────────────────────────────────
  # Helpers and reactive UI dimension capture
  # ───────────────────────────────────────────────────────────────────────────
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Given a vector of product codes, return their display names
  # 1) If new_name_product exists and differs from code, use it
  # 2) If file_type == "qualtrics", prefix old_product with "produit_"
  # 3) Otherwise (Fizz), return code as-is
  make_display_names <- function(prods) {
    tbl <- result()  # data.frame reactive containing columns product, new_name_product, old_product
    vapply(prods, function(p) {
      # 1) Use user-provided new name if available
      newnm <- tbl$new_name_product[match(p, tbl$product)]
      if (!is.na(newnm) && nzchar(newnm) && newnm != p) {
        return(newnm)
      }
      # 2) If Qualtrics, prefix old_product
      if (file_type() == "qualtrics") {
        oldp <- tbl$old_product[match(p, tbl$product)]
        return(paste0("produit_", oldp))
      }
      # 3) Else (Fizz), return code
      p
    }, character(1))
  }
  
  # Capture width and height (in pixels) of circle_plot output on client side
  ui_w_px <- reactive(session$clientData$output_circle_plot_width)
  ui_h_px <- reactive(session$clientData$output_circle_plot_height)
  
  
  # ── 1) Dynamic UI for selecting products and columns to export ─────────────
  output$export_column_selector <- renderUI({
    req(result())  # Ensure result() is available
    
    # Define all possible columns and their display labels
    all_cols <- c("attribute", "sum_coefficients", "count", "color", "new_name_attribute")
    col_labels <- c(
      attribute          = "Original Descriptor",
      sum_coefficients   = "Score",
      count               = "Frequency",
      color               = "Color",
      new_name_attribute  = "Descriptor"
    )
    
    # Define desired column order in UI (internal names)
    cols_ui_order <- c(
      "sum_coefficients",    # Score
      "color",               # Color
      "new_name_attribute",  # Descriptor
      "attribute",           # Original Descriptor
      "count"                # Frequency
    )
    
    # Get the list of products (unique codes) and sort
    prods <- sort(unique(result()$product))
    # Get their display names for UI labels
    disp  <- make_display_names(prods)
    
    tagList(
      tags$strong("Select product(s) to export:"),
      checkboxGroupInput(
        inputId  = "export_selected_tables",
        label    = NULL,
        choices  = setNames(prods, disp),
        selected = prods,
        inline   = TRUE
      ),
      
      tags$strong("Select columns to include:"),
      checkboxGroupInput(
        inputId  = "export_table_columns",
        label    = NULL,
        choices  = setNames(cols_ui_order, col_labels[cols_ui_order]),
        # Default selected columns
        selected = c("new_name_attribute", "sum_coefficients", "color"),
        inline   = TRUE        
      )
    )
  })
  
  # Ensure default columns stay selected if result() changes
  observe({
    updateCheckboxGroupInput(
      session,
      "export_table_columns",
      selected = c("new_name_attribute", "sum_coefficients", "color")
    )
  })
  
  # When result() changes, update product choices and selection
  observeEvent(result(), ignoreNULL = TRUE, {
    prods <- sort(unique(result()$product))
    disp  <- make_display_names(prods)
    updateCheckboxGroupInput(
      session,
      "export_selected_tables",
      choices  = setNames(prods, disp),
      selected = prods
    )
  })
  
  
  # ── 2) Download handler for exporting results table ────────────────────────
  output$export_download_table <- downloadHandler(
    
    # Generate filename based on chosen format and number of products
    filename = function() {
      ext <- switch(input$export_table_format,
                    "Excel (.xlsx)" = ".xlsx",
                    "CSV (.csv)"   = ".zip",
                    "PDF (.pdf)"   = if (length(input$export_selected_tables) > 1) ".zip" else ".pdf",
                    "PNG (.png)"   = if (length(input$export_selected_tables) > 1) ".zip" else ".png")
      paste0("QFD_Results_", Sys.Date(), ext)
    },
    
    # Content: create and write file(s) based on selected format
    content = function(file) {
      req(result(), input$export_table_columns, input$export_selected_tables)
      
      # ── Case: Excel (.xlsx) ───────────────────────────────────────────────
      if (input$export_table_format == "Excel (.xlsx)") {
        library(openxlsx)
        wb <- createWorkbook()
        
        # Get selected products and their display names
        prods      <- input$export_selected_tables
        disp_names <- make_display_names(prods)
        
        # Column labels mapping for Excel sheets
        col_labels <- c(
          attribute            = "Original Descriptor",
          new_name_attribute   = "Descriptor",
          sum_coefficients     = "Score",
          count                = "Frequency",
          color                = "Color"
        )
        desired_order <- c("Original Descriptor", "Descriptor", "Score", "Frequency", "Color")
        
        # Loop over each product and create one sheet per product
        for (i in seq_along(prods)) {
          prod       <- prods[i]
          sheet_name <- substr(disp_names[i], 1, 31)  # Sheet names limited to 31 chars
          
          # Filter result for this product, add missing columns, select and rename
          df <- result() %>%
            filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            select(all_of(input$export_table_columns)) %>%
            rename_with(~ col_labels[.x], all_of(input$export_table_columns)) %>%
            select(intersect(desired_order, names(.)))
          
          # Add worksheet and write data
          addWorksheet(wb, sheetName = sheet_name)
          writeData(wb, sheet = sheet_name, df)
          
          # If "Color" column is included, apply fill color per cell
          if ("color" %in% input$export_table_columns) {
            color_col_index <- which(names(df) == "Color")
            for (j in seq_len(nrow(df))) {
              fill_color <- df[["Color"]][j]
              if (!is.na(fill_color) && nzchar(fill_color)) {
                addStyle(
                  wb, sheet = sheet_name,
                  style      = createStyle(fgFill = fill_color),
                  rows       = j + 1,  # offset by 1 for header row
                  cols       = color_col_index,
                  gridExpand = FALSE,
                  stack      = TRUE
                )
              }
            }
          }
        }
        
        saveWorkbook(wb, file, overwrite = TRUE)
      }
      
      # ── Case: CSV (.csv) ───────────────────────────────────────────────────
      else if (input$export_table_format == "CSV (.csv)") {
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        prods      <- input$export_selected_tables
        disp_names <- make_display_names(prods)
        
        col_labels <- c(
          attribute            = "Original Descriptor",
          new_name_attribute   = "Descriptor",
          sum_coefficients     = "Score",
          count                = "Frequency",
          color                = "Color"
        )
        desired_order <- unname(col_labels)
        
        for (i in seq_along(prods)) {
          prod      <- prods[i]
          disp_name <- disp_names[i]
          
          df <- result() %>%
            filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            select(all_of(input$export_table_columns)) %>%
            rename_with(~ col_labels[.x], all_of(input$export_table_columns)) %>%
            select(intersect(desired_order, names(.)))
          
          # Create a safe filename by replacing non-alphanumeric chars
          safe_name <- gsub("[^[:alnum:] ]", "_", disp_name)
          safe_name <- gsub("\\s+", "_", safe_name)
          csv_path  <- file.path(temp_dir, paste0(safe_name, ".csv"))
          
          readr::write_csv(df, csv_path)
        }
        
        # Zip all generated CSVs into a single archive
        zip::zipr(zipfile = file, files = list.files(temp_dir, full.names = TRUE), recurse = FALSE)
      }
      
      # ── Case: PNG (.png) ──────────────────────────────────────────────────
      else if (input$export_table_format == "PNG (.png)") {
        tmp_dir <- tempfile()
        dir.create(tmp_dir)
        outputs <- character()
        
        prods      <- input$export_selected_tables
        disp_names <- make_display_names(prods)
        
        col_labels <- c(
          attribute            = "Original Descriptor",
          new_name_attribute   = "Descriptor",
          sum_coefficients     = "Score",
          count                = "Frequency",
          color                = "Color"
        )
        desired_order <- unname(col_labels)
        
        for (i in seq_along(prods)) {
          prod      <- prods[i]
          disp_name <- disp_names[i]
          
          df <- result() %>%
            filter(product == prod) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            select(all_of(input$export_table_columns)) %>%
            rename_with(~ col_labels[.x], all_of(input$export_table_columns)) %>%
            select(intersect(desired_order, names(.)))
          
          # Build a fill matrix for the "Color" column
          n_row <- nrow(df); n_col <- ncol(df)
          fill_mat <- matrix("white", n_row, n_col)
          if ("Color" %in% names(df)) {
            col_idx <- which(names(df) == "Color")
            fill_mat[, col_idx] <- ifelse(is.na(df$Color) | df$Color == "", "white", df$Color)
          }
          
          # Create a themed tableGrob with background colors
          tt <- gridExtra::ttheme_minimal(
            core    = list(bg_params = list(fill = fill_mat, col = NA)),
            colhead = list(bg_params = list(fill = "grey90"), fg_params = list(fontface = "bold"))
          )
          tbl_grob <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
          
          # Safe filename for PNG
          safe_base <- gsub("[^[:alnum:] ]", "_", disp_name)
          safe_base <- gsub("\\s+", "_", safe_base)
          png_path  <- file.path(tmp_dir, paste0(safe_base, ".png"))
          
          # Render to PNG using ragg
          ragg::agg_png(
            filename   = png_path,
            width      = 1200,
            height     = 600,
            units      = "px",
            res        = 150,
            background = "white"
          )
          grid::grid.newpage()
          grid::grid.draw(tbl_grob)
          dev.off()
          
          outputs <- c(outputs, png_path)
        }
        
        zip::zipr(zipfile = file, files = outputs, recurse = FALSE)
      }
      
      # ── Case: PDF (.pdf) ──────────────────────────────────────────────────
      else if (input$export_table_format == "PDF (.pdf)") {
        prods      <- input$export_selected_tables
        disp_names <- make_display_names(prods)
        
        col_labels <- c(
          attribute            = "Original Descriptor",
          new_name_attribute   = "Descriptor",
          sum_coefficients     = "Score",
          count                = "Frequency",
          color                = "Color"
        )
        desired_order <- unname(col_labels)
        
        # If multiple products, generate separate PDF pages and zip them
        if (length(prods) > 1) {
          tmp  <- tempfile(); dir.create(tmp); outs <- character()
          for (i in seq_along(prods)) {
            df <- result() %>%
              filter(product == prods[i]) %>%
              add_missing_export_cols(input$export_table_columns) %>%
              select(all_of(input$export_table_columns)) %>%
              rename_with(~ col_labels[.x], all_of(input$export_table_columns)) %>%
              select(intersect(desired_order, names(.)))
            
            # Build fill matrix for table cell backgrounds
            n_row <- nrow(df); n_col <- ncol(df)
            fill_mat <- matrix("white", n_row, n_col)
            if ("Color" %in% names(df)) {
              col_idx <- which(names(df) == "Color")
              fill_mat[, col_idx] <- ifelse(is.na(df$Color) | df$Color == "", "white", df$Color)
            }
            tt  <- gridExtra::ttheme_minimal(
              core    = list(bg_params = list(fill = fill_mat, col = NA)),
              colhead = list(bg_params = list(fill = "grey90"), fg_params = list(fontface = "bold"))
            )
            tbl <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
            
            # Create PDF file for this product
            fname <- paste0(gsub("[^[:alnum:] ]", "_", disp_names[i]), ".pdf")
            Cairo::CairoPDF(
              file   = file.path(tmp, fname),
              width  = 1200 / 150,  # 8 inches at 150 dpi
              height = 600  / 150   # 4 inches at 150 dpi
            )
            grid::grid.newpage()
            grid::grid.draw(tbl)
            dev.off()
            outs <- c(outs, file.path(tmp, fname))
          }
          
          # Zip all individual PDFs
          zip::zipr(zipfile = file, files = outs, recurse = FALSE)
        } else {
          # Single-product PDF
          df <- result() %>%
            filter(product == prods) %>%
            add_missing_export_cols(input$export_table_columns) %>%
            select(all_of(input$export_table_columns)) %>%
            rename_with(~ col_labels[.x], all_of(input$export_table_columns)) %>%
            select(intersect(desired_order, names(.)))
          
          n_row <- nrow(df); n_col <- ncol(df)
          fill_mat <- matrix("white", n_row, n_col)
          if ("Color" %in% names(df)) {
            col_idx <- which(names(df) == "Color")
            fill_mat[, col_idx] <- ifelse(is.na(df$Color) | df$Color == "", "white", df$Color)
          }
          tt  <- gridExtra::ttheme_minimal(
            core    = list(bg_params = list(fill = fill_mat, col = NA)),
            colhead = list(bg_params = list(fill = "grey90"), fg_params = list(fontface = "bold"))
          )
          tbl <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
          
          Cairo::CairoPDF(
            file   = file,
            width  = 1200 / 150,
            height = 600  / 150
          )
          grid::grid.newpage()
          grid::grid.draw(tbl)
          dev.off()
        }
      }
    }
  )
  
  
  # ── 3) UI for selecting axes when exporting a circle plot ─────────────────
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
  
  
  # ── 4) Download handler for exporting circle plot ─────────────────────────
  output$download_plot <- downloadHandler(
    
    # Generate filename based on format and number of selected products
    filename = function() {
      prods <- input$plot_export_products
      if (is.null(prods) || length(prods) == 0) {
        prods <- unique(result()$product)
      }
      fmt <- input$plot_export_format
      ext <- switch(fmt,
                    "PNG (.png)" = if (length(prods) > 1) ".zip" else ".png",
                    "SVG (.svg)" = if (length(prods) > 1) ".zip" else ".svg",
                    "PDF (.pdf)" = if (length(prods) > 1) ".zip" else ".pdf")
      base <- if (length(prods) == 1) {
        make_display_names(prods) |> gsub("[^[:alnum:] ]", "_", .)
      } else {
        paste0("circle_plot_", Sys.Date())
      }
      paste0(base, ext)
    },
    
    # Content: generate and save plot(s) in chosen format
    content = function(file) {
      req(current_plot(), ui_w_px(), ui_h_px())
      w_px      <- ui_w_px() * 2   # scale up for high resolution
      h_px      <- ui_h_px() * 2
      dpi       <- 72
      scale_fac <- 2
      
      # Determine which axes to display based on user input
      axes_sel <- if (identical(input$plot_show_axes, "yes")) {
        input$plot_axes_selected %||% character(0)
      } else {
        character(0)
      }
      
      # Determine which products to export
      prods <- input$plot_export_products
      if (is.null(prods) || length(prods) == 0) {
        prods <- unique(result()$product)
      }
      file_bases <- make_display_names(prods)
      
      # Helper: regenerate a circle plot for a specific product with current settings
      make_plot_for <- function(prod) {
        make_circle_plot(
          product               = prod,
          result_df             = result(),
          descriptors_df        = descriptors_df_data(),
          puissance_df          = puissance_table(),
          num_descriptors       = input$num_descriptors,
          equalDescriptors      = input$equalDescriptors,
          firstDoubleSecond     = input$firstDoubleSecond,
          font_descriptor       = input$font_descriptor,
          color_descriptor      = input$color_descriptor,
          font_size_descriptor  = input$font_size_descriptor * scale_fac,
          plot_horizontal_shift = input$plot_horizontal_shift,
          label_horizontal_shift= input$label_horizontal_shift,
          show_labels           = input$show_descriptor_labels,
          selected_axes         = axes_sel,
          axis_params_list      = {
            lapply(axes_sel, function(ax) {
              list(
                title      = input[[paste0("axis_title_", ax)]]   %||% ax,
                color      = input[[paste0("axis_color_", ax)]]   %||% "#000000",
                offset     = input[[paste0("axis_offset_", ax)]]  %||% 0,
                thickness  = (input[[paste0("axis_thickness_", ax)]] %||% 1) * scale_fac,
                title_size = 4 * scale_fac
              )
            }) |> setNames(axes_sel)
          },
          axis_margin_data      = 10,
          spacing_scale         = 0.5,
          axis_spacing_add      = 30,
          value_text_size       = 6
        )
      }
      
      fmt <- input$plot_export_format
      
      # ── SVG (.svg) ─────────────────────────────────────────────────────────
      if (fmt == "SVG (.svg)") {
        if (length(prods) > 1) {
          tmp  <- tempfile(); dir.create(tmp); outs <- character()
          for (i in seq_along(prods)) {
            p    <- prods[i]
            plt  <- make_plot_for(p)
            fname <- paste0(gsub("[^[:alnum:] ]", "_", file_bases[i]), ".svg")
            svglite::svglite(file.path(tmp, fname),
                             width = w_px / dpi, height = h_px / dpi)
            print(plt); dev.off()
            outs <- c(outs, file.path(tmp, fname))
          }
          zip::zipr(zipfile = file, files = outs, recurse = FALSE)
        } else {
          svglite::svglite(file, width = w_px / dpi, height = h_px / dpi)
          print(current_plot()); dev.off()
        }
      }
      
      # ── PDF (.pdf) ─────────────────────────────────────────────────────────
      else if (fmt == "PDF (.pdf)") {
        if (length(prods) > 1) {
          tmp  <- tempfile(); dir.create(tmp); outs <- character()
          for (i in seq_along(prods)) {
            p    <- prods[i]
            plt  <- make_plot_for(p)
            fname <- paste0(gsub("[^[:alnum:] ]", "_", file_bases[i]), ".pdf")
            Cairo::CairoPDF(
              file   = file.path(tmp, fname),
              width  = w_px / dpi,
              height = h_px / dpi
            )
            print(plt); dev.off()
            outs <- c(outs, file.path(tmp, fname))
          }
          zip::zipr(zipfile = file, files = outs, recurse = FALSE)
        } else {
          Cairo::CairoPDF(
            file   = file,
            width  = w_px / dpi,
            height = h_px / dpi
          )
          print(current_plot()); dev.off()
        }
      }
      
      # ── PNG (.png) ─────────────────────────────────────────────────────────
      else if (fmt == "PNG (.png)") {
        if (length(prods) > 1) {
          tmp_dir <- tempfile(); dir.create(tmp_dir); outputs <- character()
          for (i in seq_along(prods)) {
            p    <- prods[i]
            plt  <- make_plot_for(p)
            fname <- paste0(gsub("[^[:alnum:] ]", "_", file_bases[i]), ".png")
            path <- file.path(tmp_dir, fname)
            ragg::agg_png(
              filename = path,
              width    = w_px,
              height   = h_px,
              units    = "px",
              res      = dpi
            )
            print(plt); dev.off()
            outputs <- c(outputs, path)
          }
          zip::zipr(zipfile = file, files = outputs, recurse = FALSE)
        } else {
          ragg::agg_png(
            filename = file,
            width    = w_px,
            height   = h_px,
            units    = "px",
            res      = dpi
          )
          print(current_plot()); dev.off()
        }
      }
    }
  )
  
  
  # ── 5) Update list of products for circle plot export when result() changes ──
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
  
  
  # ── 6) Download handler for a comprehensive PDF report ──────────────────────
  output$download_full_report <- downloadHandler(
    filename = function() {
      paste0("QFD_Comprehensive_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # 1) Download and read the company logo for the cover page
      tmp_logo <- tempfile(fileext = ".png")
      download.file(
        url      = "https://www.mane.com/theme/images/logo.png",
        destfile = tmp_logo,
        mode     = "wb"
      )
      logo_img <- png::readPNG(tmp_logo)
      
      # 2) Open a new PDF (US Letter portrait: 8.5 x 11 inches)
      Cairo::CairoPDF(
        file   = file,
        width  = 8.5,
        height = 11
      )
      
      # ── Cover page ─────────────────────────────────────────────────────────
      grid::grid.newpage()
      grid::grid.raster(
        logo_img,
        x      = unit(0.5,  "in"),
        y      = unit(10.75,"in"),
        width  = unit(1,    "in"),
        height = unit(1,    "in"),
        just   = c("left", "top")
      )
      grid::grid.text(
        "QFD Comprehensive Report",
        x      = 0.5, y = 0.65,
        gp     = grid::gpar(
          fontsize   = 34,
          fontface   = "bold",
          col        = "#007436",
          fontfamily = "Arial"
        )
      )
      grid::grid.text(
        paste0("Generated on ", Sys.Date()),
        x      = 0.5, y = 0.58,
        gp     = grid::gpar(
          fontsize   = 14,
          fontface   = "italic",
          col        = "#005F30",
          fontfamily = "Arial"
        )
      )
      
      # ── Pages per product ─────────────────────────────────────────────────
      prods      <- input$export_selected_tables %||% sort(unique(result()$product))
      disp_names <- make_display_names(prods)
      col_labels <- c(
        attribute          = "Original Descriptor",
        new_name_attribute = "Descriptor",
        sum_coefficients   = "Score",
        count              = "Frequency",
        color              = "Color"
      )
      desired_order <- c("Original Descriptor", "Descriptor", "Score", "Frequency", "Color")
      
      for (i in seq_along(prods)) {
        prod      <- prods[i]
        disp_name <- disp_names[i]
        
        df <- result() %>%
          filter(product == prod) %>%
          add_missing_export_cols(input$export_table_columns) %>%
          select(all_of(input$export_table_columns)) %>%
          rename_with(~ col_labels[.x], all_of(input$export_table_columns)) %>%
          select(intersect(desired_order, names(.)))
        
        # Build fill matrix for table cell backgrounds
        n_row <- nrow(df); n_col <- ncol(df)
        fill_mat <- matrix("white", n_row, n_col)
        if ("Color" %in% names(df)) {
          j <- which(names(df) == "Color")
          fill_mat[, j] <- ifelse(is.na(df$Color) | df$Color == "", "white", df$Color)
        }
        
        tt <- gridExtra::ttheme_minimal(
          core    = list(bg_params = list(fill = fill_mat, col = NA)),
          colhead = list(bg_params = list(fill = "grey90"), fg_params = list(fontface = "bold"))
        )
        tbl_grob <- gridExtra::tableGrob(df, rows = NULL, theme = tt)
        
        # New page for this product
        grid::grid.newpage()
        # Title at top of page
        grid::grid.text(
          disp_name,
          x    = 0.5, y = 0.95,
          gp   = grid::gpar(
            fontsize   = 20,
            fontface   = "bold",
            col        = "#007436",
            fontfamily = "Arial"
          )
        )
        
        # Position table slightly below title with margin
        titre_y <- 0.95; titre_h <- 0.05; marge <- 0.05
        bas_titre <- titre_y - titre_h
        tableau_y <- bas_titre - marge
        
        grid::pushViewport(grid::viewport(
          x      = 0.5,
          y      = tableau_y,
          width  = 0.9,
          height = 0.20,
          just   = c("center", "top")
        ))
        grid::grid.draw(tbl_grob)
        grid::popViewport()
        
        # Prepare axis parameters for circle plot
        axes_sel <- input$plot_axes_selected %||% character(0)
        axis_params_list <- lapply(axes_sel, function(ax) {
          list(
            title      = input[[paste0("axis_title_", ax)]]    %||% ax,
            color      = input[[paste0("axis_color_", ax)]]    %||% "#000000",
            offset     = input[[paste0("axis_offset_", ax)]]   %||% 0,
            thickness  = input[[paste0("axis_thickness_", ax)]] %||% 1,
            title_size = 4
          )
        }) |> setNames(axes_sel)
        
        # Generate circle plot for current product
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
          font_size_descriptor  = input$font_size_descriptor,
          plot_horizontal_shift = input$plot_horizontal_shift,
          label_horizontal_shift= input$label_horizontal_shift,
          show_labels           = input$show_descriptor_labels,
          selected_axes         = axes_sel,
          axis_params_list      = axis_params_list,
          axis_margin_data      = 120,
          spacing_scale         = 0.9,
          axis_spacing_add      = 10,
          value_text_size       = 3
        )
        
        # Position plot under the table with margin
        tbl_y   <- 0.95; tbl_h <- 0.10; marge2 <- 0.03
        bas_tbl <- tbl_y - tbl_h
        plot_y  <- bas_tbl - marge2
        
        grid::pushViewport(grid::viewport(
          x      = 0.5,
          y      = plot_y,
          width  = 0.9,
          height = 0.75,
          just   = c("center", "top")
        ))
        print(plt, vp = grid::current.viewport())
        grid::popViewport()
      }
      
      # Close the PDF device
      dev.off()
    }
  )
}
