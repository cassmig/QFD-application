# 📌 utils_circle_plot.R - Generates circular descriptor plots
# This function builds a circular visualization for a given product based on descriptor scores,
# styling options, and optional intensity axes.

make_circle_plot <- function(
    product_val,
    result_df,
    descriptors_df,
    puissance_df,
    num_descriptors,
    equalDescriptors,
    firstDoubleSecond,
    font_descriptor,
    color_descriptor,
    font_size_descriptor,
    plot_horizontal_shift = 100,
    label_horizontal_shift = 100,
    show_labels = "show",
    selected_axes,
    axis_params_list,
    axis_margin_data = 20,
    spacing_scale = 1,
    axis_spacing_add = 0,
    value_text_size = 4
) {
  # 1) Filter the result table for the chosen product and sort descending by score and count
  df <- result_df %>%
    filter(product == product_val) %>%
    arrange(desc(sum_coefficients), desc(count))
  
  # Utility: wrap long labels onto multiple lines
  wrap_text <- function(text, width = 15) {
    sapply(strwrap(text, width = width, simplify = FALSE), paste, collapse = "\n")
  }
  
  # 2) Ensure 'color' and 'new_name_attribute' columns exist,
  #    joining them from descriptors_df if missing
  if (!"color" %in% colnames(df)) {
    df <- left_join(
      df,
      descriptors_df %>% select(original_attribute, color),
      by = c("attribute" = "original_attribute")
    )
  }
  if (!"new_name_attribute" %in% colnames(df)) {
    df <- left_join(
      df,
      descriptors_df %>%
        select(original_attribute, attribute) %>%
        rename(new_name_attribute = attribute),
      by = c("attribute" = "original_attribute")
    )
  }
  
  # 3) Select the top descriptors according to num_descriptors,
  #    handling ties if equalDescriptors = "yes"
  n <- num_descriptors
  # Take up to n+1 rows to detect a tie
  top_descriptors <- df %>%
    slice_head(n = min(n + 1, 5))
  
  # Determine if we keep the (n+1)-th descriptor because of a tie
  has_tie <- equalDescriptors == "yes" &&
    nrow(top_descriptors) > n &&
    top_descriptors$sum_coefficients[n] == top_descriptors$sum_coefficients[n + 1]
  
  if (has_tie) {
    top_descriptors <- top_descriptors %>%
      slice_head(n = min(n + 1, 5))
  } else {
    top_descriptors <- top_descriptors %>%
      slice_head(n = min(n, 5))
  }
  n_final <- nrow(top_descriptors)
  
  # 3a) Compute base radii for each descriptor circle based on options
  #     The largest radius corresponds to the highest-scoring descriptor.
  if (num_descriptors == 2) {
    if (equalDescriptors == "no" && firstDoubleSecond == "no") {
      base_radii <- c(40, 80)
    } else if (equalDescriptors == "yes" && firstDoubleSecond == "no") {
      equality_applies <- (
        n_final >= 3 &&
          !is.na(top_descriptors$sum_coefficients[3]) &&
          top_descriptors$sum_coefficients[2] == top_descriptors$sum_coefficients[3]
      )
      base_radii <- if (equality_applies) c(30, 60, 90) else c(40, 80)
    } else if (equalDescriptors == "no" && firstDoubleSecond == "yes") {
      if (top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 80)
      } else {
        base_radii <- c(40, 80)
      }
    } else {
      if (n_final == 3 && top_descriptors$sum_coefficients[1] < 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(30, 60, 90)
      } else if (n_final == 2 && top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 80)
      } else if (n_final == 3 && top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 50, 95)
      } else {
        base_radii <- c(40, 80)
      }
    }
  } else if (num_descriptors == 3) {
    if (equalDescriptors == "no" && firstDoubleSecond == "no") {
      base_radii <- c(30, 60, 90)
    } else if (equalDescriptors == "yes" && firstDoubleSecond == "no") {
      base_radii <- if (n_final == 4) c(25, 50, 75, 100) else c(30, 60, 90)
    } else if (equalDescriptors == "no" && firstDoubleSecond == "yes") {
      base_radii <- if (top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        c(25, 50, 95)
      } else {
        c(30, 60, 90)
      }
    } else {
      if (n_final == 4 && top_descriptors$sum_coefficients[1] < 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 50, 75, 100)
      } else if (n_final == 3 && top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 50, 95)
      } else if (n_final == 4 && top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(20, 40, 60, 100)
      } else {
        base_radii <- c(30, 60, 90)
      }
    }
  } else if (num_descriptors == 4) {
    if (equalDescriptors == "no" && firstDoubleSecond == "no") {
      base_radii <- c(25, 50, 75, 100)
    } else if (equalDescriptors == "yes" && firstDoubleSecond == "no") {
      base_radii <- if (n_final == 5) c(25, 50, 75, 100, 125) else c(25, 50, 75, 100)
    } else if (equalDescriptors == "no" && firstDoubleSecond == "yes") {
      base_radii <- if (top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        c(20, 40, 60, 100)
      } else {
        c(25, 50, 75, 100)
      }
    } else {
      if (n_final == 5 && top_descriptors$sum_coefficients[1] < 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 50, 75, 100, 125)
      } else if (n_final == 4 && top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(20, 40, 60, 100)
      } else if (n_final == 5 && top_descriptors$sum_coefficients[1] >= 2 * top_descriptors$sum_coefficients[2]) {
        base_radii <- c(25, 50, 75, 100, 140)
      } else {
        base_radii <- c(25, 50, 75, 100)
      }
    }
  } else {
    # Default for 5 or more descriptors
    base_radii <- c(25, 50, 75, 100, 125)
  }
  
  # Assign radii (largest first) and attach to the top_descriptors data.frame
  radii_desc <- sort(base_radii[1:n_final], decreasing = TRUE)
  top_descriptors$r <- radii_desc
  
  # 4) Compute actual radii used and positions for labels
  radii_used   <- top_descriptors$r
  radii_sorted <- sort(radii_used)  # ascending order
  
  # Label y-position: halfway between each radius and the previous one (or 0 for the innermost)
  y_positions <- vapply(seq_along(radii_sorted), function(i) {
    inner <- if (i == 1) 0 else radii_sorted[i - 1]
    (inner + radii_sorted[i]) / 2
  }, numeric(1))
  
  # Compute horizontal offset for the circles; shift further if axes are present
  x_offset <- if (length(selected_axes) > 0) {
    -40 - length(selected_axes) * 10
  } else {
    20
  }
  x_offset <- x_offset + plot_horizontal_shift
  
  # 5) Prepare data for circles (draw largest to smallest)
  df_circles <- top_descriptors %>%
    transmute(
      x0 = x_offset,
      y0 = 0,
      r = r,
      fill_color = ifelse(is.na(color), "#FFFFFF", color)
    ) %>%
    arrange(desc(r)) %>%
    mutate(grp = factor(r, levels = r))
  
  # 6) Prepare data for labels (from center outward)
  labels_df <- top_descriptors %>%
    arrange(r) %>%
    mutate(
      x = x_offset + 12 + label_horizontal_shift,
      y = y_positions,
      label = new_name_attribute
    )
  
  # 7) Extract average intensity ("puissance") for each selected axis
  puissance_data <- puissance_df %>%
    rename(scale_type = type) %>%
    filter(product == product_val, scale_type %in% selected_axes)
  avg_puissance <- setNames(puissance_data$avg_puissance, puissance_data$scale_type)
  for (ax in selected_axes) {
    if (!ax %in% names(avg_puissance) || is.na(avg_puissance[ax])) {
      avg_puissance[ax] <- 0
    }
  }
  
  # 8) Build the ggplot object
  xlim_base <- c(-300, 500)
  ylim_base <- c(-200, 200)
  
  circle_plot <- ggplot() +
    geom_circle(
      data = df_circles,
      aes(x0 = x0, y0 = y0, r = r, fill = fill_color, group = grp),
      colour = "black", size = 0.8
    ) +
    scale_fill_identity() +
    coord_fixed(
      xlim = xlim_base + plot_horizontal_shift,
      ylim = ylim_base,
      expand = FALSE,
      clip = "off"
    ) +
    # White masks to cover unwanted parts
    annotate(
      "rect", xmin = -110 + x_offset, xmax = 150 + x_offset,
      ymin = -110, ymax = 0, fill = "white", color = "white"
    ) +
    annotate(
      "rect", xmin = 0 + x_offset, xmax = 150 + x_offset,
      ymin = -110, ymax = 110, fill = "white", color = "white"
    )
  
  # Add labels if requested
  if (show_labels == "show") {
    circle_plot <- circle_plot +
      geom_text(
        data = labels_df,
        aes(x = x, y = y, label = label),
        size = font_size_descriptor,
        color = color_descriptor,
        family = font_descriptor,
        hjust = 0,
        vjust = 0.5,
        lineheight = 1.2
      )
  }
  
  # 9) Draw intensity axes if selected
  axis_spacing <- (max(50, font_size_descriptor * 2) + 50) * spacing_scale + axis_spacing_add
  label_lengths <- vapply(
    labels_df$label,
    function(txt) max(nchar(strsplit(txt, "\n")[[1]])),
    integer(1)
  )
  max_chars <- max(label_lengths)
  char_width_data <- font_size_descriptor * 2.5
  base_x_arrow <- (max(labels_df$x) + max_chars * char_width_data + axis_margin_data) * spacing_scale
  
  if (length(selected_axes) > 0) {
    for (axis_id in selected_axes) {
      params <- axis_params_list[[axis_id]]
      
      # Draw the arrowed axis line
      circle_plot <- circle_plot +
        annotate(
          "segment",
          x = base_x_arrow + params$offset,
          xend = base_x_arrow + params$offset,
          y = 0,
          yend = 100,
          arrow = arrow(length = unit(0.25, "cm")),
          color = params$color,
          size = params$thickness
        ) +
        # Draw the axis title
        annotate(
          "text",
          x = base_x_arrow + params$offset,
          y = 110,
          label = wrap_text(params$title),
          color = params$color,
          size = params$title_size %||% 5,
          fontface = "bold",
          family = font_descriptor,
          hjust = 0.5,
          vjust = 0,
          lineheight = 1.1
        )
      
      # Draw the triangle marker and value for the intensity
      p_val <- avg_puissance[axis_id]
      if (!is.na(p_val) && p_val > 0) {
        tri_w <- 30   # horizontal distance for the triangle
        tri_h <- 16   # half-height of the triangle
        tri_y <- 100 * p_val / 10  # vertical position of the triangle
        
        coords <- data.frame(
          x = c(
            base_x_arrow + tri_w + params$offset,
            base_x_arrow + tri_w + params$offset,
            base_x_arrow + params$offset
          ),
          y = c(
            tri_y - tri_h,
            tri_y + tri_h,
            tri_y
          )
        )
        
        circle_plot <- circle_plot +
          annotate(
            "polygon",
            x = coords$x, y = coords$y,
            fill = params$color, color = NA
          ) +
          annotate(
            "text",
            x = base_x_arrow + tri_w - 2 + params$offset,
            y = tri_y,
            label = round(p_val, 1),
            color = "white",
            size = value_text_size,
            fontface = "bold",
            family = font_descriptor,
            hjust = 0.92,
            vjust = 0.5
          )
      }
      
      base_x_arrow <- base_x_arrow + axis_spacing
    }
  }
  
  # 10) Finalize theme: remove axes, background, and legend
  circle_plot <- circle_plot +
    theme_void() +
    theme(
      panel.background = element_rect(fill = NA, colour = NA),
      plot.background  = element_rect(fill = NA, colour = NA),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),
      axis.text        = element_blank(),
      legend.position  = "none"
    )
  
  return(circle_plot)
}
