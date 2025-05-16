# R/utils_circle_plot.R
  
  #' Génère un ggplot circulaire pour un produit donné et des paramètres Shiny
  #'
  #' @param product_val string, nom du produit à tracer
  #' @param result_df data.frame, table résultat (avec sum_coefficients, count, product, attribute, etc.)
  #' @param descriptors_df data.frame, table de mapping original_attribute → (color, new_name_attribute)
  #' @param puissance_df data.frame, table des moyennes de puissance (avec product, type, avg_puissance)
  #' @param num_descriptors integer
  #' @param equalDescriptors string "yes"/"no"
  #' @param firstDoubleSecond string "yes"/"no"
  #' @param font_descriptor string (nom de la police)
#' @param color_descriptor string (hex color)
#' @param selected_axes character vector, ex. c("A1","A3")
#' @param axis_params_list named list of lists, chaque element pour un axis_id contient title, color, offset, thickness
#' @param font_size_descriptor numeric Taille de la police des descripteurs
#' @param plot_horizontal_shift numeric Décalage horizontal du plot
#' @param label_horizontal_shift numeric Décalage horizontal des libellés
#' @return ggplot object
#' 
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
    bg_choice = "white" 
) {
  # 1) Filtre la table sur le produit
  df <- result_df %>%
    filter(product == product_val) %>%
    arrange(desc(sum_coefficients), desc(count))   # tri décroissant
  
  # Fonction utilitaire pour le texte en plusieurs lignes
  wrap_text <- function(text, width = 15) {
    sapply(strwrap(text, width = width, simplify = FALSE), paste, collapse = "\n")
  }
  
  # 2) Ajout des colonnes color & new_name_attribute si manquantes
  if (!"color" %in% colnames(df)) {
    df <- left_join(df,
                    descriptors_df %>% select(original_attribute, color),
                    by = c("attribute" = "original_attribute"))
  }
  if (!"new_name_attribute" %in% colnames(df)) {
    df <- left_join(df,
                    descriptors_df %>% select(original_attribute, attribute) %>% rename(new_name_attribute = attribute),
                    by = c("attribute" = "original_attribute"))
  }
  
  # 3) Sélection des top descriptors selon num_descriptors, equalDescriptors, firstDoubleSecond
  n <- num_descriptors
 
   # Garde n+1 lignes pour détecter un tie
  top_descriptors <- df %>%                                   # <- df n'était pas trié
    slice_head(n = min(n + 1, 5))
  
  
  # Détecte si on doit conserver le descriptor n+1
  has_tie <- equalDescriptors == "yes" &&
    nrow(top_descriptors) > n &&
    top_descriptors$sum_coefficients[n] == top_descriptors$sum_coefficients[n + 1]
  
  if (has_tie) {
    # Garde n+1 (max 5)
    top_descriptors <- top_descriptors %>% slice_head(n = min(n + 1, 5))
  } else {
    # Garde n (max 5)
    top_descriptors <- top_descriptors %>% slice_head(n = min(n, 5))
  }
  
  n_final <- nrow(top_descriptors)
  

  # 3a) Détermine base_radii & annotate_params selon tes options
  if (num_descriptors== 2) {
    
    # PAS D'OPTIONS:
    #--------------#
    if (equalDescriptors == "no" && firstDoubleSecond == "no"){
      base_radii <- c(40,80)
    }
    
    # OPTION D'EGALITE :
    #------------------#
    else if (equalDescriptors == "yes" && firstDoubleSecond == "no") {
      
      equality_applies <- (
        n_final >= 3 &&
          !is.na(top_descriptors$sum_coefficients[3]) &&
          top_descriptors$sum_coefficients[2] == top_descriptors$sum_coefficients[3]
      )
      
      
      if (equality_applies) {
        base_radii <- c(30,60,90)
      } else {
        base_radii <- c(40,80)
      }
    }      
    
    # OPTION DE DOMINANCE :
    #----------------------#
    else if (equalDescriptors == "no" && firstDoubleSecond == "yes") {
      
      ## Si il y a dominance : 
      if (top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2]){ 
        base_radii <- c(25,80)
      }
      
      ## Sinon : 
      else{
        base_radii <- c(40,80)
      }
    } 
    
    # OPTION D'EGALITE ET DE DOMINANCE : 
    #----------------------------------#
    else {
      
      ## Egalité 
      if (n_final ==3 && top_descriptors$sum_coefficients[1] < 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(30,60,90)
      }
      
      ## Dominance
      else if (n_final ==2 && top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(25,80)
      }
      
      ## Egalité et Dominance 
      else if (n_final ==3 && top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(25,50, 95)
      }
      
      ## Aucuns des deux : 
      else {
        base_radii <- c(40,80)
      }
    }
  }
  else if(num_descriptors== 3){
    
    # PAS D'OPTIONS:
    #--------------#
    if (equalDescriptors == "no" && firstDoubleSecond == "no"){
      base_radii <- c(30,60,90)
    }
    
    # OPTION D'EGALITE :
    #------------------#
    else if (equalDescriptors == "yes" && firstDoubleSecond == "no") {
      
      ## Si il y a une égalité : 
      if (n_final == 4){
        base_radii <- c(25,50,75,100)
      }
      
      ## Sinon:
      else {
        base_radii <- c(30,60,90)
      } 
    }
    
    
    # OPTION DE DOMINANCE :
    #----------------------#
    else if (equalDescriptors == "no" && firstDoubleSecond == "yes") {
      
      ## Si il y a dominance : 
      if (top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2]){ 
        base_radii <- c(25,50,95)
      }
      
      ## Sinon : 
      else{
        base_radii <- c(30,60,90)
      }
    } 
    
    # OPTION D'EGALITE ET DE DOMINANCE : 
    #----------------------------------#
    else {
      
      ## Egalité 
      if (n_final ==4 && top_descriptors$sum_coefficients[1] < 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(25,50,75, 100)
      }
      
      ## Dominance
      else if (n_final ==3 && top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(25,50, 95)
      }
      
      ## Egalité et Dominance 
      else if (n_final ==4 && top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(20, 40, 60, 100)
      }
      
      ## Aucuns des deux : 
      else {
        base_radii <- c(30,60, 90)
      }
    }
  }
  else if (num_descriptors== 4){
    
    # PAS D'OPTIONS:
    #--------------#
    if (equalDescriptors == "no" && firstDoubleSecond == "no"){
      base_radii <- c(25,50,75,100)
    }
    
    # OPTION D'EGALITE :
    #------------------#
    else if (equalDescriptors == "yes" && firstDoubleSecond == "no") {
      
      ## Si il y a une égalité : 
      if (n_final == 5){
        base_radii <- c(25,50,75,100,125)
      }
      
      ## Sinon:
      else {
        base_radii <- c(25,50,75,100)
      } 
    }
    
    
    # OPTION DE DOMINANCE :
    #----------------------#
    else if (equalDescriptors == "no" && firstDoubleSecond == "yes") {
      
      ## Si il y a dominance : 
      if (top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2]){ 
        base_radii <- c(20,40,60,100)
      }
      
      ## Sinon : 
      else{
        base_radii <- c(25,50,75,100)
      }
    } 
    
    # OPTION D'EGALITE ET DE DOMINANCE : 
    #----------------------------------#
    else {
      
      ## Egalité 
      if (n_final ==5 && top_descriptors$sum_coefficients[1] < 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(25,50,75,100,125)
      }
      
      ## Dominance
      else if (n_final ==4 && top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(20,40,60,100)
      }
      
      ## Egalité et Dominance 
      else if (n_final ==5 && top_descriptors$sum_coefficients[1] >= 2*top_descriptors$sum_coefficients[2] ) {
        base_radii <- c(25,50,75,100,140)
      }
      
      ## Aucuns des deux : 
      else {
        base_radii <- c(25,50,75,100)
      }
    }
    
  }
  else {
    base_radii <- c(25,50,75,100,125)
  }
  
  
  # Attribution explicite du plus GRAND rayon au meilleur descripteur ----------
  radii_desc <- sort(base_radii[1:n_final], decreasing = TRUE)
  top_descriptors$r <- radii_desc   # colonne "r" = rayon du cercle
  
  # ────────────────────────────────────────────────────────────────────────────
  # 4) Rayons réellement utilisés et positions des libellés --------------------
  radii_used   <- top_descriptors$r
  radii_sorted <- sort(radii_used)   # ordre croissant (centre → ext.)
  
  
  
  # pour chaque cercle on place le label à la moyenne entre son rayon et celui du cercle précédent (0 pour le centre)
  # calculer la position Y de chaque label :
  #   pour i = 1, moyenne entre 0 et premier rayon
  #   pour i > 1, moyenne entre rayon[i-1] et rayon[i]
  y_positions <- vapply(seq_along(radii_sorted), function(i) {
    inner <- if (i == 1) 0 else radii_sorted[i - 1]
    (inner + radii_sorted[i]) / 2
  }, numeric(1))
  
  # Calcul du décalage si des axes sont sélectionnés
  x_offset <- if (length(selected_axes) > 0) {
    -40 - length(selected_axes) * 10
  } else {
    20
  }
  
  x_offset <- x_offset + plot_horizontal_shift
  
  ## ─────────────────── 5) Données cercles & libellés ─────────────────────────
  # Cercles : du plus grand au plus petit (ordre de tracé)
  df_circles <- top_descriptors %>%
    transmute(
      x0         = x_offset,
      y0         = 0,
      r          = r,
      fill_color = ifelse(is.na(color), "#FFFFFF", color)
    ) %>%
    arrange(desc(r)) %>%
    mutate(grp = factor(r, levels = r))
  

  # Libellés : du centre vers l’extérieur
  labels_df <- top_descriptors %>%
    arrange(r) %>%
    mutate(
      x     = x_offset + 12 + label_horizontal_shift,
      y     = y_positions,
      label = wrap_text(new_name_attribute)
    )
  
  
  ## ─────────────────── 6) Données puissance pour axes sélectionnés ───────────
  puissance_data <- puissance_df %>%
    rename(scale_type = type) %>%
    filter(product == product_val,
           scale_type %in% selected_axes)
  
  avg_puissance <- setNames(puissance_data$avg_puissance,
                            puissance_data$scale_type)
  for (ax in selected_axes) {
    if (is.na(avg_puissance[ax]) || !(ax %in% names(avg_puissance))) {
      avg_puissance[ax] <- 0
    }
  }
  
  
  
  ## ─────────────────── 7) Construction du ggplot ─────────────────────────────
  
  xlim_base <- c(-300, 500)          # valeurs actuelles
  ylim_base <- c(-200, 200)
  
  circle_plot <- ggplot() +
    geom_circle(
      data   = df_circles,
      aes(x0 = x0, y0 = y0, r = r, fill = fill_color, group=grp),
      colour = "black", size = 0.8
    ) +
    scale_fill_identity() +
    coord_fixed(
      xlim   = xlim_base + plot_horizontal_shift,   # ← on translate la fenêtre !
      ylim   = ylim_base,
      expand = FALSE,
      clip   = "off"
    ) +
    # Masques blancs
    annotate("rect", xmin = -110 + x_offset, xmax = 150 + x_offset,
             ymin = -110, ymax = 0,   fill = "white", color = "white") +
    annotate("rect", xmin =   0 + x_offset, xmax = 150 + x_offset,
             ymin = -110, ymax = 110, fill = "white", color = "white") 
  
  
  if (show_labels == "show") {
    circle_plot <- circle_plot +
      geom_text(
        data       = labels_df,
        aes(x = x, y = y, label = label),
        size       = font_size_descriptor,
        color      = color_descriptor,
        family     = font_descriptor,
        hjust      = 0,
        vjust      = 0.5,
        lineheight = 1.2
      )
  }
 
  ## ─────────────────── 8) Axes fléchés (si demandés) ─────────────────────────
  
  
  # marge en unités « data » entre la fin du label et le début de l'axe  
  axis_margin <- 10  
  
  axis_spacing <- max(40, font_size_descriptor*2)
  
  # on calcule combien de caractères fait la plus longue ligne de chacun de nos labels  
  label_lengths <- vapply(
    labels_df$label,
    function(txt) max(nchar(strsplit(txt, "\n")[[1]])),
    integer(1)
  )
  max_chars <- max(label_lengths)
  
  # facteur de conversion approximatif (en data-units) par caractère  
  # ajustez ce « 0.5 » si besoin (plus grand = texte plus large)  
  char_width_data <- font_size_descriptor * 2.5
  
  # on se place juste à droite du label le plus long + margin  
  base_x_arrow <- max(labels_df$x) + max_chars * char_width_data + axis_margin
  
  
   if (length(selected_axes) > 0) {
    for (axis_id in selected_axes) {
      params <- axis_params_list[[axis_id]]
      
      circle_plot <- circle_plot +
        # segment
        annotate("segment",
                 x    = base_x_arrow + params$offset,
                 xend = base_x_arrow + params$offset,
                 y    = 0,
                 yend = 100,
                 arrow = arrow(length = unit(0.25, "cm")),
                 color = params$color,
                 size  = params$thickness) +
        # titre
        annotate("text",
                 x     = base_x_arrow + params$offset,
                 y     = 110,
                 label = wrap_text(params$title),
                 color = params$color,
                 size  = params$title_size %||% 5,
                 fontface = "bold",
                 family   = font_descriptor,
                 hjust    = 0.5,
                 vjust    = 0,
                 lineheight = 1.1)
      
      # triangle + valeur de puissance
      p_val <- avg_puissance[axis_id]
      if (!is.na(p_val) && p_val > 0) {
        tri_w <- 30   # distance horizontale du triangle
        tri_h <- 16   # demi‐hauteur du triangle
        # calcul de la position verticale du triangle
        tri_y <- 100 * p_val / 10
        coords <- data.frame(
          x = c(base_x_arrow + tri_w + params$offset,
                base_x_arrow + tri_w + params$offset,
                base_x_arrow + params$offset),
          y = c(tri_y - tri_h, tri_y + tri_h, tri_y)
        )
        circle_plot <- circle_plot +
          annotate("polygon",
                   x = coords$x, y = coords$y,
                   fill = params$color, color = NA) +
          annotate("text",
                   x = base_x_arrow + tri_w - 2 + params$offset,
                   y = tri_y,
                   label = round(p_val, 1),
                   color = "white",
                   size  = 4.0,
                   fontface = "bold",
                   family   = font_descriptor,
                   hjust    = 0.92,
                   vjust    = 0.5)
      }
      base_x_arrow <- base_x_arrow + axis_spacing
    }
  }
  
  ## ─────────────────── 9) Nettoyage thème et retour ──────────────────────────
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