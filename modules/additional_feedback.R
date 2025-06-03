# modules/additional_feedback.R

# ── additional_feedback Module ─────────────────────────────────────────────
#
# This module generates a UI element displaying additional feedback for a
# selected product. It shows two sections side by side:
#  1) “Comments” – aggregated and counted from user comments
#  2) “Additional Descriptors” – parsed from Qualtrics responses (if applicable)
#
# Each list is rendered dynamically based on reactive inputs:
#   - selected_product(): currently chosen product code
#   - additional_comments(): raw comments data
#   - additional_descriptors(): raw descriptor strings (Qualtrics only)
#   - descriptors_df_data(): mapping of original descriptor names to new names
#
# Note: Values are divided by 3 (per business rule) before displaying counts.

additional_feedback <- function(input, output, session) {
  
  # Renders a flexbox container with two columns: Comments and Additional Descriptors
  output$feedback_list <- renderUI({
    # ── 1) Establish reactive dependencies to force UI refresh when any data changes ──
    req(
      selected_product(),       # currently selected product code
      additional_comments(),    # raw comments for all products
      additional_descriptors(), # raw descriptor strings for all products
      descriptors_df_data()     # mapping data frame: original_attribute → attribute
    )
    
    # ── 2) Extract and aggregate comments for the selected product ─────────────────
    #    Filter `additional_comments()` to rows matching the selected product, then
    #    extract the 'comments' column as a list of strings.
    cmts <- additional_comments() %>%
      filter(product_code == selected_product()) %>%
      pull(comments) %>%
      unlist()
    
    # If any comments exist, count occurrences (divide by 3 per business rule),
    # sort in descending order, and build a label that includes the count if > 1.
    if (length(cmts) > 0) {
      comment_counts <- tibble(comment = cmts) %>%
        count(comment, name = "n") %>%
        mutate(n = n / 3) %>%      # Business rule: divide raw count by 3
        arrange(desc(n)) %>%
        mutate(
          label = ifelse(
            n > 1,
            paste0(comment, " (", n, ")"),
            comment
          )
        )
    } else {
      # Create an empty tibble with the same columns, if no comments
      comment_counts <- tibble(
        comment = character(),
        n       = integer(),
        label   = character()
      )
    }
    
    # ── 3) Extract and aggregate additional descriptors (Qualtrics only) ───────────
    desc_counts <- NULL
    if (file_type() == "qualtrics") {
      # Retrieve raw descriptor strings for the selected product
      raw_desc <- additional_descriptors() %>%
        filter(product_code == selected_product()) %>%
        pull(descriptors) %>%
        unlist()
      
      if (length(raw_desc) > 0) {
        # 3.1) Split each comma-separated string into individual tokens
        desc_tokens <- tibble(text = raw_desc) %>%
          tidyr::separate_rows(text, sep = "\\s*,\\s*") %>%
          dplyr::mutate(descriptor = stringr::str_trim(text)) %>%
          dplyr::filter(nchar(descriptor) > 0)
        
        # 3.2) Join tokens to the mapping data frame to replace original names
        mapping <- descriptors_df_data()  # columns: original_attribute, attribute
        
        desc_counts <- desc_tokens %>%
          dplyr::count(descriptor, name = "n") %>%
          dplyr::mutate(n = n / 3) %>%  # Business rule: divide raw count by 3
          dplyr::left_join(mapping,
                           by = c("descriptor" = "original_attribute")
          ) %>%
          dplyr::mutate(
            # If a mapping exists, use the new `attribute` name; otherwise, keep original
            display = ifelse(!is.na(attribute), attribute, descriptor),
            label   = ifelse(
              n > 1,
              paste0(display, " (", n, ")"),
              display
            )
          ) %>%
          dplyr::arrange(desc(n))    # Sort by highest count
      } else {
        # Create an empty tibble if no descriptors exist
        desc_counts <- tibble(
          descriptor = character(),
          n          = integer(),
          label      = character()
        )
      }
    }
    
    # ── 4) Render the UI: two side-by-side sections ──────────────────────────────
    div(
      style = "display:flex; gap:30px;",
      
      # — Column A: Comments ─────────────────────────────────────────────────────
      div(
        style = "flex:1;",
        tags$h5("Comments", style = "margin-bottom:6px; color:#007436;"),
        
        if (nrow(comment_counts) > 0) {
          # If there are comments, list each with a comment icon
          tags$ul(
            style = "list-style:none; padding:0;",
            lapply(comment_counts$label, function(txt) {
              tags$li(
                style = "margin-bottom:4px;",
                tags$i(
                  class = "fas fa-comment-alt",
                  style = "color:#007436; margin-right:6px;"
                ),
                txt
              )
            })
          )
        } else {
          # If no comments, show a placeholder message
          tags$p(
            "No comments for this product.",
            style = "font-style:italic; color:#666;"
          )
        }
      ),
      
      # — Column B: Additional Descriptors (Qualtrics only) ─────────────────────
      if (!is.null(desc_counts)) {
        div(
          style = "flex:1;",
          tags$h5(
            "Additional Descriptors",
            style = "margin-bottom:6px; color:#e08b00;"
          ),
          
          if (nrow(desc_counts) > 0) {
            # If there are descriptors, list each with a tag icon
            tags$ul(
              style = "list-style:none; padding:0;",
              lapply(desc_counts$label, function(txt) {
                tags$li(
                  style = "margin-bottom:4px;",
                  tags$i(
                    class = "fas fa-tag",
                    style = "color:#e08b00; margin-right:6px;"
                  ),
                  txt
                )
              })
            )
          } else {
            # If no descriptors, show a placeholder message
            tags$p(
              "No descriptors for this product.",
              style = "font-style:italic; color:#666;"
            )
          }
        )
      }
    )
  })
}
