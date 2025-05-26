# modules/additional_feedback.R
# Module to display combined additional comments and descriptors for selected product

additional_feedback <- function(input, output, session) {
  observeEvent(selected_product(), {
    req(selected_product())
    req(additional_comments())
    
    # 1) Filter and retrieve comments vector
    cmts <- additional_comments() %>%
      dplyr::filter(product_code == selected_product()) %>%
      dplyr::pull(comments) %>%
      unlist()
    if (length(cmts) > 0) {
      comment_counts <- tibble::tibble(comment = cmts) %>%
        dplyr::count(comment, name = "n") %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::mutate(
          label = ifelse(n > 1,
                         paste0(comment, " (", n, "×)"),
                         comment)
        )
    } else {
      comment_counts <- tibble::tibble(
        comment = character(),
        n       = integer(),
        label   = character()
      )
    }
    
    # 2) Only for Qualtrics: filter and retrieve additional descriptors
    desc_counts <- NULL
    if (file_type() == "qualtrics") {
      req(additional_descriptors())
      desc <- additional_descriptors() %>%
        dplyr::filter(product_code == selected_product()) %>%
        dplyr::pull(descriptors) %>%
        unlist()
      if (length(desc) > 0) {
        desc_counts <- tibble::tibble(descriptor = desc) %>%
          dplyr::count(descriptor, name = "n") %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::mutate(
            label = ifelse(n > 1,
                           paste0(descriptor, " (", n, "×)"),
                           descriptor)
          )
      } else {
        desc_counts <- tibble::tibble(
          descriptor = character(),
          n          = integer(),
          label      = character()
        )
      }
    }
    
    # 3) Render combined UI into feedback_list with two columns
    output$feedback_list <- renderUI({
      # Container flex
      div(style = "display:flex; justify-content:space-between; gap: 30px;",
          # Comments column
          div(style = "flex:1;",
              tags$h5("Comments", style = "margin:0 0 6px; color:#007436;"),
              if (nrow(comment_counts) > 0) {
                tags$ul(
                  style = "list-style:none; padding:0; margin:0;",
                  lapply(comment_counts$label, function(txt) {
                    tags$li(
                      style = "display:flex; align-items:flex-start; margin-bottom:6px;",
                      tags$i(class = "fas fa-comment-alt",
                             style = "color:#007436; margin-right:6px;"),
                      span(txt, style = "font-size:13px; color:#333;")
                    )
                  })
                )
              } else {
                p("No comments for this product.",
                  style = "font-style:italic; color:#666;")
              }
          ),
          # Descriptors column (only for Qualtrics)
          if (file_type() == "qualtrics") {
            div(style = "flex:1;",
                tags$h5("Additional Descriptors", style = "margin:0 0 6px; color:#e08b00;"),
                if (nrow(desc_counts) > 0) {
                  tags$ul(
                    style = "list-style:none; padding:0; margin:0;",
                    lapply(desc_counts$label, function(txt) {
                      tags$li(
                        style = "display:flex; align-items:flex-start; margin-bottom:6px;",
                        tags$i(class = "fas fa-tag",
                               style = "color:#e08b00; margin-right:6px;"),
                        span(txt, style = "font-size:13px; color:#333;")
                      )
                    })
                  )
                } else {
                  p("No additional descriptors for this product.",
                    style = "font-style:italic; color:#666;")
                }
            )
          }
      )
    })
  })
}
