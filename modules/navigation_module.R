# ── modules/navigation_module.R ─────────────────────────────────────────────
# This module manages the top navigation bar. Clicking on each section:
# - Highlights the active section by toggling the "active" class
# - Switches the hidden tabsetPanel to the corresponding tab

navigation_module <- function(input, output, session) {
  
  # ── Click handler for "Data Importation" nav section ──────────────────────
  onclick("nav_data_importation", {
    # Remove "active" from all nav sections, then add to this one
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_data_importation').addClass('active');
    ")
    # Switch to the "data_importation" tab
    updateTabsetPanel(session, "main_tabs", selected = "data_importation")
  })
  
  # ── Click handler for "Data Visualization" nav section ────────────────────
  onclick("nav_data_visualization", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_data_visualization').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "data_visualization")
  })
  
  # ── Click handler for "Statistical Analysis" nav section ─────────────────
  onclick("nav_statistical_analysis", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_statistical_analysis').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "statistical_analysis")
  })
  
  # ── Click handler for "Automatic Report" nav section ──────────────────────
  onclick("nav_automatic_report", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_automatic_report').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "automatic_report")
  })
  
  # ── Initialize: set "Data Importation" as active on app startup ───────────
  observe({
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_data_importation').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "data_importation")
  })
}
