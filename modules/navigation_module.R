navigation_module <- function(input, output, session) {
  
  # Detect clicks on navigation divs using shinyjs
  onclick("nav_data_importation", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_data_importation').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "data_importation")
  })
  
  onclick("nav_data_visualization", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_data_visualization').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "data_visualization")
  })
  
  onclick("nav_statistical_analysis", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_statistical_analysis').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "statistical_analysis")
  })
  
  onclick("nav_automatic_report", {
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_automatic_report').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "automatic_report")
  })
  
  # Set the first section as active when the app starts
  observe({
    runjs("
      $('.nav-section').removeClass('active');
      $('#nav_data_importation').addClass('active');
    ")
    updateTabsetPanel(session, "main_tabs", selected = "data_importation")
  })
}
