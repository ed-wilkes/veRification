#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
  #' @noRd
app_server <- function(input, output, session) {

  # General functions ----
  window_height <- reactive({ifelse(is.null(input$height), 0, as.numeric(input$height))})
  window_width <- reactive({ifelse(is.null(input$width), 0, as.numeric(input$width))})
  # markdown_file <- normalizePath("inst/app/www/ACB_veRification_html_output_v0.2.Rmd")

  # Instructions module ----
  information_tab <- mod_Information_server("information_tab")

  # Imprecision module ----
  imprecision_tab <- mod_Imprecision_server("imprecision_tab")

  # Trueness EQA module ----
  trueness_tab <- mod_Trueness_server("trueness_tab", window_height)

  # Trueness (reference) module ----
  reference_tab <- mod_Reference_server("reference_tab", window_height)

  # Method comparison module ----
  comparison_tab <- mod_Comparison_server("comparison_tab", window_height)

  # Diagnostic performance module ----
  diagnostic_tab <- mod_Diagnostic_server("diagnostic_tab", window_height)

  ## "Export report" tab ----
  ## Code to be implemented in the future
  # export_tab <- mod_Export_server("export_tab")

}
