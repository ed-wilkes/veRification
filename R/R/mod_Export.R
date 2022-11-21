#' Export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Export_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    fluidRow(
      box(
        width = 4
        ,title = "Export .html report"
        ,status = "primary"
        ,solidHeader = TRUE
        ,strong("Choose the analyses to export:")
        ,checkboxInput(
          inputId = "export_imprecision"
          ,label = "Imprecision"
          ,value = FALSE
        )
        ,checkboxInput(
          inputId = "export_trueness"
          ,label = "Trueness"
          ,value = FALSE
        )
        ,checkboxInput(
          inputId = "export_comparison"
          ,label = "Method comparison"
          ,value = FALSE
        )
        ,checkboxInput(
          inputId = "export_diagnostic"
          ,label = "Diagnostic performance"
          ,value = FALSE
        )
        ,p()
        ,strong("Export your chosen analyses to a .html file:")
        ,p()
        ,downloadButton(outputId = "report", label = "Export report")
      )
    )

  ) # closes tagList

}

#' Export Server Functions
#'
#' @noRd
mod_Export_server <- function(id) {

  moduleServer( id, function(input, output, session) {

    ns <- session$ns

    observe({

      # Check if input files have been uploaded
      imprecision_checkbox <- !is.null(input$input_file_precision)
      trueness_checkbox <- !is.null(input$input_file_trueness)
      comparison_checkbox <- !is.null(input$input_file_comparison)
      diagnostic_checkbox <- !is.null(input$input_file_diagnostic)

      updateCheckboxInput(
        session
        ,"export_imprecision"
        ,value = imprecision_checkbox
      )
      updateCheckboxInput(
        session
        ,"export_trueness"
        ,value = trueness_checkbox
      )
      updateCheckboxInput(
        session
        ,"export_comparison"
        ,value = comparison_checkbox
      )
      updateCheckboxInput(
        session
        ,"export_diagnostic"
        ,value = diagnostic_checkbox
      )

    })

    output$report <- downloadHandler(

      filename = paste0("Assay_veRification_report_", Sys.Date(), ".html")
      #filename = "report.html"
      ,content = function(file) {
        withProgress(message = "Rendering report, please wait...", {

          # Copy report file to temporary directory
          temp_rmd_file <- file.path(tempdir(), markdown_file)
          file.copy(markdown_file, temp_rmd_file, overwrite = TRUE)

          # Define options
          options_and_inputs <- list(

            # Report to process
            precision_report = input$export_imprecision
            ,trueness_report = input$export_trueness
            ,comparison_report = input$export_comparison
            ,diagnostic_report = input$export_diagnostic

            # Precision options
            ,header_precision = input$header_precision
            ,analyte_precision = input$analyte_precision
            ,rows_precision = input$data_imprecision_rows_selected
            ,col_day = input$col_day
            ,col_value = input$col_value
            ,col_level = input$col_level
            ,cv_claims_test = input$cv_claims_test
            ,qc_levels = unique(df_precision()[[input$col_level]])
            ,claims = claims_vector()

            # Trueness options
            ,header_trueness = input$header_trueness
            ,duplicate_trueness = input$duplicate_trueness
            ,rows_trueness = input$data_trueness_rows_selected
            ,var_option = input$var_option
            ,col_mean_trueness = input$col_mean_trueness
            ,col_var_trueness = input$col_var_trueness
            ,col_n_trueness = input$col_n_trueness
            ,col_rep_1_trueness = input$col_rep_1_trueness
            ,col_rep_2_trueness = input$col_rep_2_trueness

            # Comparison options
            ,header_comparison = input$header_comparison
            ,analyte_comparison = input$analyte_comparison
            ,rows_comparison = input$data_comparison_rows_selected
            ,duplicate_comparison = input$duplicate_comparison
            ,method_comparison_1 = input$method_comparison_1
            ,method_comparison_2 = input$method_comparison_2
            ,reg_method_comp = input$reg_method_comp
            ,cor_method_comp = input$cor_method_comp
            ,stat_method_comp = input$stat_method_comp
            ,altman_method_comp = input$altman_method_comp
            ,var_x_comp = input$var_x_comp
            ,var_y_comp = input$var_y_comp
            ,col_ref_1 = input$col_ref_1
            ,col_ref_2 = input$col_ref_2
            ,col_new_1 = input$col_new_1
            ,col_new_2 = input$col_new_2

            # Diagnostic options
            ,header_diagnostic = input$header_diagnostics
            ,rows_diagnostic = input$data_diagnostic_rows_selected
            ,col_diag_value = input$col_diag_value
            ,col_diag_label = input$col_diag_label
            ,curve_type = input$curve_type
            ,positive_label = input$positive_label
            ,slider_threshold = input$slider_threshold

          )

          # Set up parameters
          params <- list(
            data_precision = df_precision()
            ,data_trueness = df_trueness()
            ,data_comparison = df_comparison()
            ,data_diagnostic = df_diagnostic()
            ,options = options_and_inputs
            ,colours = colours
            ,render_by_shiny = TRUE
          )

          # Render report
          rmarkdown::render(
            input = temp_rmd_file
            ,output_file = file
            ,params = params
            ,envir = new.env(parent = globalenv())
          )

        })
      }
    ) # closes downloadHandler()

  })

}

