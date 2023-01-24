#' Imprecision UI Function
#'
#' @description A shiny Module for the "Imprecision" tab
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Imprecision_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    tabsetPanel(
      id = ns("precision_tabs")

      # Data input
      ,tabPanel(
        title = "Data input"
        ,p()
        ,fluidRow(
          box(
            title = "Data input"
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,status = "primary"
            ,fileInput(
              inputId = ns("input_file")
              ,label = "Select your input file containing your data in long format:"
              ,accept = c(".csv", ".xls", ".xlsx")
              ,multiple = FALSE
            )
            ,checkboxInput(
              inputId = ns("header")
              ,label = "Are the column headers in row 1?"
              ,value = TRUE
            )
            ,numericInput(
              inputId = ns("ci_width")
              ,label = "Enter your preferred credible interval (%):"
              ,value = 89
            )
            ,textInput(
              inputId = ns("analyte_name")
              ,label = "Enter your analyte's name:"
              ,placeholder = "e.g., 'Free T4 (pmol/L)'"
            )
          )
        )
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true")
            ,box(
              title = "Your data"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,h6("NB: Click on rows you wish to exclude from the analysis!")
              ,DT::dataTableOutput(ns("data_table"))
              ,width = 6
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true")
            ,box(
              title = "Column selection"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,selectInput(
                inputId = ns("col_day")
                ,label = "Select the column that represents days:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_value")
                ,label = "Select the column that represents the measurements:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_level")
                ,label = "Select the column that represents QC level(s):"
                ,choices = ""
              )
              ,checkboxInput(
                inputId = ns("cv_claims_test")
                ,label = "Test against manufacturer's claims?"
              )
              ,actionButton(
                inputId = ns("run_model")
                ,label = "Fit model(s)"
                ,icon = icon("play")
                ,width = "100%"
              )
              ,width = 3
            )
          )
          ,fluidRow(conditionalPanel(
            condition = paste0("input[\'", ns("cv_claims_test"), "\'] == true")
            ,box(
              title = "Manufacturer's claims"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,uiOutput(ns("precision_claims"))
              ,width = 3
            )
          )
          )
        )
      )

      # Plots
      ,tabPanel(
        title = "Plots"
        ,value = "precision_tab_plots"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 12
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plots"))
                ,type = 6
              )
            )
          )
        )
      )

      # Summary statistics
      ,tabPanel(
        title = "Summary statistics"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 12
              ,shinycssloaders::withSpinner(
                uiOutput(ns("vca_results"))
                ,type = 6
              )
            )
          )
        )
      )

      # Bayesian model diagnostics
      ,tabPanel(
        title = "Bayesian model diagnostics"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 12
              ,shinycssloaders::withSpinner(
                uiOutput(ns("model_checks"))
                ,type = 6
              )
            )
          )
        )
      )

    ) # closes tabSetPanel
  ) # closes tagList

}

#' Imprecision Server Functions
#'
#' @noRd
mod_Imprecision_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cdata <- session$clientData

    output$file <- reactive({
      return(!is.null(input$input_file))
    })
    outputOptions(output, "file", suspendWhenHidden = FALSE)

    # Update selectInputs
    observe({
      updateSelectInput(
        session
        ,"col_day"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_value"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_level"
        ,choices = colnames(df())
        ,selected = ""
      )
    })

    # Data checks
    observeEvent(input$input_file, {
      checkInputFile(input, "input_file")
    })

    observeEvent(input$ci_width, {
      checkInputPositive(input, "ci_width")
    })

    # Data
    df <- reactive({
      readFile(input$input_file, headings = input$header)
    })

    # Column checks
    observeEvent(input$col_value,{
      checkInputNumeric(input, "col_value", df())
    })

    # Render data
    output$data_table <- DT::renderDataTable(
      {df()}
      ,extension = c("Buttons", "Scroller")
      ,options = list(scrollX = TRUE)
      ,rownames = FALSE
    )

    # Manufacturer's claims
    output$precision_claims <- renderUI({

      # Requirements
      req(input$cv_claims_test, input$col_level)

      qc_levels <- unique(df()[[input$col_level]])
      if (is.null(qc_levels)) {
        qc_levels <- 1
      }

      # Generate textInput boxes depending on number of levels
      lapply(seq_along(qc_levels), function(i) {
        numericInput(
          inputId = ns(paste0("claims_", i))
          ,label = paste0("Manufacturer's total CV, level ", i, " (%)")
          ,value = NULL
        )
      })

    })

    # Generate reactive vector of claim values
    claims_vector <- reactive({
      qc_levels <- unique(df()[[input$col_level]])
      if (is.null(qc_levels)) {
        qc_levels <- 1
      }
      claims <- paste0("claims_", qc_levels)
      claims_vector <- unlist(lapply(claims, function(i) {input[[i]]}))
      return(as.numeric(claims_vector))
    })

    # Check validity of claim values dynamically
    observeEvent(input$cv_claims_test, {

      req(input$col_level)
      qc_levels <- unique(df()[[input$col_level]])
      if (is.null(qc_levels)) {
        qc_levels <- 1
      }

      lapply(seq_along(qc_levels), function(i) {

        input_id <- paste0("claims_", i)
        observeEvent(input[[input_id]], {
          if (is.na(input[[input_id]])) {
            shinyFeedback::showFeedbackWarning(
              inputId = ns(input_id)
              ,text = "Value must be provided!"
            )
          } else {
            shinyFeedback::hideFeedback(ns(input_id))
          }
        })

      })

    })

    # Reactive values to store results
    plot_output <- reactiveValues(
      plots = NULL # list of fluidRow -> box -> renderPlotly -> ggplotly objects
      ,checks = NULL # list of fluidRow -> box -> renderPlot -> ggplot2 objects
    )
    fit <- reactiveValues(
      models = NULL # list of brms model objects
      ,boxes = NULL # list of fluidRow -> box
    )

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Run if "run_model" is pressed
    observeEvent(toListen(), {

      withProgress(message = "Fitting model(s), please wait...", {

        # Requirements before processing
        req(input$col_day, input$col_value, input$col_level, input$ci_width)

        # Get QC levels and claims
        colours <- c("teal", "aqua", "blue", "navy", "black")
        qc_levels <- unique(df()[[input$col_level]])
        if (is.null(qc_levels)) {
          qc_levels <- 1
        }
        colour_values <- colours[1:length(qc_levels)]
        names(colour_values) <- qc_levels

        if (input$cv_claims_test == TRUE) {
          claim_values = claims_vector()
          names(claim_values) <- qc_levels
          validate(
            need(
              expr = anyNA(claim_values) == FALSE
              ,message = "Please make sure you have entered manufacturer's claims
            for each QC level."
            )
          )
        } else {
          claim_values <- NULL
        }

        # Remove excluded rows
        if (!is.null(input$data_table_rows_selected)) {
          df_prec <- df()[-input$data_table_rows_selected,]
        } else {
          df_prec <- df()
        }

        # Checks
        validate(
          need(
            expr = is.numeric(df_prec[[input$col_value]]) == TRUE
            ,message = "The column in your data that represents your measurements does
        not contain numbers. This may be because the column contains text or missing
        values. Please check and reupload your data and/or reselect your columns."
          )
        )

        # Update focus to plots tab
        updateTabsetPanel(session, "precision_tabs", selected = "precision_tab_plots")

        # Loop to plot data
        plot_output$plots <- lapply(seq_along(qc_levels), function(i) {

          fluidRow(
            box(
              title = paste0("QC level ", i)
              ,collapsible = TRUE
              ,solidHeader = TRUE
              ,status = "primary"
              ,width = 9
              ,plotly::renderPlotly(
                plotPrecData(
                  data = df_prec
                  ,x_var = input$col_day
                  ,y_var = input$col_value
                  ,qc_level = i
                  ,col_level = input$col_level
                  ,analyte = input$analyte_name
                  ,plot_dim = cdata
                )
              )
            )
          )

        })

        # Loop to fit models
        fit$models <- lapply(seq_along(qc_levels), function(i) {

          fitModelPrec(
            data = df_prec
            ,x_var = input$col_day
            ,y_var = input$col_value
            ,qc_level = i
            ,col_level = input$col_level
          )

        })

        setProgress(0.5, "Extracting model information...")

        # Loop to extract model info into boxes
        fit$boxes <- lapply(seq_along(qc_levels), function(i) {

          fluidRow(
            box(
              title = paste0("QC level ", i)
              ,collapsible = TRUE
              ,solidHeader = TRUE
              ,status = "primary"
              ,width = 12
              ,boxesPrec(
                model = fit$models[[i]]
                ,x_var = input$col_day
                ,colours = colour_values
                ,qc_level = i
                ,test_claims = input$cv_claims_test
                ,claims_data = claim_values
                ,ci_interval = input$ci_width
              )
            )
          )

        })

        # Loop to extract model checks
        plot_output$checks <- lapply(seq_along(qc_levels), function(i) {

          check_plots <- modelChecks(
            model = fit$models[[i]]
            ,model_type = "imprecision"
            ,ci_interval = input$ci_width
          )

          fluidRow(
            box(
              title = paste0("QC level ", i)
              ,collapsible = TRUE
              ,solidHeader = TRUE
              ,status = "primary"
              ,width = 12
              ,column(
                width = 6
                ,renderPlot(check_plots[[1]])
              )
              ,column(
                width = 6
                ,renderPlot(check_plots[[2]])
              )

            )
          )

        })

      }) # ends withProgress

    })

    output$plots <- renderUI({
      plot_output$plots
    })

    output$vca_results <- renderUI({
      fit$boxes
    })

    output$model_checks <- renderUI({
      plot_output$checks
    })

  })
}
