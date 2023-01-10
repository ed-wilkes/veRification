#' Reference UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Reference_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(
    tabsetPanel(
      id = ns("ref_tabs")

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
              ,label = "Select your input file containing your data:"
              ,accept = c(".csv", ".xls", ".xlsx")
              ,multiple = FALSE
            )
            ,checkboxInput(
              inputId = ns("header")
              ,label = "Are the column headers in row 1?"
              ,value = TRUE
            )
            ,checkboxInput(
              inputId = ns("duplicate_option")
              ,label = "Has the measurand been assayed in duplicate?"
              ,value = FALSE
            )
            ,numericInput(
              inputId = ns("prior_location")
              ,label = "Enter the reference target value:"
              ,value = NULL
            )
            ,numericInput(
              inputId = ns("prior_scale")
              ,label = "Enter the reference measure of uncertainty:"
              ,value = NULL
            )
            ,numericInput(
              inputId = ns("ci_width")
              ,label = "Enter your preferred credible interval (%):"
              ,value = 89
            )
            ,selectInput(
              inputId = ns("var_option")
              ,label = "What measure of uncertainty is quoted for the reference material?"
              ,choices = c(
                "Select a measure of variance" = ""
                ,"SD"
                ,"CV (%)"
                ,"SEM"
              )
            )
            ,conditionalPanel(
              condition = paste0("input[\'", ns("var_option"), "\'] == 'SEM'")
              ,numericInput(
                inputId = ns("prior_n")
                ,label = "Enter the number of measurements for the reference target:"
                ,value = NULL
              )
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
              ,width = 3
              ,selectInput(
                inputId = ns("col_sample")
                ,label = "Select the column that represents the samples measured:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_rep_1")
                ,label = "Select the column that represents the results from your method:"
                ,choices = ""
              )
              ,actionButton(
                inputId = ns("run_model")
                ,label = "Fit model"
                ,icon = icon("play")
                ,width = "100%"
              )
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("duplicate_option"), "\'] == true")
            ,box(
              title = "Duplicate selection"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,width = 3
              ,selectInput(
                inputId = ns("col_rep_2")
                ,label = "Select the column that represents the duplicate result from your method:"
                ,choices = ""
              )
            )
          )
        )
      )

      # Plots
      ,tabPanel(
        title = "Plots"
        ,value = "plots_ref"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 8
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot"))
                ,type = 6
              )
            )
            ,box(
              title = "Statistical inferences"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,width = 4
              ,shinycssloaders::withSpinner(
                uiOutput(ns("inferences"))
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
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 6
              ,shinycssloaders::withSpinner(
                uiOutput(ns("mcmc_plot"))
                ,type = 6
              )
            )
            ,column(
              width = 6
              ,shinycssloaders::withSpinner(
                uiOutput(ns("posteriors_plot"))
                ,type = 6
              )
            )
          )
        )
      )

    ) # closes tabSetPanel
  ) # closes tagList

}

#' Reference Server Functions
#'
#' @noRd
mod_Reference_server <- function(id, window_height) {

  moduleServer( id, function(input, output, session) {

    ns <- session$ns

    output$file <- reactive({
      return(!is.null(input$input_file))
    })
    outputOptions(output, "file", suspendWhenHidden = FALSE)

    # Update selectInputs
    observe({
      updateSelectInput(
        session
        ,"col_sample"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_rep_1"
        ,choices = colnames(df())
        ,selected = ""
      )
    })

    observe({
      if (input$duplicate_option == FALSE) {
        updateSelectInput(
          session
          ,"col_rep_2"
          ,choices = colnames(df())
          ,selected = ""
        )
      } else {
        updateSelectInput(
          session
          ,"col_rep_2"
          ,choices = colnames(df())
          ,selected = ""
        )
      }
    })

    # Data checks
    observeEvent(input$input_file, {
      checkInputFile(input, "input_file")
    })

    observeEvent(input$ci_width, {
      checkInputPositive(input, "ci_width")
    })

    # Get data
    df <- reactive({
      readFile(input$input_file, headings = input$header)
    })

    # Check prior MU
    observeEvent(input$prior_scale, {
      checkInputPositive(input, "prior_scale")
    })

    # Column checks
    observeEvent(input$col_rep_1, {
      checkInputNumeric(input, "col_rep_1", df())
    })

    observeEvent(input$col_rep_2, {
      checkInputNumeric(input, "col_rep_2", df())
    })

    # Render data
    output$data_table <- DT::renderDataTable(
      {df()}
      ,extension = c("Buttons", "Scroller")
      ,options = list(scrollX = TRUE)
      ,rownames = FALSE
    )

    # Reactive values definition
    plot_output <- reactiveValues(plot = NULL)
    fit <- reactiveValues(model = NULL, boxes = NULL, checks = NULL, bf = NULL)

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Model fitting
    observeEvent(toListen(), {

      withProgress(message = "Fitting model, please wait...", {

        # Requirements before processing
        req(
          input$input_file
          ,input$col_sample
          ,input$col_rep_1
          ,input$prior_location
          ,input$prior_scale
          ,input$var_option
          ,input$ci_width
        )

        if (input$duplicate_option == TRUE) {
          req(input$col_rep_2)
        }

        if (input$var_option == "SEM") {
          req(input$prior_n)
        }

        # Remove manually excluded rows from input data
        if (!is.null(input$data_table_rows_selected)) {
          df_reference <- df()[-input$data_table_rows_selected,]
        } else {
          df_reference <- df()
        }

        # Validation
        validate(
          need(
            expr = is.numeric(df_reference[[input$col_rep_1]]) == TRUE
            ,message = "The column in your data that represents the mean of the EQA
          samples does not consist of numbers. This may be because there are missing
          values or the column contains text. Please check/edit your input data and
          try again."
          )
          ,need(
            expr = is.numeric(input$prior_location) == TRUE
            ,message = "The mean value for the reference material must be a number."
          )
          ,need(
            expr = is.numeric(input$prior_scale) == TRUE
            ,message = "The variation value for the reference material must be a number."
          )
        )

        if (input$duplicate_option == TRUE) {
          validate(
            need(
              expr = is.numeric(df_reference[[input$col_rep_2]]) == TRUE
              ,message = "The column in your data that represents your method's duplicate
            measurements does not consist of numbers. This may be because there are
            missing values or the column contains text. Please check/edit your input
            data and try again."
            )
          )
        }

        if (input$var_option == "SEM") {
          validate(
            need(
              expr = input$prior_n != ""
              ,message = "If the variance measure quoted for the reference material
            is SEM, you are required to input the number of observations used to
            determine this value."
            )
            ,need(
              expr = is.numeric(input$prior_n)
              ,message = "The number of measurements used to determine the reference
            materials parameters must be a number."
            )
          )
        }

        # Update focus to regression analysis tab
        updateTabsetPanel(session, "ref_tabs", selected = "plots_ref")

        # Fit model to data
        fit$model <- fitModelRef(
          data = df_reference
          ,prior_location = input$prior_location
          ,prior_scale = input$prior_scale
          ,prior_n = input$prior_n
          ,col_sample = input$col_sample
          ,col_value_1 = input$col_rep_1
          ,col_value_2 = input$col_rep_2
          ,var_option = input$var_option
        )

        # Get bayes factor
        setProgress(0.33, "Getting Bayes factors...")
        fit$bf <- getBayesFactor(model = fit$model, prior_location = input$prior_location)

        # Plots
        setProgress(0.66, "Plotting results...")
        plot_output$plot <- plotRef(
          data = df_reference
          ,model = fit$model
          ,col_value_1 = input$col_rep_1
          ,col_value_2 = input$col_rep_2
          ,bayes_factor = fit$bf
        )

        # Inference boxes
        if (input$col_rep_2 != "" && !is.null(input$col_rep_2)) {
          fit$boxes <- boxesRef(
            model = fit$model
            ,prior_location = input$prior_location
            ,model_type = "reference_varying"
            ,ci_interval = input$ci_width
            ,bayes_factor = fit$bf
          )
        } else {
          fit$boxes <- boxesRef(
            model = fit$model
            ,prior_location = input$prior_location
            ,model_type = "reference"
            ,ci_interval = input$ci_width
            ,bayes_factor = fit$bf
          )
        }

        # Model checks
        if (input$col_rep_2 != "" && !is.null(input$col_rep_2)) {
          fit$checks <- modelChecks(model = fit$model, model_type = "reference_varying", ci_interval = input$ci_width)
        } else {
          fit$checks <- modelChecks(model = fit$model, model_type = "reference", ci_interval = input$ci_width)
        }

      })
    })

    output$plot_ref <- renderPlot({
      plot_output$plot
    })

    output$plot <- renderUI({
      plotOutput(ns("plot_ref"), height = window_height() * 0.7)
    })

    output$inferences <- renderUI({
      fit$boxes
    })

    # Model checks
    output$mcmc_checks <- renderPlot({
      fit$checks[[1]]
    })
    output$posteriors_checks <- renderPlot({
      fit$checks[[2]]
    })
    output$mcmc_plot <- renderUI({
      plotOutput(ns("mcmc_checks"), height = window_height() * 0.8)
    })
    output$posteriors_plot <- renderUI({
      plotOutput(ns("posteriors_checks"), height = window_height() * 0.8)
    })

  })

}
