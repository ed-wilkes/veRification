#' Diagnostic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Diagnostic_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    tabsetPanel(
      id = ns("diag_tabs")

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
            ,numericInput(
              inputId = ns("ci_width")
              ,label = "Enter your preferred credible interval (%):"
              ,value = 89
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
                inputId = ns("col_value")
                ,label = "Select the column that represents the measurements:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_label")
                ,label = "Select the column that represents the outcome label:"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("positive_label")
                ,label = "Select the outcome that represents a 'positive':"
                ,choices = ""
              )
              ,actionButton(
                inputId = ns("run_model")
                ,label = "Fit model"
                ,icon = icon("play")
                ,width = "100%"
              )
              ,width = 3
            )
          )
        )
      )

      # Plots and statistics
      ,tabPanel(
        value = "diag_tabs_plots"
        ,title = "Plots and analysis"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Model fit"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot_model"))
                ,type = 6
              )
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Model coefficients"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("model_coefficients"))
                ,type = 6
              )
              ,width = 4
            )
          )
        )
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Decision curve analysis"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot_dca"))
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

#' Diagnostic Server Functions
#'
#' @noRd
mod_Diagnostic_server <- function(id, window_height) {

  moduleServer( id, function(input, output, session) {

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
        ,"col_value"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_label"
        ,choices = colnames(df())
        ,selected = ""
      )
    })

    observe({ # must be separate to above call to observe()
      updateSelectInput(
        session
        ,"positive_label"
        ,choices = unique(df()[[input$col_label]])
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

    # Get data
    df <- reactive({
      readFile(input$input_file, headings = input$header)
    })

    # Column checks
    observeEvent(input$col_value,{
      checkInputNumeric(input, "col_value", df())
    })

    observeEvent(input$col_label,{
      checkInputLevels(input, "col_label", df())
    })

    # Render data
    output$data_table <- DT::renderDataTable(
      {df()}
      ,extension = c("Buttons", "Scroller")
      ,options = list(scrollX = TRUE)
      ,rownames = FALSE
    )

    # Reactive values definition
    plot_output <- reactiveValues(curves = NULL, dca = NULL, checks = NULL)
    fit <- reactiveValues(model = NULL, boxes = NULL)

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Observe toListen(), fit curves and update plot_output with fitted model if "run_model" is pressed
    observeEvent(toListen(), {

      withProgress(message = "Fitting model, please wait...", {

        # Requirements before processing
        req(
          input$input_file
          ,input$col_label
          ,input$col_value
          ,input$positive_label
          ,input$ci_width
        )

        if (!is.null(input$data_table_rows_selected)) {
          df_diag <- df()[-input$data_table_rows_selected,]
        } else {
          df_diag <- df()
        }

        # Update focus to regression analysis tab
        updateTabsetPanel(session, "diag_tabs", selected = "diag_tabs_plots")

        # Validation
        validate(
          need(
            expr = is.numeric(df_diag[[input$col_value]]) == TRUE
            ,message = "The column in your data that represents the measurements for your method
        does not consist of numbers. This may be because there are missing values or the column
        contains text. Please check/edit your input data and try again."
          )
          ,need(
            expr = length(unique(df_diag[[input$col_label]])) <= 2
            ,message = "The column in your data that represents the class labels contains
        more than 2 different values. Please check/edit your input data and try again."
          )
        )

        # Fit logistic regression model
        fit$model <- fitModelDiag(
          data = df_diag
          ,col_value = input$col_value
          ,col_label = input$col_label
          ,positive = input$positive_label
          ,ci_interval = input$ci_width
        )

        fit$boxes <- calcDiagCoef(
          data = df_diag
          ,model = fit$model
          ,col_value = input$col_value
          ,positive = input$positive_label
          ,ci_interval = input$ci_width
        )

        # Model plot
        setProgress(0.5, "Performing decision curve analysis...")
        plot_output$curves <- plotDiag(
          data = df_diag
          ,model = fit$model
          ,col_value = input$col_value
          ,col_label = input$col_label
          ,positive = input$positive_label
          ,ci_interval = input$ci_width
          ,plot_dim = cdata
        )

        plot_output$dca <- plotDCA(
          data = df_diag
          ,model = fit$model
          ,col_value = input$col_value
          ,col_label = input$col_label
          ,positive = input$positive_label
          ,ci_interval = input$ci_width
          ,plot_dim = cdata
        )

        plot_output$checks <- modelChecks(
          model = fit$model
          ,model_type = "logistic"
          ,ci_interval = input$ci_width
        )

      })

    })

    # Plot and box outputs
    output$logistic_curves <- plotly::renderPlotly({
      plot_output$curves
    })
    output$plot_model <- renderUI({
      plotly::plotlyOutput(ns("logistic_curves"), height = window_height() * 0.6)
    })

    output$dca_curves <- plotly::renderPlotly({
      plot_output$dca
    })
    output$plot_dca <- renderUI({
      plotly::plotlyOutput(ns("dca_curves"), height = window_height() * 0.6)
    })

    output$model_coefficients <- renderUI({
      fit$boxes
    })

    # Model checks
    output$mcmc_checks <- renderPlot({
      plot_output$checks[[1]]
    })
    output$posteriors_checks <- renderPlot({
      plot_output$checks[[2]]
    })
    output$mcmc_plot <- renderUI({
      plotOutput(ns("mcmc_checks"), height = window_height() * 0.8)
    })
    output$posteriors_plot <- renderUI({
      plotOutput(ns("posteriors_checks"), height = window_height() * 0.8)
    })

    ## This is a great bit of code, but is now obsolete
    # # Dynamically adjust sliderInput values based on input data
    # observe({
    #
    #   # Requirements before updating
    #   req(
    #     df()
    #     ,input$col_diag_label
    #     ,input$col_diag_value
    #     ,input$curve_type
    #     ,input$positive_label
    #   )
    #
    #   # Update slider values based on input data and automatically pick best threshold
    #   updateSliderInput(
    #     session
    #     ,"slider_threshold"
    #     ,value = optimThreshold(
    #       data = df()
    #       ,type = input$curve_type
    #       ,value = input$col_diag_value
    #       ,label = input$col_diag_label
    #       ,positive = input$positive_label
    #     )
    #     ,min = min(df()[[input$col_diag_value]], na.rm = T)
    #     ,max = max(df()[[input$col_diag_value]], na.rm = T)
    #     # ,step = nchar(
    #     #   strsplit(as.character(df()[[input$col_diag_value]][1]), "\\.")
    #     # )[[1]][2]
    #     ,step = round((max(df()[[input$col_diag_value]], na.rm = T) - min(df()[[input$col_diag_value]], na.rm = T))/100,1)
    #   )
    # })

  })
}
