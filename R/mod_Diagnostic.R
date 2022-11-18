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
              ,label = "Enter your preferred confidence interval (%):"
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
              ,selectInput(
                inputId = ns("curve_type")
                ,label = "Select the type of curve you wish to produce:"
                ,choices = c("ROC", "PR (precision-recall)")
              )
              ,actionButton(
                inputId = ns("run_model")
                ,label = "Fit curves"
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
              title = "Bootstrapped curves"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot_curves"))
                ,type = 6
              )
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Confusion matrix at optimal threshold"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot_confmat"))
                ,type = 6
              )
            )
          )
        )
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Bootstrapping results"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                htmlOutput(ns("area_under_the_curve"))
                ,type = 6
              )
              ,tags$head(
                tags$style(
                  paste0("#", ns("area_under_the_curve"), "{color: black; font-size: 16px;}")
                )
              )
            )
            ,box(
              title = "Diagnostic threshold"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot_threshold"))
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
    plot_output <- reactiveValues(curves = NULL, confmat = NULL, thresholds = NULL)
    fit <- reactiveValues(curves = NULL, thresholds = NULL, text = NULL)

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Observe toListen(), fit curves and update plot_output with fitted model if "run_model" is pressed
    observeEvent(toListen(), {

      withProgress(message = "Bootstrapping data, please wait...", {

        # Requirements before processing
        req(
          input$input_file
          ,input$col_label
          ,input$col_value
          ,input$curve_type
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

        # Fit curves
        bootstrap <- calcCurves(
          data = df_diag
          ,type = input$curve_type
          ,value = input$col_value
          ,label = input$col_label
          ,positive = input$positive_label
          ,ci_interval = input$ci_width
        )

        fit$curves <- bootstrap$curves
        fit$thresholds <- bootstrap$thresholds
        fit$text <- bootstrap$text

        # Bootstrapped curves
        setProgress(50, "Plotting results...")
        plot_output$curves <- plotCurves(
          data = df_diag
          ,type = input$curve_type
          ,value = input$col_value
          ,label = input$col_label
          ,positive = input$positive_label
          ,curves_data = fit$curves
          ,threshold = median(fit$thresholds)
          ,plot_height = window_height()
          ,plot_width = window_width()
        )

        # Confusion matrix
        plot_output$confmat <- plotConfMat(
          data = df_diag
          ,value = input$col_value
          ,label = input$col_label
          ,positive = input$positive_label
          ,threshold = median(fit$thresholds)
        )

        # Histogram of bootstrapped thresholds
        plot_output$thresholds <- plotThresholds(
          data = fit$thresholds
          ,plot_height = window_height()
          ,plot_width = window_width()
        )

      })

    })

    output$boot_curves <- renderPlot({
      plot_output$curves
    })
    output$plot_curves <- renderUI({
      plotOutput(ns("boot_curves"), height = window_height() * 0.6)
    })

    output$confmat <- renderPlot({
      plot_output$confmat
    })
    output$plot_confmat <- renderUI({
      plotOutput(ns("confmat"), height = window_height() * 0.6)
    })

    output$area_under_the_curve <- renderUI({
      fit$text
    })

    output$thresholds <- renderPlot({
      plot_output$thresholds
    })
    output$plot_threshold <- renderUI({
      plotOutput(ns("thresholds"), height = window_height() * 0.6)
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
