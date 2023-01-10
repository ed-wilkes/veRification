#' mod_Trueness UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Trueness_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    tabsetPanel(
      id = ns("trueness_tabs")

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
              inputId = ns("ci_width")
              ,label = "Enter your preferred confidence/credible interval (%):"
              ,value = 89
            )
            ,selectInput(
              inputId = ns("var_option")
              ,label = "What measure of variation is quoted for the EQA samples?"
              ,choices = c(
                "Select a measure of variance" = ""
                ,"SD"
                ,"CV (%)"
                ,"SEM"
              )
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true")
            ,box(
              title = "Analysis settings"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,selectInput(
                inputId = ns("reg_method")
                ,multiple = FALSE
                ,label = "Select the regression method you wish to use:"
                ,choices = c(
                  "Select a method" = ""
                  ,"Bayesian"
                  ,"Passing-Bablok"
                  ,"Deming"
                  ,"Ordinary least-squares"
                )
              )
              ,selectInput(
                inputId = ns("cor_method")
                ,multiple = FALSE
                ,selected = ""
                ,label = "Select the correlation cofficient you wish to use:"
                ,choices = c(
                  "Select a method" = ""
                  ,"Bayesian"
                  ,"Spearman"
                  ,"Pearson"
                  ,"Kendall")
              )
              ,selectInput(
                inputId = ns("stat_method")
                ,multiple = FALSE
                ,label = "Select the statistical inference you wish to use:"
                ,choices = c(
                  "Select a method" = ""
                  ,"Paired t-test"
                  ,"Paired Wilcoxon (Mann-Whitney) test"
                  ,"Bayesian posterior summary"
                )
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
            condition = paste0("input[\'", ns("reg_method"), "\'] == 'Deming'")
            ,box(
              title = "Deming regression parameters"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,numericInput(
                inputId = "var_x"
                ,label = "Enter the variance of method 1:"
                ,value = 1
              )
              ,numericInput(
                inputId = "var_y"
                ,label = "Enter the variance of method 2:"
                ,value = 1
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
                inputId = ns("col_mean")
                ,label = "Select the column that represents the EQA target value (e.g., ALTM):"
                ,choices = ""
              )
              ,selectInput(
                inputId = ns("col_var")
                ,label = "Select the column that represents the EQA measure of variation (e.g., SD):"
                ,choices = ""
              )
              ,conditionalPanel(
                condition = paste0("input[\'", ns("var_option"), "\'] == 'SEM'")
                ,selectInput(
                  inputId = ns("col_n")
                  ,label = "Select the column that represents the number of observations:"
                  ,choices = ""
                )
              )
              ,selectInput(
                inputId = ns("col_rep_1")
                ,label = "Select the column that represents the results from your method:"
                ,choices = ""
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

      # Regression analysis
      ,tabPanel(
        title = "Regression analysis"
        ,value = "trueness_tab_plots"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 6
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot"))
                ,type = 6
              )
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Model parameters"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                htmlOutput(ns("cor_values"))
                ,type = 6
              )
              ,shinycssloaders::withSpinner(
                htmlOutput(ns("stats"))
                ,type = 6
              )
              ,width = 4
              ,tags$head(
                tags$style(
                  paste0("#", ns("cor_values"), "{color: black; font-size: 18px;}")
                  ,paste0("#", ns("stats"), "{color: black; font-size: 18px;}")
                )
              )
            )
            ,box(
              title = "Statistical inferences"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                uiOutput(ns("inferences"))
                ,type = 6
              )
              ,width = 4
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

#' mod_Trueness Server Functions
#'
#' @noRd
mod_Trueness_server <- function(id, window_height) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$file <- reactive({
      return(!is.null(input$input_file))
    })
    outputOptions(output, "file", suspendWhenHidden = FALSE)

    # Update selectInputs
    observe({
      updateSelectInput(
        session
        ,"col_mean"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_var"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_n"
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

    observe({
      if (input$reg_method == "Bayesian") {
        updateSelectInput(
          session
          ,"stat_method"
          ,choices = "Bayesian posterior summary"
          ,selected = "Bayesian posterior summary"
        )
        showTab(inputId = "trueness_tabs", target = "Bayesian model diagnostics")
      } else {
        updateSelectInput(
          session
          ,"stat_method"
          ,choices = c("Select a method" = "", "Paired t-test", "Paired Wilcoxon (Mann-Whitney) test")
        )
        hideTab(inputId = "trueness_tabs", target = "Bayesian model diagnostics")
      }
    })

    observe({
      if (input$reg_method == "Bayesian") {
        updateSelectInput(
          session
          ,"cor_method"
          ,choices = "Bayesian"
          ,selected = "Bayesian"
        )
      } else {
        updateSelectInput(
          session
          ,"cor_method"
          ,choices = c("Select a method" = "", "Kendall", "Pearson", "Spearman")
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

    # Column checks
    observeEvent(input$col_mean,{
      checkInputNumeric(input, "col_mean", df())
    })

    observeEvent(input$col_var,{
      checkInputNumeric(input, "col_var", df())
    })

    observeEvent(input$col_n,{
      checkInputNumeric(input, "col_n", df())
    })

    observeEvent(input$col_rep_1,{
      checkInputNumeric(input, "col_rep_1", df())
    })

    observeEvent(input$col_rep_2,{
      checkInputNumeric(input, "col_rep_2", df())
    })

    observeEvent(input$var_x,{
      checkInputNull(input, "var_x")
    })

    observeEvent(input$var_y,{
      checkInputNull(input, "var_y")
    })

    # Render data
    output$data_table <- DT::renderDataTable(
      {df()}
      ,extension = c("Buttons", "Scroller")
      ,options = list(scrollX = TRUE)
      ,rownames = FALSE
    )

    # Reactive values definition
    plot_output <- reactiveValues(plot = NULL, checks = NULL)
    fit <- reactiveValues(model = NULL, text_coef = NULL, text_cor = NULL, tests = NULL)

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Observe toListen(), fit model and update plot_output with fitted model if "run_model" is pressed
    observeEvent(toListen(), {

      withProgress(message = "Fitting model, please wait...", {

        # Requirements before processing
        req(
          input$input_file
          ,input$var_option
          ,input$col_mean
          ,input$col_var
          ,input$col_rep_1
          ,input$cor_method
          ,input$stat_method
          ,input$reg_method
          ,input$ci_width
        )

        if (input$duplicate_option == TRUE) {
          req(input$col_rep_2)
        }

        if (input$reg_method == "Deming") {
          req(input$var_x, input$var_y)
        }

        if (input$var_option == "SEM") {
          req(input$col_n)
        }

        settings <- list(as.numeric(input$var_x), as.numeric(input$var_y))

        if (!is.null(input$data_table_rows_selected)) {
          df_true <- df()[-input$data_table_rows_selected,]
        } else {
          df_true <- df()
        }

        # Validation
        validate(
          need(
            expr = is.numeric(df_true[[input$col_mean]]) == TRUE
            ,message = "The column in your data that represents the mean of the EQA
        samples does not consist of numbers. This may be because there are missing
        values or the column contains text. Please check/edit your input data and
        try again."
          )
          ,need(
            expr = is.numeric(df_true[[input$col_var]]) == TRUE
            ,message = "The column in your data that represents the variation of the EQA
        samples does not consist of numbers. This may be because there are missing
        values or the column contains text. Please check/edit your input data and
        try again."
          )
          ,need(
            expr = is.numeric(df_true[[input$col_rep_1]]) == TRUE
            ,message = "The column in your data that represents your method's measurements
        does not consist of numbers. This may be because there are missing values
        or the column contains text. Please check/edit your input data and try again."
          )
        )

        if (input$var_option == "SEM") {
          validate(
            need(
              expr = is.numeric(df_true[[input$col_n]]) == TRUE
              ,message = "The column in your data that represents the number of labs
          contributing to the EQA samples does not consist of numbers. This may be
          because there are missing values or the column contains text. Please check
          /edit your input data and try again."
            )
          )
        }

        if (input$duplicate_option == TRUE) {
          validate(
            need(
              expr = is.numeric(df_true[[input$col_rep_2]]) == TRUE
              ,message = "The column in your data that represents your method's duplicate
          measurements does not consist of numbers. This may be because there are
          missing values or the column contains text. Please check/edit your input
          data and try again."
            )
          )
        }

        if (input$reg_method == "Deming") {
          validate(
            need(
              expr = is.numeric(input$var_x) == TRUE
              ,message = "The variance for method 1 must be a number."
            )
            ,need(
              expr = is.numeric(input$var_y) == TRUE
              ,message = "The variance for method 2 must be a number."
            )
          )
        }

        # Update focus to regression analysis tab
        updateTabsetPanel(session, "trueness_tabs", selected = "trueness_tab_plots")

        # Fit measurement error model
        fit$model <- fitModelEQA(
          data = df_true
          ,col_value_1 = input$col_rep_1
          ,col_value_2 = input$col_rep_2
          ,col_mean = input$col_mean
          ,col_var = input$col_var
          ,col_n = input$col_n
          ,var_option = input$var_option
          ,method = input$reg_method
          ,settings = settings
        )

        setProgress(0.5, message = "Plotting data ...")
        plot_output$plot <- plotEQA(
          data = df_true
          ,option_var = input$var_option
          ,col_mean = input$col_mean
          ,col_var = input$col_var
          ,col_n = input$col_n
          ,col_value_1 = input$col_rep_1
          ,col_value_2 = input$col_rep_2
          ,model = fit$model
          ,method = input$reg_method
          ,plot_height = window_height()
        )

        # Gather parameters and save as UI components
        fit$text_coef <- calcCompCoef(
          method = input$reg_method
          ,model = fit$model
        )

        # Gather R2 values and save as UI components
        fit$text_cor <- calcCompCor(
          data = df_true
          ,model = fit$model
          ,value_x1 = input$col_mean
          ,value_y1 = input$col_rep_1
          ,value_y2 = input$col_rep_2
          ,coef_type = input$cor_method
          ,ci_interval = input$ci_width
        )

        # Perform statistical inference
        fit$tests <- calcCompTest(
          data = df_true
          ,model = fit$model
          ,value_x1 = input$col_mean
          ,value_y1 = input$col_rep_1
          ,value_y2 = input$col_rep_2
          ,method = input$stat_method
          ,ci_interval = input$ci_width
        )

        # Summarise model checks
        if (input$reg_method == "Bayesian") {
          plot_output$checks <- modelChecks(
            model = fit$model
            ,model_type = "regression"
            ,ci_interval = input$ci_width
          )
        }

      }) # ends withProgress

    })

    # Model fit plot
    output$model_fit <- plotly::renderPlotly({
      plot_output$plot
    })

    # Dynamic UI to display plot
    output$plot <- renderUI({
      plotly::plotlyOutput(ns("model_fit"))
    })

    # Model parameters/coefficients
    output$stats <- renderUI({
      fit$text_coef
    })

    # Correlation coefficients
    output$cor_values <- renderUI({
      fit$text_cor
    })

    # Statistical inferences
    output$inferences <- renderUI({
      fit$tests
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

  })

}

