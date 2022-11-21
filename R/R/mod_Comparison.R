#' Comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Comparison_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    tabsetPanel(
      id = ns("comp_tabs")

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
            # ,textInput(
            #   inputId = ns("analyte")
            #   ,label = "Enter your analyte's name:"
            #   ,placeholder = "e.g., 'Testosterone'"
            # )
            ,textInput(
              inputId = ns("method_name_1")
              ,label = "Enter the name of method 1:"
              ,placeholder = "e.g., 'Abbott Alinity (nmol/L)'"
            )
            ,textInput(
              inputId = ns("method_name_2")
              ,label = "Enter the name of method 2:"
              ,placeholder = "e.g., 'LC-MS/MS (nmol/L)'"
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
                ,label = "Select the statistical test you wish to use:"
                ,choices = c(
                  "Select a method" = ""
                  ,"Paired t-test"
                  ,"Paired Wilcoxon (Mann-Whitney) test"
                  ,"Bayesian posterior summary"
                )
              )
              ,selectInput(
                inputId = ns("altman_method")
                ,multiple = FALSE
                ,selected = ""
                ,label = "Select the Bland-Altman y-axis you wish to plot:"
                ,choices = c(
                  "Select an option" = ""
                  ,"Absolute"
                  ,"Relative"
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
                inputId = ns("var_x")
                ,label = "Enter the variance of method 1:"
                ,value = 1
              )
              ,numericInput(
                inputId = ns("var_y")
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
              ,selectInput(
                inputId = ns("col_ref_1")
                ,label = "Select the column that represents the measurement for method 1:"
                ,choices = "")
              ,selectInput(
                inputId = ns("col_new_1")
                ,label = "Select the column that represents the measurement for method 2:"
                ,choices = "")
              ,width = 3
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("duplicate_option"), "\'] == true")
            ,box(
              title = "Duplicate selection"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,selectInput(
                inputId = ns("col_ref_2")
                ,label = "Select the column that represents the additional measurement for method 1:"
                ,choices = "")
              ,selectInput(
                inputId = ns("col_new_2")
                ,label = "Select the column that represents the additional measurement for method 2:"
                ,choices = "")
              ,width = 3
            )
          )
        )
      )

      # Regression analysis
      ,tabPanel(
        title = "Regression analysis"
        ,value = "comp_tabs_reg"
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

      # Bland-Altman analysis
      ,tabPanel(
        title = "Bland-Altman analysis"
        ,p()
        ,fluidRow(
          conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,column(
              width = 6
              ,shinycssloaders::withSpinner(
                uiOutput(ns("plot_altman"))
                ,type = 6
              )
            )
          )
          ,conditionalPanel(
            condition = paste0("output[\'", ns("file"), "\'] == true && input[\'", ns("run_model"), "\'] != 0")
            ,box(
              title = "Summary statistics"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,status = "primary"
              ,shinycssloaders::withSpinner(
                htmlOutput(ns("altman_values"))
                ,type = 6
              )
              ,tags$head(
                tags$style(
                  paste0("#", ns("altman_values"), "{color: black; font-size: 18px;}")
                )
              )
              ,width = 3
            )
          )
        )
      )

    ) # closes tabSetPanel
  ) # closes tagList

}

#' Comparison Server Functions
#'
#' @noRd
mod_Comparison_server <- function(id, window_height) {

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
        ,"col_ref_1"
        ,choices = colnames(df())
        ,selected = ""
      )
      updateSelectInput(
        session
        ,"col_new_1"
        ,choices = colnames(df())
        ,selected = ""
      )
    })

    observe({
      if (input$duplicate_option == FALSE) {
        updateSelectInput(
          session
          ,"col_ref_2"
          ,choices = colnames(df())
          ,selected = ""
        )
        updateSelectInput(
          session
          ,"col_new_2"
          ,choices = colnames(df())
          ,selected = ""
        )
      } else {
        updateSelectInput(
          session
          ,"col_ref_2"
          ,choices = colnames(df())
          ,selected = ""
        )
        updateSelectInput(
          session
          ,"col_new_2"
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
        showTab(inputId = "comp_tabs", target = "Bayesian model results")
      } else {
        updateSelectInput(
          session
          ,"stat_method"
          ,choices = c("Select a method" = "", "Paired t-test", "Paired Wilcoxon (Mann-Whitney) test")
        )
        hideTab(inputId = "comp_tabs", target = "Bayesian model results")
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
    observeEvent(input$col_ref_1,{
      checkInputNumeric(input, "col_ref_1", df())
    })

    observeEvent(input$col_new_1,{
      checkInputNumeric(input, "col_new_1", df())
    })

    observeEvent(input$col_ref_2,{
      checkInputNumeric(input, "col_ref_2", df())
    })

    observeEvent(input$col_new_2,{
      checkInputNumeric(input, "col_new_2", df())
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
    plot_output <- reactiveValues(plot = NULL, checks = NULL, ba_plot = NULL)
    fit <- reactiveValues(model = NULL, text_coef = NULL, text_cor = NULL, tests = NULL, text_ba = NULL)

    # Listen to "run_model" input
    toListen <- reactive({
      list(input$run_model) # this is a list in case extra things need to be added later
    })

    # Observe toListenComp(), fit model and update plot_output with fitted model if "run_model_comp" is pressed
    observeEvent(toListen(), {

      withProgress(message = "Fitting model, please wait...", {

        # Requirements before processing
        req(
          input$input_file
          ,input$cor_method
          ,input$stat_method
          ,input$altman_method
          ,input$reg_method
          ,input$col_ref_1
          ,input$col_new_1
          ,input$ci_width
        )

        if (input$duplicate_option == TRUE) {
          req(input$col_ref_2, input$col_new_2)
        }

        if (input$reg_method == "Deming") {
          req(input$var_x, input$var_y)
        }

        # Update focus to regression analysis tab
        updateTabsetPanel(session, "comp_tabs", selected = "comp_tabs_reg")

        settings <- list(as.numeric(input$var_x), as.numeric(input$var_y))

        # Remove manually excluded rows from input data
        if (!is.null(input$data_table_rows_selected)) {
          df_comp <- df()[-input$data_table_rows_selected,]
        } else {
          df_comp <- df()
        }

        # Validation
        validate(
          need(
            expr = is.numeric(df_comp[[input$col_ref_1]]) == TRUE
            ,message = "The column in your data that represents the measurements for method 1 do not
          consist of numbers. This may be because there are missing values or the column
          contains text. Please check/edit your input data and try again."
          )
          ,need(
            expr = is.numeric(df_comp[[input$col_new_1]]) == TRUE
            ,message = "The column in your data that represents the measurements for method 2 do not
          consist of numbers. This may be because there are missing values or the column
          contains text. Please check/edit your input data and try again."
          )
        )

        if (input$duplicate_option == TRUE) {
          validate(
            need(
              expr = is.numeric(df_comp[[input$col_ref_2]]) == TRUE
              ,message = "The column in your data that represents the duplicate measurements for method 1
            do not consist of numbers. This may be because there are missing values or the
            column contains text. Please check/edit your input data and try again."
            )
            ,need(
              expr = is.numeric(df_comp[[input$col_new_2]]) == TRUE
              ,message = "The column in your data that represents the duplicate measurements for method 2
            do not consist of numbers. This may be because there are missing values or the
            column contains text. Please check/edit your input data and try again."
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

        # Fit chosen model data and save in "fit" object
        fit$model <- fitModelComp(
          data = df_comp
          ,method = input$reg_method
          ,settings = settings
          ,value_x1 = input$col_ref_1
          ,value_x2 = input$col_ref_2
          ,value_y1 = input$col_new_1
          ,value_y2 = input$col_new_2
          ,ci_interval = input$ci_width
        )

        # Plot data from chosen model
        setProgress(50, message = "Plotting data ...")
        plot_output$plot <- plotComparison(
          data = df_comp
          ,method = input$reg_method
          ,value_x1 = input$col_ref_1
          ,value_x2 = input$col_ref_2
          ,value_y1 = input$col_new_1
          ,value_y2 = input$col_new_2
          ,model = fit$model
          ,x_name = input$method_name_1
          ,y_name = input$method_name_2
          ,plot_height = window_height()
        )

        # Gather parameters and save as UI components
        fit$text_coef <- calcCompCoef(
          method = input$reg_method
          ,model = fit$model
        )

        # Gather R2 values and save as UI components
        fit$text_cor <- calcCompCor(
          data = df_comp
          ,model = fit$model
          ,value_x1 = input$col_ref_1
          ,value_x2 = input$col_ref_2
          ,value_y1 = input$col_new_1
          ,value_y2 = input$col_new_2
          ,coef_type = input$cor_method
          ,ci_interval = input$ci_width
        )

        # Perform statistical inference
        fit$tests <- calcCompTest(
          data = df_comp
          ,model = fit$model
          ,value_x1 = input$col_ref_1
          ,value_x2 = input$col_ref_2
          ,value_y1 = input$col_new_1
          ,value_y2 = input$col_new_2
          ,method = input$stat_method
          ,ci_interval = input$ci_width
        )

        # Model checks
        if (input$reg_method == "Bayesian") {
          plot_output$checks <- modelChecks(model = fit$model, model_type = "regression", ci_interval = input$ci_width)
        }

        # Bland-Altman analysis
        setProgress(75, "Performing Bland-Altman analysis...")
        plot_output$ba_plot <- plotBlandAltman(
          data = df_comp
          ,method = input$altman_method
          ,value_x1 = input$col_ref_1
          ,value_x2 = input$col_ref_2
          ,value_y1 = input$col_new_1
          ,value_y2 = input$col_new_2
          ,x_name = input$method_name_1
          ,y_name = input$method_name_2
          ,ci_interval = input$ci_width
          ,plot_height = window_height()
        )

        fit$text_ba <- calcBAStats(
          data = df_comp
          ,method = input$altman_method
          ,value_x1 = input$col_ref_1
          ,value_x2 = input$col_ref_2
          ,value_y1 = input$col_new_1
          ,value_y2 = input$col_new_2
        )

      }) # ends withProgress

    })

    # Model fit plot
    output$model_fit <- renderPlotly({
      plot_output$plot
    })

    # Dynamic UI to display plot
    output$plot <- renderUI({
      plotlyOutput(ns("model_fit"))
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

    # Bland-Altman plot
    output$altman <- plotly::renderPlotly({
      plot_output$ba_plot
    })
    output$plot_altman <- renderUI({
      plotly::plotlyOutput(ns("altman"))
    })

    # Bland-Altman summary
    output$altman_values <- renderUI({
      fit$text_ba
    })

  })

}
