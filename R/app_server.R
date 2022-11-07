#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @import plotly
#' @noRd
app_server <- function(input, output, session) {

  ## General functions ----
  # output$acb_logo <- renderImage({
  #   filename <- normalizePath(
  #     file.path("www/acb_logo.png")
  #   )
  #   list(src = filename)
  # }, deleteFile = FALSE)
  window_height <- reactive({ifelse(is.null(input$height), 0, as.numeric(input$height))})
  window_width <- reactive({ifelse(is.null(input$width), 0, as.numeric(input$width))})
  colours <- c("teal", "aqua", "blue", "navy", "black")
  # markdown_file <- normalizePath("inst/app/www/ACB_veRification_html_output_v0.2.Rmd")

  ## Instructions ----
  output$instructions_imprecision <- renderUI({guideImprecision()})
  output$instructions_trueness_eqa <- renderUI({guideTruenessEQA()})
  output$instructions_trueness_ref <- renderUI({guideTruenessRef()})
  output$instructions_comparison <- renderUI({guideComparison()})
  output$instructions_diagnostic <- renderUI({guideDiagnostic()})

  ## "Imprecision" tab ----
  output$file_precision <- reactive({
    return(!is.null(input$input_file_precision))
  })
  outputOptions(output, "file_precision", suspendWhenHidden = FALSE)

  # Update selectInputs
  observe({
    updateSelectInput(
      session
      ,"col_day"
      ,choices = colnames(df_precision())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_value"
      ,choices = colnames(df_precision())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_level"
      ,choices = colnames(df_precision())
      ,selected = ""
    )
  })

  # Data checks
  observeEvent(input$input_file_precision, {
    checkInputFile(input, "input_file_precision")
  })

  # Data
  df_precision <- reactive({
    readFile(input$input_file_precision, headings = input$header_precision)
  })

  # Column checks
  observeEvent(input$col_value,{
    checkInputNumeric(input, "col_value", df_precision())
  })

  # Render data
  output$data_imprecision <- DT::renderDataTable(
    {df_precision()}
    ,extension = c("Buttons", "Scroller")
    ,options = list(scrollX = TRUE)
    ,rownames = FALSE
  )

  # Manufacturer's claims
  output$precision_claims <- renderUI({

    # Requirements
    req(input$cv_claims_test, input$col_level)

    qc_levels <- unique(df_precision()[[input$col_level]])
    if (is.null(qc_levels)) {
      qc_levels <- 1
    }

    # Generate textInput boxes depending on number of levels
    lapply(seq_along(qc_levels), function(i) {
      numericInput(
        paste0("claims_", i)
        ,label = paste0("Manufacturer's total CV, level ", i, " (%)")
        ,value = NULL
      )
    })

  })

  # Generate reactive vector of claim values
  claims_vector <- reactive({
    qc_levels <- unique(df_precision()[[input$col_level]])
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
    qc_levels <- unique(df_precision()[[input$col_level]])
    if (is.null(qc_levels)) {
      qc_levels <- 1
    }

    lapply(seq_along(qc_levels), function(i) {

      input_id <- paste0("claims_", i)
      observeEvent(input[[input_id]], {
        if (is.na(input[[input_id]])) {
          shinyFeedback::showFeedbackWarning(
            inputId = input_id
            ,text = "Value must be provided!"
          )
        } else {
          shinyFeedback::hideFeedback(input_id)
        }
      })

    })

  })

  # Plot(s)
  output$plot_imprecision <- renderUI({

    # Requirements to start processing
    req(input$col_day, input$col_value, input$col_level)

    qc_levels <- unique(df_precision()[[input$col_level]])
    if (is.null(qc_levels)) {
      qc_levels <- 1
    }

    # Plot imprecision data as boxplots
    lapply(seq_along(qc_levels), function(i) {

      if (!is.null(input$data_imprecision_rows_selected)) {
        df_prec <- df_precision()[-input$data_imprecision_rows_selected, ]
      } else {
        df_prec <- df_precision()
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

      fluidRow(
        box(
          title = paste0("QC level ", i)
          ,collapsible = TRUE
          ,solidHeader = TRUE
          ,status = "primary"
          ,renderPlotly(
            plotPrecData(
              data = df_prec
              ,x_var = input$col_day
              ,y_var = input$col_value
              ,qc_level = i
              ,col_level = input$col_level
              ,analyte = input$analyte_precision
            )
          )
        )
      )

    }) # ends lapply

  })

  # brms model(s)
  output$vca_results <- renderUI({

    withProgress(message = "Fitting model(s), please wait...", {

      # Requirements before processing
      req(input$col_day, input$col_value, input$col_level)

      qc_levels <- unique(df_precision()[[input$col_level]])
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

      # Apply brms models to each level in data and return valueBoxes
      lapply(seq_along(qc_levels), function(i) {

        if (!is.null(input$data_imprecision_rows_selected)) {
          df <- df_precision()[-input$data_imprecision_rows_selected, ]
        } else {
          df <- df_precision()
        }

        # Validation
        validate(
          need(
            expr = is.numeric(df[[input$col_value]]) == TRUE
            ,message = "The column in your data that represents your measurements does
            not contain numbers. This may be because the column contains text or missing
            values. Please check and reupload your data and/or reselect your columns."
          )
          ,need(
            expr = anyNA(df[[input$col_level]]) == FALSE
            ,message = "The column in your data that represents the QC levels contains
            missing values. Please check and reupload your data and/or reslect your
            columns"
          )
        )

        #list(
        fluidRow(
          box(
            title = paste0("QC level ", i)
            ,solidHeader = TRUE
            ,collapsible = TRUE
            ,status = "primary"
            ,width = 12
            ,fitModelPrec(
              data = df
              ,x_var = input$col_day
              ,y_var = input$col_value
              ,qc_level = i
              ,col_level = input$col_level
              ,colours = colour_values
              ,test_claims = input$cv_claims_test
              ,claims_data = claim_values
            )
          )
        )

        # setProgress(i / length(qc_levels), "Fitting model(s), please wait...")

      }) # ends lapply

      # setProgress(100, "Finishing...")

    }) # ends withProgress

  })

  ## "Trueness (EQA)" tab ----
  output$file_trueness <- reactive({
    return(!is.null(input$input_file_trueness))
  })
  outputOptions(output, "file_trueness", suspendWhenHidden = FALSE)

  # Update selectInputs
  observe({
    updateSelectInput(
      session
      ,"col_sample_trueness"
      ,choices = colnames(df_trueness())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_mean_trueness"
      ,choices = colnames(df_trueness())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_var_trueness"
      ,choices = colnames(df_trueness())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_n_trueness"
      ,choices = colnames(df_trueness())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_rep_1_trueness"
      ,choices = colnames(df_trueness())
      ,selected = ""
    )
  })

  observe({
    if (input$duplicate_trueness == FALSE) {
      updateSelectInput(
        session
        ,"col_rep_2_trueness"
        ,choices = colnames(df_trueness())
        ,selected = ""
      )
    } else {
      updateSelectInput(
        session
        ,"col_rep_2_trueness"
        ,choices = colnames(df_trueness())
        ,selected = ""
      )
    }
  })

  observe({
    if (input$reg_method_eqa == "Bayesian") {
      updateSelectInput(
        session
        ,"stat_method_eqa"
        ,choices = "Bayesian posterior summary"
        ,selected = "Bayesian posterior summary"
      )
    } else {
      updateSelectInput(
        session
        ,"stat_method_eqa"
        ,choices = c("Select a method" = "", "Paired t-test", "Paired Wilcoxon (Mann-Whitney) test")
      )
    }
  })

  observe({
    if (input$reg_method_eqa == "Bayesian") {
      updateSelectInput(
        session
        ,"cor_method_eqa"
        ,choices = "Bayesian"
        ,selected = "Bayesian"
      )
    } else {
      updateSelectInput(
        session
        ,"cor_method_eqa"
        ,choices = c("Select a method" = "", "Kendall", "Pearson", "Spearman")
        ,selected = ""
      )
    }
  })

  # Data checks
  observeEvent(input$input_file_trueness, {
    checkInputFile(input, "input_file_trueness")
  })

  # Get data
  df_trueness <- reactive({
    readFile(input$input_file_trueness, headings = input$header_trueness)
  })

  # Column checks
  observeEvent(input$col_mean_trueness,{
    checkInputNumeric(input, "col_mean_trueness", df_trueness())
  })

  observeEvent(input$col_var_trueness,{
    checkInputNumeric(input, "col_var_trueness", df_trueness())
  })

  observeEvent(input$col_n_trueness,{
    checkInputNumeric(input, "col_n_trueness", df_trueness())
  })

  observeEvent(input$col_rep_1_trueness,{
    checkInputNumeric(input, "col_rep_1_trueness", df_trueness())
  })

  observeEvent(input$col_rep_2_trueness,{
    checkInputNumeric(input, "col_rep_2_trueness", df_trueness())
  })

  observeEvent(input$var_x_eqa,{
    checkInputNull(input, "var_x_eqa")
  })

  observeEvent(input$var_y_eqa,{
    checkInputNull(input, "var_y_eqa")
  })

  # Render data
  output$data_trueness <- DT::renderDataTable(
    {df_trueness()}
    ,extension = c("Buttons", "Scroller")
    ,options = list(scrollX = TRUE)
    ,rownames = FALSE
  )

  # Plots
  # Reactive values definition
  plot_output_eqa <- reactiveValues(plot = NULL)
  fit_eqa <- reactiveValues(model = NULL, text_coef = NULL, text_cor = NULL, tests = NULL)

  # Listen to "run_model" input
  toListenEQA <- reactive({
    list(input$run_model_eqa) # this is a list in case extra things need to be added later
  })

  # Observe toListenEQA(), fit model and update plot_output_eqa with fitted model if "run_model_eqa" is pressed
  observeEvent(toListenEQA(), {

    withProgress(message = "Fitting model, please wait...", {

      # Requirements before processing
      req(
        input$input_file_trueness
        ,input$var_option
        ,input$col_mean_trueness
        ,input$col_var_trueness
        ,input$col_n_trueness
        ,input$col_rep_1_trueness
        ,input$cor_method_eqa
        ,input$stat_method_eqa
        ,input$reg_method_eqa
      )

      if (input$duplicate_trueness == TRUE) {
        req(input$col_rep_2_trueness)
      }

      if (input$reg_method_comp == "Deming") {
        req(input$var_x_eqa, input$var_y_eqa)
      }

      # Update focus to regression analysis tab
      updateTabsetPanel(session, "trueness_tabs", selected = "trueness_tab_plots")

      settings <- list(as.numeric(input$var_x_eqa), as.numeric(input$var_y_eqa))

      if (!is.null(input$data_trueness_rows_selected)) {
        df_true <- df_trueness()[-input$data_trueness_rows_selected,]
      } else {
        df_true <- df_trueness()
      }

      # Validation
      validate(
        need(
          expr = is.numeric(df_true[[input$col_mean_trueness]]) == TRUE
          ,message = "The column in your data that represents the mean of the EQA
        samples does not consist of numbers. This may be because there are missing
        values or the column contains text. Please check/edit your input data and
        try again."
        )
        ,need(
          expr = is.numeric(df_true[[input$col_var_trueness]]) == TRUE
          ,message = "The column in your data that represents the variation of the EQA
        samples does not consist of numbers. This may be because there are missing
        values or the column contains text. Please check/edit your input data and
        try again."
        )
        ,need(
          expr = is.numeric(df_true[[input$col_n_trueness]]) == TRUE
          ,message = "The column in your data that represents the number of labs
        contributing to the EQA samples does not consist of numbers. This may be
        because there are missing values or the column contains text. Please check
        /edit your input data and try again."
        )
        ,need(
          expr = is.numeric(df_true[[input$col_rep_1_trueness]]) == TRUE
          ,message = "The column in your data that represents your method's measurements
        does not consist of numbers. This may be because there are missing values
        or the column contains text. Please check/edit your input data and try again."
        )
      )

      if (input$duplicate_trueness == TRUE) {
        validate(
          need(
            expr = is.numeric(df_true[[input$col_rep_2_trueness]]) == TRUE
            ,message = "The column in your data that represents your method's duplicate
          measurements does not consist of numbers. This may be because there are
          missing values or the column contains text. Please check/edit your input
          data and try again."
          )
        )
      }

      if (input$reg_method_eqa == "Deming") {
        validate(
          need(
            expr = is.numeric(input$var_x_eqa) == TRUE
            ,message = "The variance for method 1 must be a number."
          )
          ,need(
            expr = is.numeric(input$var_y_eqa) == TRUE
            ,message = "The variance for method 2 must be a number."
          )
        )
      }

      # Fit measurement error model
      fit_eqa$model <- fitModelEQA(
        data = df_true
        ,col_value_1 = input$col_rep_1_trueness
        ,col_value_2 = input$col_rep_2_trueness
        ,col_mean = input$col_mean_trueness
        ,col_var = input$col_var_trueness
        ,col_n = input$col_n_trueness
        ,var_option = input$var_option
        ,method = input$reg_method_eqa
        ,settings = settings
      )

      setProgress(50, message = "Plotting data ...")
      plot_output_eqa$plot <- plotEQA(
        data = df_true
        ,option_var = input$var_option
        ,col_mean = input$col_mean_trueness
        ,col_var = input$col_var_trueness
        ,col_n = input$col_n_trueness
        ,col_value_1 = input$col_rep_1_trueness
        ,col_value_2 = input$col_rep_2_trueness
        ,model = fit_eqa$model
        ,method = input$reg_method_eqa
        ,plot_height = window_height()
      )

      # Gather parameters and save as UI components
      fit_eqa$text_coef <- calcCompCoef(
        method = input$reg_method_eqa
        ,model = fit_eqa$model
      )

      # Gather R2 values and save as UI components
      fit_eqa$text_cor <- calcCompCor(
        data = df_true
        ,model = fit_eqa$model
        ,value_x1 = input$col_mean_trueness
        ,value_y1 = input$col_rep_1_trueness
        ,value_y2 = input$col_rep_2_trueness
        ,coef_type = input$cor_method_eqa
      )

      # Perform statistical inference
      fit_eqa$tests <- calcCompTest(
        data = df_true
        ,model = fit_eqa$model
        ,value_x1 = input$col_mean_trueness
        ,value_y1 = input$col_rep_1_trueness
        ,value_y2 = input$col_rep_2_trueness
        ,method = input$stat_method_eqa
      )

    }) # ends withProgress

  })

  # Model fit plot
  output$model_fit_eqa <- plotly::renderPlotly({
    plot_output_eqa$plot
  })

  # Dynamic UI to display plot
  output$plot_trueness <- renderUI({
    plotly::plotlyOutput("model_fit_eqa")
  })

  # Model parameters/coefficients
  output$trueness_stats <- renderUI({
    fit_eqa$text_coef
  })

  # Correlation coefficients
  output$trueness_cor_values <- renderUI({
    fit_eqa$text_cor
  })

  # Statistical inferences
  output$trueness_test <- renderUI({
    fit_eqa$tests
  })

  ## "Trueness (reference)" tab ----
  output$file_ref <- reactive({
    return(!is.null(input$input_file_ref))
  })
  outputOptions(output, "file_ref", suspendWhenHidden = FALSE)

  # Update selectInputs
  observe({
    updateSelectInput(
      session
      ,"col_sample_ref"
      ,choices = colnames(df_ref())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_rep_1_ref"
      ,choices = colnames(df_ref())
      ,selected = ""
    )
  })

  observe({
    if (input$duplicate_ref == FALSE) {
      updateSelectInput(
        session
        ,"col_rep_2_ref"
        ,choices = colnames(df_ref())
        ,selected = ""
      )
    } else {
      updateSelectInput(
        session
        ,"col_rep_2_ref"
        ,choices = colnames(df_ref())
        ,selected = ""
      )
    }
  })

  # Data checks
  observeEvent(input$input_file_ref, {
    checkInputFile(input, "input_file_ref")
  })

  # Get data
  df_ref <- reactive({
    readFile(input$input_file_ref, headings = input$header_ref)
  })

  # Column checks
  observeEvent(input$col_rep_1_ref,{
    checkInputNumeric(input, "col_rep_1_ref", df_ref())
  })

  observeEvent(input$col_rep_2_ref,{
    checkInputNumeric(input, "col_rep_2_ref", df_ref())
  })

  # Render data
  output$data_ref <- DT::renderDataTable(
    {df_ref()}
    ,extension = c("Buttons", "Scroller")
    ,options = list(scrollX = TRUE)
    ,rownames = FALSE
  )

  # Reactive values definition
  fit_ref <- reactiveValues(model = NULL)

  # Model fitting
  observe({

    withProgress(message = "Fitting model, please wait...", {

      # Requirements before processing
      req(
        input$input_file_ref
        ,input$col_sample_ref
        ,input$col_rep_1_ref
        ,input$mean_ref
        ,input$var_ref
        ,input$var_option_ref
      )

      if (input$duplicate_ref == TRUE) {
        req(input$col_rep_2_ref)
      }

      if (input$var_option == "SEM") {
        req(input$n_ref)
      }

      # Remove manually excluded rows from input data
      if (!is.null(input$data_ref_rows_selected)) {
        df_reference <- df_ref()[-input$data_ref_rows_selected,]
      } else {
        df_reference <- df_ref()
      }

      # Validation
      validate(
        need(
          expr = is.numeric(df_reference[[input$col_rep_1_ref]]) == TRUE
          ,message = "The column in your data that represents the mean of the EQA
          samples does not consist of numbers. This may be because there are missing
          values or the column contains text. Please check/edit your input data and
          try again."
        )
        ,need(
          expr = is.numeric(input$mean_ref) == TRUE
          ,message = "The mean value for the reference material must be a number."
        )
        ,need(
          expr = is.numeric(input$var_ref) == TRUE
          ,message = "The variation value for the reference material must be a number."
        )
      )

      if (input$duplicate_ref == TRUE) {
        validate(
          need(
            expr = is.numeric(df_reference[[input$col_rep_2_ref]]) == TRUE
            ,message = "The column in your data that represents your method's duplicate
            measurements does not consist of numbers. This may be because there are
            missing values or the column contains text. Please check/edit your input
            data and try again."
          )
        )
      }

      if (input$var_option_ref == "SEM") {
        validate(
          need(
            expr = input$n_ref != ""
            ,message = "If the variance measure quoted for the reference material
            is SEM, you are required to input the number of observations used to
            determine this value."
          )
          ,need(
            expr = is.numeric(input$n_ref)
            ,message = "The number of measurements used to determine the reference
            materials parameters must be a number."
          )
        )
      }

      # Fit model to data
      fit_ref$model <- fitModelRef(
        data = df_reference
        ,prior_mean = input$mean_ref
        ,prior_var = input$var_ref
        ,prior_n = input$n_ref
        ,col_sample = input$col_sample_ref
        ,col_value_1 = input$col_rep_1_ref
        ,col_value_2 = input$col_rep_2_ref
        ,var_option = input$var_option_ref
      )

      # Update focus to regression analysis tab
      updateTabsetPanel(session, "ref_tabs", selected = "plots_ref")

    })
  })

  # Plots
  output$plot_ref <- renderPlot({

    withProgress(message = "Plotting data, please wait...", {

      # Requirements before processing
      req(
        input$input_file_ref
        ,input$col_sample_ref
        ,input$col_rep_1_ref
        ,input$mean_ref
        ,input$var_ref
        ,input$var_option_ref
      )

      if (!is.null(input$data_ref_rows_selected)) {
        df_reference <- df_ref()[-input$data_ref_rows_selected,]
      } else {
        df_reference <- df_ref()
      }

      # Validation
      validate(
        need(
          expr = is.numeric(df_reference[[input$col_rep_1_ref]]) == TRUE
          ,message = "The column in your data that represents the mean of the EQA
          samples does not consist of numbers. This may be because there are missing
          values or the column contains text. Please check/edit your input data and
          try again."
        )
        ,need(
          expr = is.numeric(input$mean_ref) == TRUE
          ,message = "The mean value for the reference material must be a number."
        )
        ,need(
          expr = is.numeric(input$var_ref) == TRUE
          ,message = "The variation value for the reference material must be a number."
        )
      )

      if (input$duplicate_ref == TRUE) {
        validate(
          need(
            expr = is.numeric(df_reference[[input$col_rep_2_ref]]) == TRUE
            ,message = "The column in your data that represents your method's duplicate
            measurements does not consist of numbers. This may be because there are
            missing values or the column contains text. Please check/edit your input
            data and try again."
          )
        )
      }

      if (input$var_option_ref == "SEM") {
        validate(
          need(
            expr = input$n_ref != ""
            ,message = "If the variance measure quoted for the reference material
            is SEM, you are required to input the number of observations used to
            determine this value."
          )
          ,need(
            expr = is.numeric(input$n_ref)
            ,message = "The number of measurements used to determine the reference
            material's parameters must be a number."
          )
        )
      }

      plotRef(
        data = df_reference
        ,model = fit_ref$model
        ,col_value_1 = input$col_rep_1_ref
        ,col_value_2 = input$col_rep_2_ref
        ,prior_mean = input$mean_ref
        ,prior_var = input$var_ref
        ,prior_n = input$n_ref
        ,var_option = input$var_option_ref
      )

    })

  })

  output$plot_trueness_ref <- renderUI({
    plotOutput("plot_ref", height = input$height * 0.7)
  })

  # Generate boxes for model summaries
  output$trueness_ref_tests <- renderUI({

    # Requirements before processing
    req(
      input$input_file_ref
      ,input$col_sample_ref
      ,input$col_rep_1_ref
      ,input$mean_ref
      ,input$var_ref
      ,input$var_option_ref
    )

    # Validation
    validate(
      need(
        expr = is.numeric(input$mean_ref) == TRUE
        ,message = "The mean value for the reference material must be a number."
      )
      ,need(
        expr = is.numeric(input$var_ref) == TRUE
        ,message = "The variation value for the reference material must be a number."
      )
    )

    if (input$var_option_ref == "SEM") {
      validate(
        need(
          expr = input$n_ref != ""
          ,message = "If the variance measure quoted for the reference material
            is SEM, you are required to input the number of observations used to
            determine this value."
        )
        ,need(
          expr = is.numeric(input$n_ref)
          ,message = "The number of measurements used to determine the reference
            materials parameters must be a number."
        )
      )
    }

    boxesRef(
      model = fit_ref$model
      ,prior_mean = input$mean_ref
      ,prior_var = input$var_ref
      ,prior_n = input$n_ref
      ,var_option = input$var_option_ref
    )

  })

  ## "Method comparison" tab ----
  output$file_comparison <- reactive({
    return(!is.null(input$input_file_comparison))
  })
  outputOptions(output, "file_comparison", suspendWhenHidden = FALSE)

  # Update selectInputs
  observe({
    updateSelectInput(
      session
      ,"col_ref_1"
      ,choices = colnames(df_comparison())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_new_1"
      ,choices = colnames(df_comparison())
      ,selected = ""
    )
  })

  observe({
    if (input$duplicate_comparison == FALSE) {
      updateSelectInput(
        session
        ,"col_ref_2"
        ,choices = colnames(df_comparison())
      )
      updateSelectInput(
        session
        ,"col_new_2"
        ,choices = colnames(df_comparison())
      )
    } else {
      updateSelectInput(
        session
        ,"col_ref_2"
        ,choices = colnames(df_comparison())
      )
      updateSelectInput(
        session
        ,"col_new_2"
        ,choices = colnames(df_comparison())
      )
    }
  })

  observe({
    if (input$reg_method_comp == "Bayesian") {
      updateSelectInput(
        session
        ,"stat_method_comp"
        ,choices = "Bayesian posterior summary"
        ,selected = "Bayesian posterior summary"
      )
    } else {
      updateSelectInput(
        session
        ,"stat_method_comp"
        ,choices = c("Select a method" = "", "Paired t-test", "Paired Wilcoxon (Mann-Whitney) test")
      )
    }
  })

  observe({
    if (input$reg_method_comp == "Bayesian") {
      updateSelectInput(
        session
        ,"cor_method_comp"
        ,choices = "Bayesian"
        ,selected = "Bayesian"
      )
    } else {
      updateSelectInput(
        session
        ,"cor_method_comp"
        ,choices = c("Select a method" = "", "Kendall", "Pearson", "Spearman")
        ,selected = ""
      )
    }
  })

  # Data checks
  observeEvent(input$input_file_comparison, {
    checkInputFile(input, "input_file_comparison")
  })

  # Get data
  df_comparison <- reactive({
    readFile(input$input_file_comparison, headings = input$header_comparison)
  })

  # Column checks
  observeEvent(input$col_ref_1,{
    checkInputNumeric(input, "col_ref_1", df_comparison())
  })

  observeEvent(input$col_new_1,{
    checkInputNumeric(input, "col_new_1", df_comparison())
  })

  observeEvent(input$col_ref_2,{
    checkInputNumeric(input, "col_ref_2", df_comparison())
  })

  observeEvent(input$col_new_2,{
    checkInputNumeric(input, "col_new_2", df_comparison())
  })

  observeEvent(input$var_x_comp,{
    checkInputNull(input, "var_x_comp")
  })

  observeEvent(input$var_y_comp,{
    checkInputNull(input, "var_y_comp")
  })

  # Render data
  output$data_comparison <- DT::renderDataTable(
    {df_comparison()}
    ,extension = c("Buttons", "Scroller")
    ,options = list(scrollX = TRUE)
    ,rownames = FALSE
  )

  # Reactive values definition
  plot_output_comp <- reactiveValues(plot = NULL)
  fit_comp <- reactiveValues(model = NULL, text_coef = NULL, text_cor = NULL, tests = NULL)

  # Listen to "run_model" input
  toListenComp <- reactive({
    list(input$run_model_comp) # this is a list in case extra things need to be added later
  })

  # Observe toListenComp(), fit model and update plot_output_comp with fitted model if "run_model_comp" is pressed
  observeEvent(toListenComp(), {

    withProgress(message = "Fitting model, please wait...", {

      # Requirements before processing
      req(
        input$input_file_comparison
        ,input$cor_method_comp
        ,input$stat_method_comp
        ,input$altman_method_comp
        ,input$reg_method_comp
        ,input$col_ref_1
        ,input$col_new_1
      )

      if (input$duplicate_trueness == TRUE) {
        req(input$col_ref_2, input$col_new_2)
      }

      if (input$reg_method_comp == "Deming") {
        req(input$var_x_comp, input$var_y_comp)
      }

      # Update focus to regression analysis tab
      updateTabsetPanel(session, "comp_tabs", selected = "comp_tabs_reg")

      settings <- list(as.numeric(input$var_x_comp), as.numeric(input$var_y_comp))

      # Remove manually excluded rows from input data
      if (!is.null(input$data_comparison_rows_selected)) {
        df_comp <- df_comparison()[-input$data_comparison_rows_selected,]
      } else {
        df_comp <- df_comparison()
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

      if (input$duplicate_comparison == TRUE) {
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

      if (input$reg_method_comp == "Deming") {
        validate(
          need(
            expr = is.numeric(input$var_x_comp) == TRUE
            ,message = "The variance for method 1 must be a number."
          )
          ,need(
            expr = is.numeric(input$var_y_comp) == TRUE
            ,message = "The variance for method 2 must be a number."
          )
        )
      }

      # Fit chosen model data and save in "fit_comp" object
      fit_comp$model <- fitModelComp(
        data = df_comp
        ,method = input$reg_method_comp
        ,settings = settings
        ,value_x1 = input$col_ref_1
        ,value_x2 = input$col_ref_2
        ,value_y1 = input$col_new_1
        ,value_y2 = input$col_new_2
      )

      # Plot data from chosen model
      setProgress(50, message = "Plotting data ...")
      plot_output_comp$plot <- plotComparison(
        data = df_comp
        ,method = input$reg_method_comp
        ,value_x1 = input$col_ref_1
        ,value_x2 = input$col_ref_2
        ,value_y1 = input$col_new_1
        ,value_y2 = input$col_new_2
        ,model = fit_comp$model
        ,x_name = input$method_comparison_1
        ,y_name = input$method_comparison_2
        ,plot_height = window_height()
      )

      # Gather parameters and save as UI components
      fit_comp$text_coef <- calcCompCoef(
        method = input$reg_method_comp
        ,model = fit_comp$model
      )

      # Gather R2 values and save as UI components
      fit_comp$text_cor <- calcCompCor(
        data = df_comp
        ,model = fit_comp$model
        ,value_x1 = input$col_ref_1
        ,value_x2 = input$col_ref_2
        ,value_y1 = input$col_new_1
        ,value_y2 = input$col_new_2
        ,coef_type = input$cor_method_comp
      )

      # Perform statistical inference
      fit_comp$tests <- calcCompTest(
        data = df_comp
        ,model = fit_comp$model
        ,value_x1 = input$col_ref_1
        ,value_x2 = input$col_ref_2
        ,value_y1 = input$col_new_1
        ,value_y2 = input$col_new_2
        ,method = input$stat_method_comp
      )

    }) # ends withProgress

  })

  # Model fit plot
  output$model_fit_comp <- plotly::renderPlotly({
    plot_output_comp$plot
  })

  # Dynamic UI to display plot
  output$plot_comparison <- renderUI({
    plotly::plotlyOutput("model_fit_comp")
  })

  # Model parameters/coefficients
  output$comparison_stats <- renderUI({
    fit_comp$text_coef
  })

  # Correlation coefficients
  output$comparison_cor_values <- renderUI({
    fit_comp$text_cor
  })

  # Statistical inferences
  output$comparison_test <- renderUI({
    fit_comp$tests
  })

  # Bland-Altman analysis
  output$plot_altman <- renderPlotly({

    # Requirements before processing
    req(
      input$input_file_comparison
      ,input$altman_method_comp
      ,input$reg_method_comp
      ,input$col_ref_1
      ,input$col_new_1
    )

    if (!is.null(input$data_comparison_rows_selected)) {
      df_altman <- df_comparison()[-input$data_comparison_rows_selected,]
    } else {
      df_altman <- df_comparison()
    }

    # Validation
    validate(
      need(
        expr = is.numeric(df_altman[[input$col_ref_1]]) == TRUE
        ,message = "The column in your data that represents the measurements for method 1 do not
          consist of numbers. This may be because there are missing values or the column
          contains text. Please check/edit your input data and try again."
      )
      ,need(
        expr = is.numeric(df_altman[[input$col_new_1]]) == TRUE
        ,message = "The column in your data that represents the measurements for method 2 do not
          consist of numbers. This may be because there are missing values or the column
          contains text. Please check/edit your input data and try again."
      )
    )

    if (input$duplicate_comparison == TRUE) {
      validate(
        need(
          expr = is.numeric(df_altman[[input$col_ref_2]]) == TRUE
          ,message = "The column in your data that represents the duplicate measurements for method 1
            do not consist of numbers. This may be because there are missing values or the
            column contains text. Please check/edit your input data and try again."
        )
        ,need(
          expr = is.numeric(df_altman[[input$col_new_2]]) == TRUE
          ,message = "The column in your data that represents the duplicate measurements for method 2
            do not consist of numbers. This may be because there are missing values or the
            column contains text. Please check/edit your input data and try again."
        )
      )
    }

    plotBlandAltman(
      data = df_altman
      ,method = input$altman_method_comp
      ,value_x1 = input$col_ref_1
      ,value_x2 = input$col_ref_2
      ,value_y1 = input$col_new_1
      ,value_y2 = input$col_new_2
      ,x_name = input$method_comparison_1
      ,y_name = input$method_comparison_2
      ,plot_height = window_height()
    )

  })

  # Output of various summary statistics from the Bland-Altman analysis
  output$altman_values <- renderUI({

    # Requirements before processing
    req(
      input$input_file_comparison
      ,input$altman_method_comp
      ,input$reg_method_comp
      ,input$col_ref_1
      ,input$col_new_1
    )

    if (!is.null(input$data_comparison_rows_selected)) {
      df_altman <- df_comparison()[-input$data_comparison_rows_selected,]
    } else {
      df_altman <- df_comparison()
    }

    # Validation
    validate(
      need(
        expr = is.numeric(df_altman[[input$col_ref_1]]) == TRUE
        ,message = "The column in your data that represents the measurements for method 1 do not
          consist of numbers. This may be because there are missing values or the column
          contains text. Please check/edit your input data and try again."
      )
      ,need(
        expr = is.numeric(df_altman[[input$col_new_1]]) == TRUE
        ,message = "The column in your data that represents the measurements for method 2 do not
          consist of numbers. This may be because there are missing values or the column
          contains text. Please check/edit your input data and try again."
      )
    )

    if (input$duplicate_comparison == TRUE) {
      validate(
        need(
          expr = is.numeric(df_altman[[input$col_ref_2]]) == TRUE
          ,message = "The column in your data that represents the duplicate measurements for method 1
            do not consist of numbers. This may be because there are missing values or the
            column contains text. Please check/edit your input data and try again."
        )
        ,need(
          expr = is.numeric(df_altman[[input$col_new_2]]) == TRUE
          ,message = "The column in your data that represents the duplicate measurements for method 2
            do not consist of numbers. This may be because there are missing values or the
            column contains text. Please check/edit your input data and try again."
        )
      )
    }

    calcBAStats(
      data = df_altman
      ,method = input$altman_method_comp
      ,value_x1 = input$col_ref_1
      ,value_x2 = input$col_ref_2
      ,value_y1 = input$col_new_1
      ,value_y2 = input$col_new_2
    )

  })

  ## Deprecated statistical testing for Bland-Altman analyses
  ## These are better served by the Bayesian measurement error regression model
  # output$altman_test <- renderUI({
  #
  #   if (!is.null(input$data_comparison_rows_selected)) {
  #     df_comp <- df_comparison()[-input$data_comparison_rows_selected,]
  #   } else {
  #     df_comp <- df_comparison()
  #   }
  #
  #   # Column checks
  #   if (!is.numeric(df_comp[[input$col_ref_1]])) {
  #     stop(
  #       "The column in your data that represents the measurements for method 1 do not
  #           consist of numbers. This may be because there are missing values or the column
  #           contains text. Please check/edit your input data and try again."
  #     )
  #   }
  #   if (!is.numeric(df_comp[[input$col_new_1]])) {
  #     stop(
  #       "The column in your data that represents the measurements for method 2 do not
  #           consist of numbers. This may be because there are missing values or the column
  #           contains text. Please check/edit your input data and try again."
  #     )
  #   }
  #   if (input$duplicate_comparison == TRUE && !is.numeric(df_comp[[input$col_ref_2]])) {
  #     stop(
  #       "The column in your data that represents the duplicate measurements for method 1
  #           do not consist of numbers. This may be because there are missing values or the
  #           column contains text. Please check/edit your input data and try again."
  #     )
  #   }
  #   if (input$duplicate_comparison == TRUE && !is.numeric(df_comp[[input$col_new_2]])) {
  #     stop(
  #       "The column in your data that represents the duplicate measurements for method 2
  #           do not consist of numbers. This may be because there are missing values or the
  #           column contains text. Please check/edit your input data and try again."
  #     )
  #   }
  #
  #   calcBATest(
  #     data = df_comp
  #     ,method = input$altman_method_comp
  #     ,value_x1 = input$col_ref_1
  #     ,value_x2 = input$col_ref_2
  #     ,value_y1 = input$col_new_1
  #     ,value_y2 = input$col_new_2
  #   )
  #
  # })

  ## "Diagnostic" tab ----
  output$file_diagnostic <- reactive({
    return(!is.null(input$input_file_diagnostic))
  })
  outputOptions(output, "file_diagnostic", suspendWhenHidden = FALSE)

  # Update selectInputs
  observe({
    updateSelectInput(
      session
      ,"col_diag_value"
      ,choices = colnames(df_diagnostic())
      ,selected = ""
    )
    updateSelectInput(
      session
      ,"col_diag_label"
      ,choices = colnames(df_diagnostic())
      ,selected = ""
    )
  })

  observe({ # must be separate to above call to observe()
    updateSelectInput(
      session
      ,"positive_label"
      ,choices = unique(df_diagnostic()[[input$col_diag_label]])
      ,selected = ""
    )
  })

  # Data checks
  observeEvent(input$input_file_diagnostic, {
    checkInputFile(input, "input_file_diagnostic")
  })

  # Get data
  df_diagnostic <- reactive({
    readFile(input$input_file_diagnostic, headings = input$header_diagnostic)
  })

  # Column checks
  observeEvent(input$col_diag_value,{
    checkInputNumeric(input, "col_diag_value", df_diagnostic())
  })

  observeEvent(input$col_diag_label,{
    checkInputLevels(input, "col_diag_label", df_diagnostic())
  })

  # Dynamically adjust sliderInput values based on input data
  observe({

    # Requirements before updating
    req(
      df_diagnostic()
      ,input$col_diag_label
      ,input$col_diag_value
      ,input$curve_type
      ,input$positive_label
    )

    # Update slider values based on input data
    updateSliderInput(
      session
      ,"slider_threshold"
      ,value = mean(df_diagnostic()[[input$col_diag_value]], na.rm = T)
      ,min = min(df_diagnostic()[[input$col_diag_value]], na.rm = T)
      ,max = max(df_diagnostic()[[input$col_diag_value]], na.rm = T)
      ,step = nchar(
        strsplit(as.character(df_diagnostic()[[input$col_diag_value]][1]), "\\.")
      )[[1]][2]
    )
  })

  # Render data
  output$data_diagnostic <- DT::renderDataTable(
    {df_diagnostic()}
    ,extension = c("Buttons", "Scroller")
    ,options = list(scrollX = TRUE)
    ,rownames = FALSE
  )

  # Plots and statistics
  output$diagnostic_curve <- renderPlot({

    # Requirements before processing
    req(
      input$input_file_diagnostic
      ,input$col_diag_label
      ,input$col_diag_value
      ,input$curve_type
      ,input$positive_label
    )

    if (!is.null(input$data_diagnostic_rows_selected)) {
      df_diag <- df_diagnostic()[-input$data_diagnostic_rows_selected,]
    } else {
      df_diag <- df_diagnostic()
    }

    print(length(unique(df_diag[[input$col_diag_label]])))

    # Validation
    validate(
      need(
        expr = is.numeric(df_diag[[input$col_diag_value]]) == TRUE
        ,message = "The column in your data that represents the measurements for your method
        does not consist of numbers. This may be because there are missing values or the column
        contains text. Please check/edit your input data and try again."
      )
      ,need(
        expr = length(unique(df_diag[[input$col_diag_label]])) <= 2
        ,message = "The column in your data that represents the class labels contains
        more than 2 different values. Please check/edit your input data and try again."
      )
    )

    plotAUC(
      data = df_diag
      ,type = input$curve_type
      ,value = input$col_diag_value
      ,label = input$col_diag_label
      ,positive = input$positive_label
      ,plot_height = window_height()
      ,plot_width = window_width()
      ,threshold = input$slider_threshold
    )

  })

  output$plot_diagnostic_curve <- renderUI({
    plotOutput("diagnostic_curve", height = input$height * 0.6)
  })

  output$diagnostic_conf <- renderPlot({

    # Requirements before processing
    req(
      input$input_file_diagnostic
      ,input$col_diag_label
      ,input$col_diag_value
      ,input$curve_type
      ,input$positive_label
    )

    if (!is.null(input$data_diagnostic_rows_selected)) {
      df_conf <- df_diagnostic()[-input$data_diagnostic_rows_selected,]
    } else {
      df_conf <- df_diagnostic()
    }

    # Validation
    validate(
      need(
        expr = is.numeric(df_conf[[input$col_diag_value]]) == TRUE
        ,message = "The column in your data that represents the measurements for your method
        does not consist of numbers. This may be because there are missing values or the column
        contains text. Please check/edit your input data and try again."
      )
    )

    plotConfMat(
      data = df_conf
      ,value = input$col_diag_value
      ,label = input$col_diag_label
      ,positive = input$positive_label
      ,threshold = input$slider_threshold
    )

  })

  output$plot_diagnostic_conf <- renderUI({
    plotOutput("diagnostic_conf", height = input$height * 0.6)
  })

  # Calculate AUC and output as HTML
  output$area_under_the_curve <- renderUI({

    # Requirements before processing
    req(
      input$input_file_diagnostic
      ,input$col_diag_label
      ,input$col_diag_value
      ,input$curve_type
      ,input$positive_label
    )

    if (!is.null(input$data_diagnostic_rows_selected)) {
      df_auc <- df_diagnostic()[-input$data_diagnostic_rows_selected,]
    } else {
      df_auc <- df_diagnostic()
    }

    # Validation
    validate(
      need(
        expr = is.numeric(df_auc[[input$col_diag_value]]) == TRUE
        ,message = "The column in your data that represents the measurements for your method
        does not consist of numbers. This may be because there are missing values or the column
        contains text. Please check/edit your input data and try again."
      )
    )

    calcAUC(
      data = df_auc
      ,type = input$curve_type
      ,value = input$col_diag_value
      ,label = input$col_diag_label
      ,positive = input$positive_label
      ,threshold = input$slider_threshold
    )

  })

  ## "Export report" tab ----
  ## Code to be implemented in the future
  # observe({
  #
  #   # Check if input files have been uploaded
  #   imprecision_checkbox <- !is.null(input$input_file_precision)
  #   trueness_checkbox <- !is.null(input$input_file_trueness)
  #   comparison_checkbox <- !is.null(input$input_file_comparison)
  #   diagnostic_checkbox <- !is.null(input$input_file_diagnostic)
  #
  #   updateCheckboxInput(
  #     session
  #     ,"export_imprecision"
  #     ,value = imprecision_checkbox
  #   )
  #   updateCheckboxInput(
  #     session
  #     ,"export_trueness"
  #     ,value = trueness_checkbox
  #   )
  #   updateCheckboxInput(
  #     session
  #     ,"export_comparison"
  #     ,value = comparison_checkbox
  #   )
  #   updateCheckboxInput(
  #     session
  #     ,"export_diagnostic"
  #     ,value = diagnostic_checkbox
  #   )
  #
  # })
  #
  # output$report <- downloadHandler(
  #
  #   filename = paste0("Assay_veRification_report_", Sys.Date(), ".html")
  #   #filename = "report.html"
  #   ,content = function(file) {
  #     withProgress(message = "Rendering report, please wait...", {
  #
  #       # Copy report file to temporary directory
  #       temp_rmd_file <- file.path(tempdir(), markdown_file)
  #       file.copy(markdown_file, temp_rmd_file, overwrite = TRUE)
  #
  #       # Define options
  #       options_and_inputs <- list(
  #
  #         # Report to process
  #         precision_report = input$export_imprecision
  #         ,trueness_report = input$export_trueness
  #         ,comparison_report = input$export_comparison
  #         ,diagnostic_report = input$export_diagnostic
  #
  #         # Precision options
  #         ,header_precision = input$header_precision
  #         ,analyte_precision = input$analyte_precision
  #         ,rows_precision = input$data_imprecision_rows_selected
  #         ,col_day = input$col_day
  #         ,col_value = input$col_value
  #         ,col_level = input$col_level
  #         ,cv_claims_test = input$cv_claims_test
  #         ,qc_levels = unique(df_precision()[[input$col_level]])
  #         ,claims = claims_vector()
  #
  #         # Trueness options
  #         ,header_trueness = input$header_trueness
  #         ,duplicate_trueness = input$duplicate_trueness
  #         ,rows_trueness = input$data_trueness_rows_selected
  #         ,var_option = input$var_option
  #         ,col_mean_trueness = input$col_mean_trueness
  #         ,col_var_trueness = input$col_var_trueness
  #         ,col_n_trueness = input$col_n_trueness
  #         ,col_rep_1_trueness = input$col_rep_1_trueness
  #         ,col_rep_2_trueness = input$col_rep_2_trueness
  #
  #         # Comparison options
  #         ,header_comparison = input$header_comparison
  #         ,analyte_comparison = input$analyte_comparison
  #         ,rows_comparison = input$data_comparison_rows_selected
  #         ,duplicate_comparison = input$duplicate_comparison
  #         ,method_comparison_1 = input$method_comparison_1
  #         ,method_comparison_2 = input$method_comparison_2
  #         ,reg_method_comp = input$reg_method_comp
  #         ,cor_method_comp = input$cor_method_comp
  #         ,stat_method_comp = input$stat_method_comp
  #         ,altman_method_comp = input$altman_method_comp
  #         ,var_x_comp = input$var_x_comp
  #         ,var_y_comp = input$var_y_comp
  #         ,col_ref_1 = input$col_ref_1
  #         ,col_ref_2 = input$col_ref_2
  #         ,col_new_1 = input$col_new_1
  #         ,col_new_2 = input$col_new_2
  #
  #         # Diagnostic options
  #         ,header_diagnostic = input$header_diagnostics
  #         ,rows_diagnostic = input$data_diagnostic_rows_selected
  #         ,col_diag_value = input$col_diag_value
  #         ,col_diag_label = input$col_diag_label
  #         ,curve_type = input$curve_type
  #         ,positive_label = input$positive_label
  #         ,slider_threshold = input$slider_threshold
  #
  #       )
  #
  #       # Set up parameters
  #       params <- list(
  #         data_precision = df_precision()
  #         ,data_trueness = df_trueness()
  #         ,data_comparison = df_comparison()
  #         ,data_diagnostic = df_diagnostic()
  #         ,options = options_and_inputs
  #         ,colours = colours
  #         ,render_by_shiny = TRUE
  #       )
  #
  #       # Render report
  #       rmarkdown::render(
  #         input = temp_rmd_file
  #         ,output_file = file
  #         ,params = params
  #         ,envir = new.env(parent = globalenv())
  #       )
  #
  #     })
  #   }
  # ) # closes downloadHandler()

}
