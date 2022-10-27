#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyFeedback
#' @import shinycssloaders
#' @noRd
app_ui <- function(request) {

  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    ## Start dashboardPage() ----
    dashboardPage(

      ## Title ----
      title = "veRification"

      ## Header ----
      ,dashboardHeader(
        title = tags$a(
          href='http://www.github.com/ed-wilkes/veRification'
          ,tags$img(src = 'www/logo-veRification.png', width = '65%')
        )
      )

      ## Sidebar ----
      ,dashboardSidebar(
        sidebarMenu(
          menuItem("Information", tabName = "information", icon = icon("circle-info"))
          ,menuItem("Imprecision", tabName = "imprecision", icon = icon("chart-line"))
          ,menuItem("Trueness (EQA)", tabName = "trueness", icon = icon("bullseye"))
          ,menuItem("Trueness (reference)", tabName = "trueness_ref", icon = icon("vial"))
          ,menuItem("Method comparison", tabName = "comparison", icon = icon("vials"))
          ,menuItem("Diagnostic performance", tabName = "diagnostic", icon = icon("stethoscope"))
          #,menuItem("Export report", tabName = "export", icon = icon("file-alt")) # feature to be added
        )
      )

      ## Body ----
      ,dashboardBody(
        # Tags ----
        tags$style(
          HTML(
            ".skin-blue .main-header .logo {
          background-color:#1d3c72
        }
        .skin-blue .main-header .navbar {
          background-color:#1d3c72
        }
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#1d3c72
        }
       .box.box-solid.box-primary {
          border-bottom-color:#1d3c72;
          border-left-color:#1d3c72;
          border-right-color:#1d3c72;
          border-top-color:#1d3c72;
       }
       .h2 {
          margin-top:0;
       }"
          )
        )
        ,shinyFeedback::useShinyFeedback()
        ,tabItems(

          ## First tab "Information" ----
          tabItem(
            tabName = "information"
            #,imageOutput("acb_logo", inline = TRUE)
            ,fluidRow(
              box(
                title = "Welcome!"
                ,solidHeader = TRUE
                ,status = "primary"
                ,collapsible = TRUE
                ,width = 12
                ,h2("Welcome to the assay veRification application")
                ,p("This software has been fully validated for routine use; however, please use the utmost caution and vigilance when using the application. Upon use of the software, the user accepts responsibility for any issues that may arise.")
                ,p("Instructions for use of this application are shown in the boxes below.")
                ,p("This software was written using the R language and all the source code is available at github.com/ed-wilkes/assay-veRification")
                ,p("If you encounter any issues when using the app, please contact edmund.wilkes@nhs.net and ", strong("attach the data you are trying to analyse."))
                ,p(strong("Version:"), "0.1")
              )
            )
            ,fluidRow(
              box(
                title = "Imprecision"
                ,solidHeader = TRUE
                ,status = "primary"
                ,collapsible = TRUE
                ,collapsed = TRUE
                ,htmlOutput("instructions_imprecision")
              )
              ,box(
                title = "Trueness (EQA)"
                ,solidHeader = TRUE
                ,status = "primary"
                ,collapsible = TRUE
                ,collapsed = TRUE
                ,htmlOutput("instructions_trueness_eqa")
              )
            )
            ,fluidRow(
              box(
                title = "Trueness (reference)"
                ,solidHeader = TRUE
                ,status = "primary"
                ,collapsible = TRUE
                ,collapsed = TRUE
                ,htmlOutput("instructions_trueness_ref")
              )
              ,box(
                title = "Method comparison"
                ,solidHeader = TRUE
                ,status = "primary"
                ,collapsible = TRUE
                ,collapsed = TRUE
                ,htmlOutput("instructions_comparison")
              )
            )
            ,fluidRow(
              box(
                title = "Diagnostic performance"
                ,solidHeader = TRUE
                ,status = "primary"
                ,collapsible = TRUE
                ,collapsed = TRUE
                ,htmlOutput("instructions_diagnostic")
              )
            )
          )

          ## Second tab "Imprecision" ----
          ,tabItem(
            tabName = "imprecision"
            ,tabsetPanel(

              # Data input
              tabPanel(
                title = "Data input"
                ,p()
                ,fluidRow(
                  box(
                    title = "Data input"
                    ,solidHeader = TRUE
                    ,collapsible = TRUE
                    ,status = "primary"
                    ,fileInput(
                      inputId = "input_file_precision"
                      ,label = "Select your input file containing your data:"
                      ,accept = c(".csv", ".xls", ".xlsx")
                      ,multiple = FALSE
                    )
                    ,checkboxInput(
                      inputId = "header_precision"
                      ,label = "Are the column headers in row 1?"
                      ,value = TRUE
                    )
                    ,textInput(
                      inputId = "analyte_precision"
                      ,label = "Enter your analyte's name:"
                      ,placeholder = "e.g., 'Free T4'"
                    )
                  )
                )
                ,fluidRow(
                  conditionalPanel(
                    condition = "output.file_precision == true"
                    ,box(
                      title = "Your data"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,h6("NB: Click on rows you wish to exclude from the analysis!")
                      ,DT::dataTableOutput("data_imprecision")
                      ,width = 6
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_precision == true"
                    ,box(
                      title = "Column selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,selectInput(
                        inputId = "col_day"
                        ,label = "Select the column that represents days:"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_value"
                        ,label = "Select the column that represents the measurements:"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_level"
                        ,label = "Select the column that represents QC level(s):"
                        ,choices = ""
                      )
                      ,checkboxInput(
                        inputId = "cv_claims_test"
                        ,label = "Test against manufacturer's claims?"
                      )
                      ,width = 3
                    )
                  )
                  ,fluidRow(conditionalPanel(
                    condition = "input.cv_claims_test == true"
                    ,box(
                      title = "Manufacturer's claims"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,uiOutput("precision_claims")
                      ,width = 3
                    )
                  )
                  )
                )
              )

              # Plots
              ,tabPanel(
                title = "Plots"
                ,p()
                ,fluidRow(
                  conditionalPanel(
                    condition = "input.col_day != '' && input.value != ''"
                    ,column(
                      width = 12
                      ,shinycssloaders::withSpinner(
                        uiOutput("plot_imprecision")
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
                    condition = "output.file_precision == true"
                    ,column(
                      width = 12
                      ,shinycssloaders::withSpinner(
                        uiOutput("vca_results")
                        ,type = 6
                      )
                    )
                  )
                )
              )
            )
          ) # closes "imprecision"

          ## Third tab "Trueness (EQA)" ----
          ,tabItem(
            tabName = "trueness"
            ,tabsetPanel(
              id = "trueness_tabs"

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
                      inputId = "input_file_trueness"
                      ,label = "Select your input file containing your data:"
                      ,accept = c(".csv", ".xls", ".xlsx")
                      ,multiple = FALSE
                    )
                    ,checkboxInput(
                      inputId = "header_trueness"
                      ,label = "Are the column headers in row 1?"
                      ,value = TRUE
                    )
                    ,checkboxInput(
                      inputId = "duplicate_trueness"
                      ,label = "Has the measurand been assayed in duplicate?"
                      ,value = FALSE
                    )
                    ,selectInput(
                      inputId = "var_option"
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
                    condition = "output.file_trueness == true"
                    ,box(
                      title = "Analysis settings"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,selectInput(
                        inputId = "reg_method_eqa"
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
                        inputId = "cor_method_eqa"
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
                        inputId = "stat_method_eqa"
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
                        inputId = "run_model_eqa"
                        ,label = "Fit model"
                        ,icon = icon("play")
                        ,width = "100%"
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "input.reg_method_eqa == 'Deming'"
                    ,box(
                      title = "Deming regression parameters"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,numericInput(
                        inputId = "var_x_eqa"
                        ,label = "Enter the variance of method 1:"
                        ,value = 1
                      )
                      ,numericInput(
                        inputId = "var_y_eqa"
                        ,label = "Enter the variance of method 2:"
                        ,value = 1
                      )
                    )
                  )
                )
                ,fluidRow(
                  conditionalPanel(
                    condition = "output.file_trueness == true"
                    ,box(
                      title = "Your data"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,h6("NB: Click on rows you wish to exclude from the analysis!")
                      ,DT::dataTableOutput("data_trueness")
                      ,width = 6
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_trueness == true"
                    ,box(
                      title = "Column selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,width = 3
                      ,selectInput(
                        inputId = "col_mean_trueness"
                        ,label = "Select the column that represents the EQA target value (e.g., ALTM):"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_var_trueness"
                        ,label = "Select the column that represents the EQA measure of variation (e.g., SD):"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_n_trueness"
                        ,label = "Select the column that represents the number of observations:"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_rep_1_trueness"
                        ,label = "Select the column that represents the results from your method:"
                        ,choices = ""
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_trueness == true && input.duplicate_trueness == true"
                    ,box(
                      title = "Duplicate selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,width = 3
                      ,selectInput(
                        inputId = "col_rep_2_trueness"
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
                    condition = "output.file_trueness == true && input.run_model_eqa != 0"
                    ,column(
                      width = 6
                      ,shinycssloaders::withSpinner(
                        uiOutput("plot_trueness")
                        ,type = 6
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_trueness == true && input.run_model_eqa != 0"
                    ,box(
                      title = "Model parameters"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,shinycssloaders::withSpinner(
                        htmlOutput("trueness_cor_values")
                        ,type = 6
                      )
                      ,shinycssloaders::withSpinner(
                        htmlOutput("trueness_stats")
                        ,type = 6
                      )
                      ,width = 6
                      ,tags$head(
                        tags$style(
                          "#trueness_cor_values{color: black; font-size: 18px;}"
                          ,"#trueness_stats{color: black; font-size: 18px;}"
                        )
                      )
                    )
                    ,box(
                      title = "Statistical inferences"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,shinycssloaders::withSpinner(
                        uiOutput("trueness_test")
                        ,type = 6
                      )
                      ,width = 6
                    )
                  )
                )
              )
            )
          ) # closes "trueness"

          ## Fourth tab "Trueness (reference)" ----
          ,tabItem(
            tabName = "trueness_ref"
            ,tabsetPanel(
              id = "ref_tabs"

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
                      inputId = "input_file_ref"
                      ,label = "Select your input file containing your data:"
                      ,accept = c(".csv", ".xls", ".xlsx")
                      ,multiple = FALSE
                    )
                    ,checkboxInput(
                      inputId = "header_ref"
                      ,label = "Are the column headers in row 1?"
                      ,value = TRUE
                    )
                    ,checkboxInput(
                      inputId = "duplicate_ref"
                      ,label = "Has the measurand been assayed in duplicate?"
                      ,value = FALSE
                    )
                    ,numericInput(
                      inputId = "mean_ref"
                      ,label = "Enter the reference target value:"
                      ,value = NULL
                    )
                    ,numericInput(
                      inputId = "var_ref"
                      ,label = "Enter the reference measure of variance:"
                      ,value = NULL
                    )
                    ,selectInput(
                      inputId = "var_option_ref"
                      ,label = "What measure of variation is quoted for the reference material?"
                      ,choices = c(
                        "Select a measure of variance" = ""
                        ,"SD"
                        ,"CV (%)"
                        ,"SEM"
                      )
                    )
                    ,conditionalPanel(
                      condition = "input.var_option_ref == 'SEM'"
                      ,numericInput(
                        inputId = "n_ref"
                        ,label = "Enter the number of measurements for the reference target:"
                        ,value = NULL
                      )
                    )
                  )
                )
                ,fluidRow(
                  conditionalPanel(
                    condition = "output.file_ref == true"
                    ,box(
                      title = "Your data"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,h6("NB: Click on rows you wish to exclude from the analysis!")
                      ,DT::dataTableOutput("data_ref")
                      ,width = 6
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_ref == true"
                    ,box(
                      title = "Column selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,width = 3
                      ,selectInput(
                        inputId = "col_sample_ref"
                        ,label = "Select the column that represents the samples measured:"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_rep_1_ref"
                        ,label = "Select the column that represents the results from your method:"
                        ,choices = ""
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_ref == true && input.duplicate_ref == true"
                    ,box(
                      title = "Duplicate selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,width = 3
                      ,selectInput(
                        inputId = "col_rep_2_ref"
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
                    condition = "output.file_ref == true && input.col_rep_1_ref != '' && input.var_option_ref != ''"
                    ,column(
                      width = 8
                      ,shinycssloaders::withSpinner(
                        uiOutput("plot_trueness_ref")
                        ,type = 6
                      )
                    )
                    ,box(
                      title = "Statistical modelling"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,width = 4
                      ,shinycssloaders::withSpinner(
                        uiOutput("trueness_ref_tests")
                        ,type = 6
                      )
                    )
                  )
                )
              )
            )
          ) # closes "trueness_ref"

          ## Fifth tab "Method comparison" ----
          ,tabItem(
            tabName = "comparison"
            ,tabsetPanel(
              id = "comp_tabs"

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
                      inputId = "input_file_comparison"
                      ,label = "Select your input file containing your data:"
                      ,accept = c(".csv", ".xls", ".xlsx")
                      ,multiple = FALSE
                    )
                    ,checkboxInput(
                      inputId = "header_comparison"
                      ,label = "Are the column headers in row 1?"
                      ,value = TRUE
                    )
                    ,checkboxInput(
                      inputId = "duplicate_comparison"
                      ,label = "Has the measurand been assayed in duplicate?"
                      ,value = FALSE
                    )
                    ,textInput(
                      inputId = "analyte_comparison"
                      ,label = "Enter your analyte's name:"
                      ,placeholder = "e.g., 'Free T4'"
                    )
                    ,textInput(
                      inputId = "method_comparison_1"
                      ,label = "Enter the name of method 1:"
                      ,placeholder = "e.g., 'LC-MS/MS'"
                    )
                    ,textInput(
                      inputId = "method_comparison_2"
                      ,label = "Enter the name of method 2:"
                      ,placeholder = "e.g., 'Siemens Immulite'"
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_comparison == true"
                    ,box(
                      title = "Analysis settings"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,selectInput(
                        inputId = "reg_method_comp"
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
                        inputId = "cor_method_comp"
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
                        inputId = "stat_method_comp"
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
                        inputId = "altman_method_comp"
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
                        inputId = "run_model_comp"
                        ,label = "Fit model"
                        ,icon = icon("play")
                        ,width = "100%"
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "input.reg_method_comp == 'Deming'"
                    ,box(
                      title = "Deming regression parameters"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,numericInput(
                        inputId = "var_x_comp"
                        ,label = "Enter the variance of method 1:"
                        ,value = 1
                      )
                      ,numericInput(
                        inputId = "var_y_comp"
                        ,label = "Enter the variance of method 2:"
                        ,value = 1
                      )
                    )
                  )
                )
                ,fluidRow(
                  conditionalPanel(
                    condition = "output.file_comparison == true"
                    ,box(
                      title = "Your data"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,h6("NB: Click on rows you wish to exclude from the analysis!")
                      ,DT::dataTableOutput("data_comparison")
                      ,width = 6
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_comparison == true"
                    ,box(
                      title = "Column selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,selectInput(
                        inputId = "col_ref_1"
                        ,label = "Select the column that represents the measurement for method 1:"
                        ,choices = "")
                      ,selectInput(
                        inputId = "col_new_1"
                        ,label = "Select the column that represents the measurement for method 2:"
                        ,choices = "")
                      ,width = 3
                    )
                  )
                  ,conditionalPanel(
                    condition = "input.duplicate_comparison == true && output.file_comparison == true"
                    ,box(
                      title = "Duplicate selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,selectInput(
                        inputId = "col_ref_2"
                        ,label = "Select the column that represents the additional measurement for method 1:"
                        ,choices = "")
                      ,selectInput(
                        inputId = "col_new_2"
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
                    condition = "output.file_comparison == true && input.run_model_comp != 0"
                    ,column(
                      width = 6
                      ,shinycssloaders::withSpinner(
                        uiOutput("plot_comparison")
                        ,type = 6
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_comparison == true && input.run_model_comp != 0"
                    ,box(
                      title = "Model parameters"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,shinycssloaders::withSpinner(
                        htmlOutput("comparison_cor_values")
                        ,type = 6
                      )
                      ,shinycssloaders::withSpinner(
                        htmlOutput("comparison_stats")
                        ,type = 6
                      )
                      ,width = 6
                      ,tags$head(
                        tags$style(
                          "#comparison_cor_values{color: black; font-size: 18px;}"
                          ,"#comparison_stats{color: black; font-size: 18px;}"
                        )
                      )
                    )
                    ,box(
                      title = "Statistical inferences"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,shinycssloaders::withSpinner(
                        uiOutput("comparison_test")
                        ,type = 6
                      )
                      ,width = 6
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
                    condition = "output.file_comparison == true && input.altman_method_comp != ''"
                    ,column(
                      width = 6
                      ,shinycssloaders::withSpinner(
                        plotlyOutput("plot_altman")
                        ,type = 6
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_comparison == true && input.altman_method_comp != ''"
                    ,box(
                      title = "Summary statistics"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,shinycssloaders::withSpinner(
                        htmlOutput("altman_values")
                        ,type = 6
                      )
                      ,tags$head(
                        tags$style(
                          "#altman_values{color: black; font-size: 18px;}"
                        )
                      )
                    )
                    ## Legacy code for Bland-Altman statistical testing:
                    # ,box(
                    #   title = "Statistical testing"
                    #   ,solidHeader = TRUE
                    #   ,collapsible = TRUE
                    #   ,status = "primary"
                    #   ,uiOutput("altman_test")
                    #   ,width = 6
                    # )
                  )
                )
              )
            )
          ) # closes "comparison"

          ## Sixth tab "Diagnostic performance" ----
          ,tabItem(
            tabName = "diagnostic"
            ,tabsetPanel(

              # Data input
              tabPanel(
                title = "Data input"
                ,p()
                ,fluidRow(
                  box(
                    title = "Data input"
                    ,solidHeader = TRUE
                    ,collapsible = TRUE
                    ,status = "primary"
                    ,fileInput(
                      inputId = "input_file_diagnostic"
                      ,label = "Select your input file containing your data:"
                      ,accept = c(".csv", ".xls", ".xlsx")
                      ,multiple = FALSE
                    )
                    ,checkboxInput(
                      inputId = "header_diagnostic"
                      ,label = "Are the column headers in row 1?"
                      ,value = TRUE
                    )
                  )
                )
                ,fluidRow(
                  conditionalPanel(
                    condition = "output.file_diagnostic == true"
                    ,box(
                      title = "Your data"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,h6("NB: Click on rows you wish to exclude from the analysis!")
                      ,DT::dataTableOutput("data_diagnostic")
                      ,width = 6
                    )
                  )
                  ,conditionalPanel(
                    condition = "output.file_diagnostic == true"
                    ,box(
                      title = "Column selection"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,selectInput(
                        inputId = "col_diag_value"
                        ,label = "Select the column that represents the measurements:"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "col_diag_label"
                        ,label = "Select the column that represents the outcome label:"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "positive_label"
                        ,label = "Select the outcome that represents a 'positive':"
                        ,choices = ""
                      )
                      ,selectInput(
                        inputId = "curve_type"
                        ,label = "Select the type of curve you wish to produce:"
                        ,choices = c("ROC", "PR (precision-recall)")
                      )
                      ,width = 3
                    )
                  )
                )
              )

              # Plots and statistics
              ,tabPanel(
                title = "Plots and analysis"
                ,p()
                ,fluidRow(
                  conditionalPanel(
                    condition = "input.col_diag_value != '' && input.col_diag_label != '' && input.curve_type != ''"
                    ,column(
                      width = 6
                      ,shinycssloaders::withSpinner(
                        uiOutput("plot_diagnostic_curve")
                        ,type = 6
                      )
                    )
                  )
                  ,conditionalPanel(
                    condition = "input.col_diag_value != '' && input.col_diag_label != '' && input.curve_type != ''"
                    ,column(
                      width = 6
                      ,shinycssloaders::withSpinner(
                        uiOutput("plot_diagnostic_conf")
                        ,type = 6
                      )
                    )
                  )
                )
                ,p()
                ,fluidRow(
                  conditionalPanel(
                    condition = "input.col_diag_value != '' && input.col_diag_label != '' && input.curve_type != ''"
                    ,box(
                      title = "Summary statistics"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,shinycssloaders::withSpinner(
                        htmlOutput("area_under_the_curve")
                        ,type = 6
                      )
                      ,tags$head(
                        tags$style(
                          "#area_under_the_curve{color: black; font-size: 16px;}"
                        )
                      )
                    )
                    ,box(
                      title = "Diagnostic threshold"
                      ,solidHeader = TRUE
                      ,collapsible = TRUE
                      ,status = "primary"
                      ,sliderInput(
                        "slider_threshold"
                        ,label = "Choose your analyte threshold:"
                        ,value = 0
                        ,min = 0
                        ,max = 1
                      )
                    )
                  )
                )
              )
            )
          ) # closes "diagnostic performance"

          ## Seventh tab "Export report" ----
          ## Future code to be implemented
          # ,tabItem(
          #   tabName = "export"
          #   ,fluidRow(
          #     box(
          #       width = 4
          #       ,title = "Export .html report"
          #       ,status = "primary"
          #       ,solidHeader = TRUE
          #       ,strong("Choose the analyses to export:")
          #       ,checkboxInput(
          #         inputId = "export_imprecision"
          #         ,label = "Imprecision"
          #         ,value = FALSE
          #       )
          #       ,checkboxInput(
          #         inputId = "export_trueness"
          #         ,label = "Trueness"
          #         ,value = FALSE
          #       )
          #       ,checkboxInput(
          #         inputId = "export_comparison"
          #         ,label = "Method comparison"
          #         ,value = FALSE
          #       )
          #       ,checkboxInput(
          #         inputId = "export_diagnostic"
          #         ,label = "Diagnostic performance"
          #         ,value = FALSE
          #       )
          #       ,p()
          #       ,strong("Export your chosen analyses to a .html file:")
          #       ,p()
          #       ,downloadButton(outputId = "report", label = "Export report")
          #     )
          #   )
          # ) # closes "export report"

        ) # closes tabItems()

        ## Window sizing code ----
        ,tags$head(
          tags$script('
                var height = 0;
                var width = 0;
                $(document).on("shiny:connected", function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                $(window).resize(function(e) {
                  height = window.innerHeight;
                  width = window.innerWidth;
                  Shiny.onInputChange("height", height);
                  Shiny.onInputChange("width", width);
                });
                '
          )
        )

        ## Skin colour ----
        ,skin = "blue"

      ) # closes dashboardBody
    ) # closes dashboardPage
  ) # closes tagList
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "veRification"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

  )
}
