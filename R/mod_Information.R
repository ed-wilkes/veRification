#' Information UI Function
#'
#' @description A shiny Module for the "Information" tab
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Information_ui <- function(id) {

  ns <- NS(id)

  # UI elements
  tagList(

    fluidRow(
      box(
        title = "Welcome!"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,width = 12
        ,h2("Welcome to the assay veRification application")
        ,p("This software has been fully validated for routine use; however, please use the utmost caution and vigilance when using the application. Upon use of the software, the user accepts responsibility for any issues that may arise.")
        ,p("Instructions for use of this application are shown in the boxes below.")
        ,p("This software was written using the R language and all the source code is available at github.com/ed-wilkes/veRification")
        ,p("If you encounter any issues when using the app, please contact edmund.wilkes@nhs.net and ", strong("attach the data you are trying to analyse."))
      )
    )
    ,fluidRow(
      box(
        title = "Imprecision"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,collapsed = TRUE
        ,htmlOutput(ns("instructions_imprecision"))
      )
      ,box(
        title = "Trueness (EQA)"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,collapsed = TRUE
        ,htmlOutput(ns("instructions_trueness_eqa"))
      )
    )
    ,fluidRow(
      box(
        title = "Trueness (reference)"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,collapsed = TRUE
        ,htmlOutput(ns("instructions_trueness_ref"))
      )
      ,box(
        title = "Method comparison"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,collapsed = TRUE
        ,htmlOutput(ns("instructions_comparison"))
      )
    )
    ,fluidRow(
      box(
        title = "Diagnostic performance"
        ,solidHeader = TRUE
        ,status = "primary"
        ,collapsible = TRUE
        ,collapsed = TRUE
        ,htmlOutput(ns("instructions_diagnostic"))
      )
    )

  ) # ends tagList

}

#' Information Server Functions
#'
#' @noRd
mod_Information_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    output$instructions_imprecision <- renderUI({
      guideImprecision()
    })
    output$instructions_trueness_eqa <- renderUI({
      guideTruenessEQA()
    })
    output$instructions_trueness_ref <- renderUI({
      guideTruenessRef()
    })
    output$instructions_comparison <- renderUI({
      guideComparison()
    })
    output$instructions_diagnostic <- renderUI({
      guideDiagnostic()
    })

  })

}
