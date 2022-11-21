#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
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
          ,tags$img(src = 'www/logo-veRification.png', width = '70%')
        )
      )

      ## Sidebar ----
      ,dashboardSidebar(
        sidebarMenu(
          menuItem("Information", tabName = "information", icon = icon("circle-info"))
          ,menuItem("Imprecision", tabName = "imprecision", icon = icon("chart-line"))
          ,menuItem("Trueness (EQA)", tabName = "trueness", icon = icon("bullseye"))
          ,menuItem("Trueness (reference)", tabName = "reference", icon = icon("vial"))
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

          # First tab "Information" ----
          tabItem(
            tabName = "information"
            ,mod_Information_ui("information_tab")
          )

          # Second tab "Imprecision" ----
          ,tabItem(
            tabName = "imprecision"
            ,mod_Imprecision_ui("imprecision_tab")
          )

          # Third tab "Trueness (EQA)" ----
          ,tabItem(
            tabName = "trueness"
            ,mod_Trueness_ui("trueness_tab")
          )

          # Fourth tab "Trueness (reference)" ----
          ,tabItem(
            tabName = "reference"
            ,mod_Reference_ui("reference_tab")
          )

          # Fifth tab "Method comparison" ----
          ,tabItem(
            tabName = "comparison"
            ,mod_Comparison_ui("comparison_tab")
          )

          # Sixth tab "Diagnostic performance" ----
          ,tabItem(
            tabName = "diagnostic"
            ,mod_Diagnostic_ui("diagnostic_tab")
          )

          # Seventh tab "Export report" ----
          # Future code to be implemented
          # ,tabItem(
          #   tabName = "export"
          #   ,mod_Export_ui("export_tab")
          # )

        ) # closes tabItems()

        # Window sizing code ----
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

        # Skin colour ----
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
