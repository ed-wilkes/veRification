#' boxesRef
#'
#' @param model rstanarm model object
#' @param prior_location numeric prior location
#' @param ci_interval numeric CI interval
#' @param model_type character string of: "reference" or "reference_varying"
#' @param bayes_factor bayesfactor object returned from getBayesFactor()
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
boxesRef <- function(model, prior_location, ci_interval, model_type, bayes_factor) {

  if (model_type == "reference_varying") {
    columns <- 1:4
  } else {
    columns <- 1:3
  }

  if (any(brms::rhat(model) > 1.1) || any(brms::neff_ratio(model)[columns] < 0.1)) {
    value_check <- "Fail"
    sub_check <- "Something has gone wrong with the Bayesian model. Interpret results with caution."
    colour_check <- "red"
    icon_check <- "triangle-exclamation"
  } else {
    value_check <- "Pass"
    sub_check <- "Bayesian model checks passed"
    colour_check <- "green"
    icon_check <- "circle-check"
  }

  # Summarise parameter values
  ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
  ci_upr <- 1 - ((1 - (ci_interval / 100)) / 2)

  summary <- paste0(
    format(round(median(as.matrix(model)[,1]), 2), nsmall = 2), " ("
    ,format(round(quantile(as.matrix(model)[,1], ci_lwr), 2), nsmall = 2), ", "
    ,format(round(quantile(as.matrix(model)[,1], ci_upr), 2), nsmall = 2), ")"
  )

  # Calculate Bayes factor
  bf <- round(exp(bayes_factor$log_BF), 1)

  width <- 12

  # Generate value boxes
  boxes <- list(
    p()
    ,fluidRow(
      valueBox(
        value = summary
        ,subtitle = "Posterior summary of the mean"
        ,color = "blue"
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          paste0(
          "NB: The numbers within parentheses represent the ", ci_interval, "%
          credible interval for the mean value of the measurand based on your measurements
          and the prior distribution for the reference material."
          )
        )
      )
    )
    ,p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = bf
        ,subtitle = paste0("Bayes factor against the null of ", prior_location)
        ,color = "blue"
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          "NB: A Bayes factor > 10 indicates strong evidence that the mean value measured
          using your method is different to that expected for the reference material."
        )
      )
    )
    ,p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = value_check
        ,color = colour_check
        ,subtitle = sub_check
        ,icon = icon(icon_check)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          "NB: The model checks performed here cover the basics and do not
            necessarily account for all the possible errors that might occur."
        )
      )
    )
  )
  return(boxes)
}
