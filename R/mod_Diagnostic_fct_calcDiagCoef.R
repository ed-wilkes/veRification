#' calcDiagCoef
#'
#' @param data data.frame input
#' @param model mcr or brms model object
#' @param col_value character string representing column of values
#' @param positive character string representing a "positive" case
#' @param ci_interval numeric CI interval
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
calcDiagCoef <- function(data
                         ,model
                         ,col_value
                         ,positive
                         ,ci_interval) {

  # Parameter values
  ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
  ci_upr <- 1 - ((1 - (ci_interval / 100)) / 2)
  intercept <- paste0(
    format(round(median(plogis(as.matrix(model)[,1])), 2), nsmall = 2), " ("
    ,format(round(quantile(plogis(as.matrix(model)[,1]), ci_lwr), 2), nsmall = 2), ", "
    ,format(round(quantile(plogis(as.matrix(model)[,1]), ci_upr), 2), nsmall = 2), ")"
  )

  coef <- paste0(
    format(round(median((exp(as.matrix(model)[,2]) - 1) * 100), 1), nsmall = 1), "% ("
    ,format(round(quantile((exp(as.matrix(model)[,2]) - 1) * 100, ci_lwr), 1), nsmall = 1), "%, "
    ,format(round(quantile((exp(as.matrix(model)[,2]) - 1) * 100, ci_upr), 1), nsmall = 1), "%)"
  )

  width <- 12

  # Basic model checks
  if (any(brms::rhat(model) > 1.1) || any(brms::neff_ratio(model)[1:2] < 0.1)) {
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

  # Generate value boxes
  boxes <- list(
    p()
    ,fluidRow(
      valueBox(
        value = intercept
        ,subtitle = paste0("Probability of ", positive, " at the median value of ", col_value)
        ,color = "blue"
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          paste0("NB: The numbers within parentheses represent the ", ci_interval,
                 "% credible interval for this parameter based on a weakly informative prior distribution."
          )
        )
      )
    )
    ,p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = coef
        ,subtitle = paste0("Change in the odds of ", positive, " given one unit increase in ", col_value)
        ,color = "blue"
          ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          paste0("NB: The numbers within parentheses represent the ", ci_interval,
                 "% credible interval for this parameter based on a weakly informative prior distribution."
          )
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
