#' boxesRef
#'
#' @param model rstanarm model object
#' @param prior_mean numeric prior mean
#' @param prior_var numeric prior SD/SEM/CV
#' @param var_option character representing variance measure type
#' @param prior_n numeric prior n
#' @param ci_interval numeric CI interval
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
boxesRef <- function(model, prior_mean, prior_var, var_option, prior_n, ci_interval) {

  # Convert SEM/CV to SD as necessary
  if (var_option == "SEM") {
    prior_var <- prior_var * sqrt(prior_n)
  } else if (var_option == "CV (%)") {
    prior_var <- (prior_var / 100) * prior_mean
  }

  # Model checks
  if (any(summary(model)[,"Rhat"] > 1.1) || any(summary(model)[,"n_eff"] / 20000 < 0.1)) {
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

  sum_zero <- sum(as.matrix(model)[,1] > prior_mean) / length(as.matrix(model)[,1])
  icon_tested_one <- ifelse(
    sum_zero > ci_upr
    ,yes = "triangle-exclamation"
    ,no = ifelse(
      sum_zero < ci_lwr
      ,yes = "triangle-exclamation"
      ,no = "circle-check"
    )
  )
  colour_tested_one <- ifelse(
    sum_zero > ci_upr
    ,yes = "red"
    ,no = ifelse(
      sum_zero < ci_lwr
      ,yes = "red"
      ,no = "green"
    )
  )

  # Calculate Bayes factor
  bf_obj <-  bayestestR::bayesfactor(
    posterior = as.matrix(model)[,1]
    ,prior = bayestestR::distribution_normal(10000, mean = prior_mean, sd = prior_var)
    ,null = prior_mean
  )
  bf <- round(exp(bf_obj$log_BF), 1)

  icon_tested_two <- ifelse(
    bf > 10
    ,yes = "triangle-exclamation"
    ,no = "circle-check"
  )
  colour_tested_two <- ifelse(
    bf > 10
    ,yes = "red"
    ,no = "green"
  )
  width <- 12

  # Generate value boxes
  boxes <- list(
    p()
    ,fluidRow(
      valueBox(
        value = summary
        ,subtitle = "Posterior median"
        ,color = colour_tested_one
        ,icon = icon(icon_tested_one)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          paste0(
          "NB: The numbers within parentheses represent the ", ci_interval, "%
          credible interval for the value of the measurand based on your measurements
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
        ,subtitle = paste0("Bayes factor against the null of ", prior_mean)
        ,color = colour_tested_two
        ,icon = icon(icon_tested_two)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          "NB: A Bayes factor > 10 indicates strong evidence that mean value measured
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
