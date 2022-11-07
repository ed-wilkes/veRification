#' fitModelRef
#'
#' @param data data.frame input
#' @param prior_mean numeric mean of prior distribution
#' @param prior_var numeric variation of prior distribution
#' @param prior_n numeric number of observations used to determine prior mean
#' @param col_sample character string denoting column containing sample IDs
#' @param col_value_1 character string denoting column containing measurements
#' @param col_value_2 character string denoting column containing duplicate measurements
#' @param var_option character string denoting variance type
#'
#' @return rstanarm model object
#' @export
#'
#' @examples
fitModelRef <- function(data
                        ,prior_mean
                        ,prior_var
                        ,prior_n = NULL
                        ,col_sample
                        ,col_value_1
                        ,col_value_2 = NULL
                        ,var_option) {

  # Load rstanarm
  require(rstanarm)

  # Convert SEM/CV to SD as necessary
  if (var_option == "SEM") {
    prior_var <- prior_var * sqrt(prior_n)
  } else if (var_option == "CV (%)") {
    prior_var <- (prior_var / 100) * prior_mean
  }

  # Fit models
  if (!is.null(col_value_2) && col_value_2 != "") {

    # Pivot data for modelling
    data <- tidyr::pivot_longer(
      data
      ,cols = c(col_value_1, col_value_2)
      ,values_to = "value"
    ) %>%
      mutate(Distribution = "data")

    # Fit random effects model to include duplicate measurements
    sd_y <- sd(data$value)
    form <- paste0("value ~ 1 + (1|", col_sample, ")")
    fit <- rstanarm::stan_lmer(
      form = form
      ,data = data
      ,prior_intercept = normal(prior_mean, prior_var, autoscale = FALSE)
      ,prior_aux = exponential(1 / sd_y, autoscale = FALSE)
      ,iter = 20000
      ,adapt_delta = 0.99
      ,seed = 123
    )

  } else {

    sd_y <- sd(data[[col_value_1]])
    data$value <- data[[col_value_1]]
    fit <- rstanarm::stan_glm(
      form = value ~ 1
      ,data = data
      ,prior_intercept = normal(prior_mean, prior_var, autoscale = FALSE)
      ,prior_aux = exponential(1 / sd_y, autoscale = FALSE)
      ,iter = 20000
      ,adapt_delta = 0.99
      ,seed = 123
    )

  }

  return(fit)

}
