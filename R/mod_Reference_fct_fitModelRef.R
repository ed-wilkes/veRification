#' fitModelRef
#'
#' @param data data.frame input
#' @param prior_location numeric mean of prior distribution
#' @param prior_scale numeric variation of prior distribution
#' @param prior_n numeric number of observations used to determine prior mean
#' @param col_sample character string denoting column containing sample IDs
#' @param col_value_1 character string denoting column containing measurements
#' @param col_value_2 character string denoting column containing duplicate measurements
#' @param var_option character string denoting variance type
#'
#' @return brms model object
#' @export
#'
#' @examples
fitModelRef <- function(data
                        ,prior_location
                        ,prior_scale
                        ,prior_n = NULL
                        ,col_sample
                        ,col_value_1
                        ,col_value_2 = NULL
                        ,var_option) {

  # Load brms
  require(brms)

  # Convert SEM/CV to SD as necessary
  if (var_option == "SEM") {
    prior_scale <- prior_scale * sqrt(prior_n)
  } else if (var_option == "CV (%)") {
    prior_scale <- (prior_scale / 100) * prior_location
  }

  # Fit models
  if (!is.null(col_value_2) && col_value_2 != "") {

    # Pivot data for modelling
    data <- tidyr::pivot_longer(
      data
      ,cols = c(any_of(col_value_1), any_of(col_value_2))
      ,values_to = "value"
    ) %>%
      dplyr::mutate(Distribution = "data")

    sd_y <- sd(data$value)
    form <- paste0("value ~ 1 + (1|", col_sample, ")") # includes duplicates as varying effects

    prior_obj <- c(
      prior(student_t(30, prior_location, prior_scale), class = "Intercept")
      ,prior(exponential(1 / sd_y), class = "sd")
      ,prior(exponential(1 / sd_y), class = "sigma")
    )

  } else {

    sd_y <- sd(data[[col_value_1]])
    data$value <- data[[col_value_1]]
    form <- "value ~ 1"

    prior_obj <- c(
      prior(student_t(30, prior_location, prior_scale), class = "Intercept")
      ,prior(exponential(1 / sd_y), class = "sigma")
    )

  }

  stanvars <- brms::stanvar(sd_y, name = "sd_y")+
    brms::stanvar(prior_location, name = "prior_location")+
    brms::stanvar(prior_scale, name = "prior_scale")

  fit <- brms::brm(
    form = form
    ,data = data
    ,prior = prior_obj
    ,iter = 20000
    ,control = list(adapt_delta = 0.99)
    ,seed = 123
    ,cores = 4
    ,refresh = 0
    ,stanvars = stanvars
  )

  return(fit)

}
