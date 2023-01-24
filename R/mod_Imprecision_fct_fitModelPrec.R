#' fitModelPrec
#'
#' @param data data.frame input
#' @param x_var character string denoting column containing x values
#' @param y_var character string denoting column containing y values
#' @param qc_level character string/numeric denoting QC level
#' @param col_level character string denoting colour for output
#'
#' @return brms model object
#' @export
#'
#' @examples
fitModelPrec <- function(data
                         ,x_var
                         ,y_var
                         ,qc_level
                         ,col_level) {

  require(brms)

  if (!is.null(col_level)) {
    df_filter <- dplyr::filter(data, !!rlang::sym(col_level) == qc_level)
  } else {
    df_filter <- data
  }

  # Fit model
  form <- as.formula(paste0(y_var, "~ 1 + (1|", x_var, ")"))
  mean_y <- mean(data[[y_var]])
  sd_y <- sd(data[[y_var]])

  stanvars <- brms::stanvar(mean_y, name = "mean_y")+
    brms::stanvar(sd_y, name = "sd_y")

  # Can't use rstanarm due to difficulties with the StanHeaders and rstan versions (2023/01/24)
  # brms requires v2.26.13 of both packages to run, but the application won't deploy due to
  # rstanarm's dependencies. Moved to brms, despite slower performance.

  # fit <- rstanarm::stan_lmer(
  #   formula = form
  #   ,data = df_filter
  #   ,prior_intercept = normal(mean_y, 2.5 * sd_y, autoscale = FALSE)
  #   ,prior_aux = exponential(1 / sd_y, autoscale = FALSE)
  #   ,seed = 1234
  #   ,iter = 4000 # arbitrary number of samples to get stable estimates
  #   ,cores = 4
  #   ,adapt_delta = 0.999 # arbitrarily high to fit most models
  # )

  fit <- brms::brm(
    formula = brms::bf(form)
    ,data = df_filter
    ,prior = c(
      prior(normal(mean_y, 2.5 * sd_y), class = "Intercept")
      ,prior(exponential(1 / sd_y), class = "sd")
      ,prior(exponential(1 / sd_y), class = "sigma")
    )
    ,seed = 1234
    ,iter = 4000
    ,cores = 4
    ,control = list(adapt_delta = 0.999, max_treedepth = 15)
    ,stanvars = stanvars
  )

  return(fit)

}
