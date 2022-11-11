#' fitModelPrec
#'
#' @param data data.frame input
#' @param x_var character string denoting column containing x values
#' @param y_var character string denoting column containing y values
#' @param qc_level character string/numeric denoting QC level
#' @param col_level character string denoting colour for output
#'
#' @return rstanarm model object
#' @export
#'
#' @examples
fitModelPrec <- function(data
                         ,x_var
                         ,y_var
                         ,qc_level
                         ,col_level) {

  if (!is.null(col_level)) {
    df_filter <- dplyr::filter(data, .data[[col_level]] == qc_level)
  } else {
    df_filter <- data
  }

  # Fit model
  form <- as.formula(paste0(y_var, "~ 1 + (1|", x_var, ")"))
  mean_y <- mean(data[[y_var]])
  sd_y <- sd(data[[y_var]])

  # Use rstanarm due to greater speed compared to brms
  fit <- rstanarm::stan_lmer(
    formula = form
    ,data = df_filter
    ,prior_intercept = normal(mean_y, 2.5 * sd_y, autoscale = FALSE)
    ,prior_aux = exponential(1 / sd_y, autoscale = FALSE)
    ,seed = 1234
    ,iter = 4000 # arbitrary number of samples to get stable estimates
    ,adapt_delta = 0.999 # arbitrarily high to fit most models
  )

  return(fit)

}
