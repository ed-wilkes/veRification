#' fitModelPrec
#'
#' @param data data.frame input
#' @param x_var character string denoting column containing x values
#' @param y_var character string denoting column containing y values
#' @param qc_level character string/numeric denoting QC level
#' @param col_level character string denoting colour for output
#' @param method character string denoting analysis method
#'
#' @return brms model object
#' @export
#'
#' @examples
fitModelPrec <- function(data
                         ,x_var
                         ,y_var
                         ,qc_level
                         ,col_level
                         ,method) {

  require(brms)

  if (!is.null(col_level)) {
    df_filter <- dplyr::filter(data, !!rlang::sym(col_level) == qc_level) %>%
      dplyr::mutate(!!rlang::sym(x_var) := as.factor(!!rlang::sym(x_var))) %>%
      stats::na.omit()
  } else {
    df_filter <- data %>%
      dplyr::mutate(!!rlang::sym(x_var) := as.factor(!!rlang::sym(x_var))) %>%
      stats::na.omit()
  }

  if (method != "Bayesian") {

    form <- as.formula(paste0(y_var, "~", x_var))
    fit <- VCA::remlVCA(form = form, Data = as.data.frame(df_filter), quiet = TRUE)

  } else {

    n_cores <- parallel::detectCores()
    if (n_cores >= 4) {
      n_cores <- 4
    }

    # Fit model
    form <- as.formula(paste0(y_var, "~ 1 + (1|", x_var, ")"))
    mean_y <- mean(df_filter[[y_var]], na.rm = TRUE)
    sd_y <- sd(df_filter[[y_var]], na.rm = TRUE)

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
      ,cores = n_cores
      ,control = list(adapt_delta = 0.999, max_treedepth = 15)
      ,stanvars = stanvars
      ,refresh = 0
    )

  }

  return(fit)

}
