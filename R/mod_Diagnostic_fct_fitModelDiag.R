#' fitModelDiag
#'
#' @param data data.frame input
#' @param col_value character string representing column of values
#' @param col_label character string representing column of labels
#' @param positive character string representing a "positive" case
#' @param ci_interval numeric CI interval
#'
#' @return brms model object
#' @export
#'
#' @examples
fitModelDiag <- function(data
                         ,col_value
                         ,col_label
                         ,positive
                         ,ci_interval) {

  # Load brms
  require(brms)

  n_cores <- parallel::detectCores()
  if (n_cores >= 4) {
    n_cores <- 4
  }

  # Mutate the outcome variable to integer
  cases <- as.character(unique(data[[col_label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }

  # Centre data to the median and change outcome to binary integer
  data <- data %>%
    dplyr::mutate(
      outcome = dplyr::if_else(!!rlang::sym(col_label) == positive, true = 1, false = 0)
      ,value_centred = !!rlang::sym(col_value) - median(!!rlang::sym(col_value), na.rm = TRUE)
    ) %>%
    stats::na.omit()

  # Full measurement error model
  fit <- brms::brm(
    formula = outcome ~ 1 + value_centred
    ,data = data
    ,family = bernoulli
    ,prior = c(
      prior(normal(0, 1.5), class = "Intercept")
      ,prior(normal(0, 1.5), class = "b")
    )
    ,seed = 1234 # for reproducibility
    ,iter = 4000
    ,cores = n_cores
    ,refresh = 0
    ,control = list(
      adapt_delta = 0.9999
      ,max_treedepth = 15
    )
  )

  return(fit)

}
