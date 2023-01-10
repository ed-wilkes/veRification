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


  # Mutate the outcome variable to integer
  cases <- as.character(unique(data[[col_label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }

  # Centre data to the median and change outcome to binary integer
  data <- data %>%
    dplyr::mutate(
      outcome = dplyr::if_else(!!sym(col_label) == positive, true = 1, false = 0)
      ,value_centred = !!sym(col_value) - median(!!sym(col_value), na.rm = TRUE)
    )

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
    ,cores = 4
    ,control = list(
      adapt_delta = 0.9999
      ,max_treedepth = 15
    )
  )

  return(fit)

}
