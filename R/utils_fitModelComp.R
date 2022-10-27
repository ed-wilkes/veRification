#' fitModelComp
#'
#' @param data data.frame input
#' @param method character string denoting modelling method
#' @param settings list of settings for "Deming" method only
#' @param value_x1 character string denoting column containing method 1 values
#' @param value_x2 character string denoting column containing method 1 duplicate values
#' @param value_y1 character string denoting column containing method 2 values
#' @param value_y2 character string denoting column containing method 2 duplicate values
#'
#' @return mcr or brms model object
#' @export
#'
#' @examples
fitModelComp <- function(data
                         ,method
                         ,settings = NULL
                         ,value_x1
                         ,value_x2 = NULL
                         ,value_y1
                         ,value_y2 = NULL) {

  if (method == "Bayesian") {
    # Load brms to get resp_se() and me() functions
    require(brms)
  }

  # Average duplicates if present
  if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean)
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean)
    data$sd_x <- apply(data[,c(value_x1, value_x2)], 1 , sd)
    data$sd_y <- apply(data[,c(value_y1, value_y2)], 1, sd)
    value_x1 <- "mean_x"
    value_y1 <- "mean_y"
  }

  # Choose regression method
  if (method == "Passing-Bablok") {
    method <- "PaBa"
  } else if (method == "Deming") {
    method <- "Deming"
  } else if (method == "Ordinary least-squares") {
    method <- "LinReg"
  }

  # Regression model
  if (method != "Bayesian") {

    if (method != "Deming") {
      fit <- mcr::mcreg(
        x = data[[value_x1]]
        ,y = data[[value_y1]]
        ,method.reg = method
      )
    } else {
      fit <- mcr::mcreg(
        x = data[[value_x1]]
        ,y = data[[value_y1]]
        ,method.reg = method
        ,error.ratio = settings[[1]]/settings[[2]]
      )
    }

  } else {

    # Set priors
    sd_y <- sd(data[[value_y1]])
    sd_x <- sd(data[[value_x1]])
    stanvars <- brms::stanvar(sd_y, name = "sd_y") + brms::stanvar(sd_x, name = "sd_x")

    if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {

      mean_x <- mean(data[[value_x1]])
      stanvars <- stanvars + brms::stanvar(mean_x, name = "mean_x")

      # Full measurement error model
      fit <- brms::brm(
        formula = brms::bf(mean_y | se(sd_y, sigma = TRUE) ~ 0 + Intercept + me(mean_x, sd_x))
        ,data = data
        ,prior = c(
          prior(normal(0, 2.5 * sd_y), class = "b", coef = "Intercept")
          ,prior(normal(1, 2.5 * (sd_y / sd_x)), class = "b", coef = "memean_xsd_x")
          ,prior(normal(mean_x, 2.5 * sd_x), class = "meanme")
          ,prior(exponential(0.5 / sd_x), class = "sdme")
          ,prior(exponential(1 / sd_y), class = "sigma")
        )
        ,seed = 1234 # for reproducibility
        ,iter = 4000
        # ,refresh = 0 # suppress messages for clarity
        ,control = list(
          adapt_delta = 0.9999
          ,max_treedepth = 15
        )
        ,stanvars = stanvars
      )

    } else {

      data$value_x1 <- data[[value_x1]] # create new variable for brms/stan to work
      form <- as.formula(paste0(value_y1, " ~ 0 + Intercept + value_x1"))

      # Simple model
      fit <- brms::brm(
        formula = brms::bf(form)
        ,data = data
        ,prior = c(
          prior(normal(0, 2.5 * sd_y), class = "b", coef = "Intercept")
          ,prior(normal(1, 2.5 * (sd_y / sd_x)), class = "b", coef = "value_x1")
          ,prior(exponential(1 / sd_y), class = "sigma")
        )
        ,iter = 4000
        ,seed = 1234 # for reproducibility
        # ,refresh = 0 # suppress messages for clarity
        ,control = list(
          adapt_delta = 0.9999
          ,max_treedepth = 15
        )
        ,stanvars = stanvars
      )

    }
  }

  return(fit)

}
