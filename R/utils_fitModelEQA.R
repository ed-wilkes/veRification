#' fitModelEQA
#'
#' @param data data.frame input
#' @param col_value_1 character string denoting column containing measurement values
#' @param col_value_2character string denoting column containing duplicate measurement values
#' @param col_mean character string denoting column EQA mean values
#' @param col_var character string denoting column EQA variance values
#' @param col_n character string denoting column EQA n values
#' @param var_option character string denoting variance type
#'
#' @return brms model object
#' @export
#'
#' @examples
fitModelEQA <- function(data
                        ,col_value_1
                        ,col_value_2 = NULL
                        ,col_mean
                        ,col_var
                        ,col_n
                        ,var_option
                        ,method
                        ,settings = NULL) {

  # Average and calculate SD for duplicates if present
  if (!is.null(col_value_2) && col_value_2 != "") {
    data$mean_sample <- apply(data[,c(col_value_1, col_value_2)], 1, mean)
    data$sd_sample <- apply(data[,c(col_value_1, col_value_2)], 1, sd)
    df$sd_sample[which(df$sd_sample == 0)] <- 1E-09 # replace zeros with arbitrarily small value
  } else {
    data$mean_sample <- data[[col_value_1]]
  }

  # Convert SEM/CV to SD as necessary
  if (var_option == "SEM") {
    data[[col_var]] <- data[[col_var]] * sqrt(data[[col_n]])
  } else if (var_option == "CV (%)") {
    data[[col_var]] <- (data[[col_var]] / 100) * data[[col_mean]]
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
        x = data[[col_mean]]
        ,y = data$mean_sample
        ,method.reg = method
      )
    } else {
      fit <- mcr::mcreg(
        x = data[[col_mean]]
        ,y = data$mean_sample
        ,method.reg = method
        ,error.ratio = settings[[1]]/settings[[2]]
      )
    }

  } else {

    # Load brms to get resp_se() and me() functions
    require(brms)

    sd_y <- sd(data$mean_sample)
    sd_x <- sd(data[[col_mean]])
    mean_x <- mean(data[[col_mean]])
    stanvars <- brms::stanvar(sd_y, name = "sd_y") + brms::stanvar(sd_x, name = "sd_x") + brms::stanvar(mean_x, name = "mean_x")
    data$mean <- data[[col_mean]] # this is lazy, but simplifies the call to me() within brms
    data$sd <- data[[col_var]] # this is lazy, but simplifies the call to me() within brms

    # Fit models
    if (!is.null(col_value_2) && col_value_2 != "") {

      # Full measurement error model
      fit <- brms::brm(
        formula = brms::bf(mean_sample | se(sd_sample, sigma = TRUE) ~ 0 + Intercept + me(mean, sd))
        ,data = data
        ,prior = c(
          prior(normal(0, 2.5 * sd_y), class = "b", coef = "Intercept")
          ,prior(normal(1, 2.5 * (sd_y / sd_x)), class = "b", coef = "memeansd")
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

      # Simple model
      fit <- brms::brm(
        formula = brms::bf(mean_sample ~ 0 + Intercept + mean)
        ,data = data
        ,prior = c(
          prior(normal(0, 2.5 * sd_y), class = "b", coef = "Intercept")
          ,prior(normal(1, 2.5 * (sd_y / sd_x)), class = "b", coef = "mean")
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
