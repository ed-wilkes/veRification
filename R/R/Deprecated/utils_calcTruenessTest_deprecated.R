#' calcTruenessTest
#'
#' @param data data.frame input
#' @param col_value_1 character string denoting column containing measurements
#' @param col_value_2 character string denoting column containing measurements
#' @param col_mean character string denoting column containing mean EQA values
#' @param col_varcharacter character string denoting column containing EQA variance values
#' @param col_n character string denoting column containing EQA n values
#' @param var_option character string denoting variance value type
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
calcTruenessTest <- function(data
                             ,col_value_1
                             ,col_value_2 = NULL
                             ,col_mean
                             ,col_var
                             ,col_n
                             ,var_option) {

  # Average and calculate SD for duplicates if present
  if (!is.null(col_value_2)) {
    data$mean_sample <- apply(data[,c(col_value_1, col_value_2)], 1, mean)
    data$sd_sample <- apply(data[,c(col_value_1, col_value_2)], 1, sd)
  } else {
    data$mean_sample <- data[[col_value_1]]
  }

  # Convert SEM/CV to SD as necessary
  if (var_option == "SEM") {
    data[[col_var]] <- data[[col_var]] * sqrt(data[[col_n]])
  } else if (var_option == "CV (%)") {
    data[[col_var]] <- (data[[col_var]] / 100) * data[[col_mean]]
  }

  # Statistical modelling
  sd_y <- sd(data$mean_sample)
  sd_x <- sd(data[[col_mean]])
  stanvars <- brms::stanvar(sd_y, name = "sd_y") + brms::stanvar(sd_x, name = "sd_x")

  if (!is.null(col_value_2)) {

    fit <- brms::brm(
      formula = brms::bf(mean_sample | se(sd_sample, sigma = TRUE) ~ 0 + Intercept + me(mean, sd))
      ,data = data
      ,prior = c(
        prior(normal(0, 2.5 * sd_y), class = "b", coef = "Intercept")
        ,prior(normal(1, 2.5 * (sd_y / sd_x)), class = "b", coef = "memeansd")
        ,prior(exponential(0.5 / sd_y), class = "sdme")
        ,prior(exponential(1 / sd_y), class = "sigma")
      )
      ,seed = 1234 # for reproducibility
      ,iter = 3000
      # ,refresh = 0 # suppress messages for clarity
      ,control = list(
        adapt_delta = 0.999
        ,max_treedepth = 15
      )
      ,stanvars = stanvars
    )

  } else {

    fit <- brms::brm(
      formula = brms::bf(mean_sample ~ 0 + Intercept + me(mean, sd))
      ,data = data
      ,prior = c(
        prior(normal(0, 2.5 * sd_y), class = "b", coef = "Intercept")
        ,prior(normal(1, 2.5 * (sd_y / sd_x)), class = "b", coef = "memeansd")
        ,prior(exponential(0.5 / sd_y), class = "sdme")
        ,prior(exponential(1 / sd_y), class = "sigma")
      )
      ,iter = 3000
      ,seed = 1234 # for reproducibility
      # ,refresh = 0 # suppress messages for clarity
      ,control = list(
        adapt_delta = 0.999
        ,max_treedepth = 15
      )
      ,stanvars = stanvars
    )

  }

  # Parameter values
  additive <- paste0(
    round(median(as.matrix(fit)[,1]),2), " ("
    ,round(quantile(as.matrix(fit)[,1], 0.025), 2), ", "
    ,round(quantile(as.matrix(fit)[,1], 0.975), 2), ")"
  )

  add_zero <- sum(as.matrix(fit)[,1] > 0) / length(as.matrix(fit)[,1])
  icon_tested_one <- ifelse(
    add_zero > 0.95
    ,yes = "triangle-exclamation"
    ,no = ifelse(
      add_zero < 0.05
      ,yes = "triangle-exclamation"
      ,no = "circle-check"
    )
  )
  colour_tested_one <- ifelse(
    add_zero > 0.95
    ,yes = "red"
    ,no = ifelse(
      add_zero < 0.05
      ,yes = "red"
      ,no = "green"
    )
  )

  proportional <- paste0(
    round(median(as.matrix(fit)[,2]),2), " ("
    ,round(quantile(as.matrix(fit)[,2], 0.025), 2), ", "
    ,round(quantile(as.matrix(fit)[,2], 0.975), 2), ")"
  )

  prop_zero <- sum(as.matrix(fit)[,2] > 1) / length(as.matrix(fit)[,2])
  icon_tested_two <- ifelse(
    prop_zero > 0.95
    ,yes = "triangle-exclamation"
    ,no = ifelse(
      prop_zero < 0.05
      ,yes = "exclamation_triangle"
      ,no = "circle-check"
    )
  )
  colour_tested_two <- ifelse(
    prop_zero > 0.95
    ,yes = "red"
    ,no = ifelse(
      prop_zero < 0.05
      ,yes = "red"
      ,no = "green"
    )
  )
  width <- 8

  # Generate value boxes
  boxes <- list(
    p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = additive
        ,subtitle = "Additive difference"
        ,color = colour_tested_one
        ,icon = icon(icon_tested_one)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 8
        ,tags$i(
          "NB: The numbers within parentheses represent the 95% credible interval for this parameter
          based on a scaled, weakly informative prior distribution."
        )
      )
    )
    ,p()
    ,hr()
    ,fluidRow(
      valueBox(
        value = proportional
        ,subtitle = "Proportional difference"
        ,color = colour_tested_two
        ,icon = icon(icon_tested_two)
        ,width = width
      )
    )
    ,fluidRow(
      column(
        width = 8
        ,tags$i(
          "NB: The numbers within parentheses represent the 95% credible interval for this parameter
          based on a scaled, weakly informative prior distribution."
        )
      )
    )
  )
  return(boxes)

}
