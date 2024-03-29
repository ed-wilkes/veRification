#' calcCompTest
#'
#' @param data data.frame input
#' @param model mcr or brms model object
#' @param value_x1 character string denoting column containing method 1 values
#' @param value_y1 character string denoting column containing method 2 values
#' @param value_x2 character string denoting column containing method 1 duplicate values
#' @param value_y2 character string denoting column containing method 2 duplicate values
#' @param method character string denoting test method
#' @param ci_interval numeric CI interval
#'
#' @return list of shiny valueBoxes
#' @export
#'
#' @examples
calcCompTest <- function(data
                         ,model
                         ,value_x1
                         ,value_y1
                         ,value_x2 = NULL
                         ,value_y2 = NULL
                         ,method
                         ,ci_interval) {

  # Average duplicates if present
  if (!is.null(value_x2) && value_x2 != "") {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean, na.rm = TRUE)
    value_x1 <- "mean_x"
  }

  if (!is.null(value_y2) && value_y2 != "") {
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean, na.rm = TRUE)
    value_y1 <- "mean_y"
  }

  # Perform statistical tests
  if (method != "Bayesian posterior summary") {

    if (method == "Paired t-test") {

      test_obj <- t.test(
        x = data[[value_x1]]
        ,y = data[[value_y1]]
        ,paired = TRUE
        ,conf.level = ci_interval / 100
      )
      centre_x <- mean(data[[value_x1]], na.rm = TRUE)
      centre_y <- mean(data[[value_y1]], na.rm = TRUE)
      centre_value <- "Mean"

    } else if (method == "Paired Wilcoxon (Mann-Whitney) test" ) {

      test_obj <- wilcox.test(
        x = data[[value_x1]]
        ,y = data[[value_y1]]
        ,paired = TRUE
        ,conf.level = ci_interval / 100
      )
      centre_x <- median(data[[value_x1]], na.rm = TRUE)
      centre_y <- median(data[[value_y1]], na.rm = TRUE)
      centre_value <- "Median"

    }

    p_value <- format(round(test_obj$p.value, 3), nsmall = 3)
    stat <- format(round(test_obj$statistic, 1), nsmall = 1)
    colour_tested <- "blue"

    # Generate value boxes
    boxes <- list(
      fluidRow(
        valueBox(
          value = format(round(centre_y, 2), nsmall = 2)
          ,subtitle = paste0(centre_value, " of y values")
          ,color = "blue"
          ,width = 6
        )
        ,valueBox(
          value = format(round(centre_x, 2), nsmall = 2)
          ,subtitle = paste0(centre_value, " of x values")
          ,color = "blue"
          ,width = 6
        )
      )
      ,fluidRow(
        valueBox(
          value = stat
          ,subtitle = paste0(method, " statistic")
          ,color = "blue"
          ,width = 6
        )
        ,valueBox(
          value = p_value
          ,subtitle = paste0(method, " P-value")
          ,color = colour_tested
          ,width = 6
        )
      )
      ,fluidRow(
        column(
          width = 12
          ,tags$i(
            "NB: This P-value represents the probability of obtaining your data, or more extreme values, assuming
          that the null hypothesis (that the mean/median difference between the two methods'
          results is equal to zero) is true."
          )
        )
      )
    )

  } else {

    # Parameter values
    ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
    ci_upr <- 1 - ((1 - (ci_interval / 100)) / 2)
    additive <- paste0(
      format(round(median(as.matrix(model)[,1]), 2), nsmall = 2), " ("
      ,format(round(quantile(as.matrix(model)[,1], ci_lwr), 2), nsmall = 2), ", "
      ,format(round(quantile(as.matrix(model)[,1], ci_upr), 2), nsmall = 2), ")"

    )

    proportional <- paste0(
      format(round(median(as.matrix(model)[,2]),2), nsmall = 2), " ("
      ,format(round(quantile(as.matrix(model)[,2], ci_lwr), 2), nsmall = 2), ", "
      ,format(round(quantile(as.matrix(model)[,2], ci_upr), 2), nsmall = 2), ")"
    )

    width <- 12

    # Basic model checks
    if (any(brms::rhat(model) > 1.1) || any(brms::neff_ratio(model)[1:3] < 0.1)) {
      value_check <- "Fail"
      sub_check <- "Something has gone wrong with the Bayesian model. Interpret results with caution."
      colour_check <- "red"
      icon_check <- "triangle-exclamation"
    } else {
      value_check <- "Pass"
      sub_check <- "Bayesian model checks passed"
      colour_check <- "green"
      icon_check <- "circle-check"
    }

    # Generate value boxes
    boxes <- list(
      p()
      ,fluidRow(
        valueBox(
          value = additive
          ,subtitle = "Additive difference (intercept)"
          ,color = "blue"
          ,width = width
        )
      )
      ,fluidRow(
        column(
          width = 12
          ,tags$i(
            paste0("NB: The numbers within parentheses represent the ", ci_interval,
                   "% credible interval for this parameter based on a scaled, weakly informative prior distribution."
            )
          )
        )
      )
      ,p()
      ,hr()
      ,fluidRow(
        valueBox(
          value = proportional
          ,subtitle = "Proportional difference (slope)"
          ,color = "blue"
          ,width = width
        )
      )
      ,fluidRow(
        column(
          width = 12
          ,tags$i(
            paste0("NB: The numbers within parentheses represent the ", ci_interval,
                   "% credible interval for this parameter based on a scaled, weakly informative prior distribution."
            )
          )
        )
      )
      ,p()
      ,hr()
      ,fluidRow(
        valueBox(
          value = value_check
          ,color = colour_check
          ,subtitle = sub_check
          ,icon = icon(icon_check)
          ,width = width
        )
      )
      ,fluidRow(
        column(
          width = 12
          ,tags$i(
            "NB: The model checks performed here cover the basics and do not
            necessarily account for all the possible errors that might occur."
          )
        )
      )
    )

  }

  return(boxes)

}
