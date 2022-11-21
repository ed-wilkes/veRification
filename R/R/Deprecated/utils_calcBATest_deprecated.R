calcBATest <- function(data
                       ,value_x1
                       ,value_y1
                       ,value_x2 = NULL
                       ,value_y2 = NULL
                       ,method) {

  # Average duplicates if present
  if (!is.null(value_x2) && !is.null(value_y2)) {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean)
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean)
    value_x1 <- "mean_x"
    value_y1 <- "mean_y"
  }

  if (method == "Absolute") {
    data[[paste0(method, " difference")]] <- data[[value_y1]] - data[[value_x1]]
  } else if (method == "Relative") {
    data[[paste0(method, " difference")]] <- (data[[value_y1]] - data[[value_x1]]) / data[[value_x1]] * 100
  }

  # Perform statistical tests
  test_obj <- t.test(x = data[[paste0(method, " difference")]]
                     ,conf.level = 0.95)

  p_value <- test_obj$p.value
  stat <- test_obj$statistic
  icon_tested <- ifelse(p_value < 0.05, yes = "exclamation-triangle", no = "check-circle")
  colour_tested <- ifelse(p_value < 0.05, yes = "red", no = "green")

  # Generate value boxes
  boxes <- list(
    fluidRow(
      valueBox(
        value = round(stat, 3)
        ,subtitle = paste0("One-sample t statistic")
        ,color = "blue"
        ,width = 6
      )
      ,valueBox(
        value = format(p_value, digits = 3)
        ,subtitle = paste0("One-sample t-test P-value")
        ,color = colour_tested
        ,icon = icon(icon_tested)
        ,width = 6
      )
    )
    ,fluidRow(
      column(
        width = 12
        ,tags$i(
          "NB: This P-value represents the probability of obtaining your results, assuming
          that the null hypothesis (that the mean difference between the two methods'
          results is equal to zero) is true"
        )
      )
    )
  )
  return(boxes)

}
