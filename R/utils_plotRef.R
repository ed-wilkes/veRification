plotRef <- function(data
                   ,model
                   ,col_value_1
                   ,col_value_2 = NULL
                   ,prior_mean
                   ,prior_var
                   ,prior_n
                   ,var_option) {

  # Convert SEM/CV to SD as necessary
  if (var_option == "SEM") {
    prior_var <- prior_var * sqrt(prior_n)
  } else if (var_option == "CV (%)") {
    prior_var <- (prior_var / 100) * prior_mean
  }

  # Wrangle data prior to plotting
  if (!is.null(col_value_2) && col_value_2 != "") {
    data <- tidyr::pivot_longer(
      data
      ,cols = c(col_value_1, col_value_2)
      ,values_to = "value"
    ) %>%
      dplyr::mutate(Distribution = "data")
  } else {
    data$value <- data[[col_value_1]]
    data$Distribution = "data"
  }

  # Plot
  bf_obj <-  bayestestR::bayesfactor(
    posterior = as.matrix(model)[,1]
    ,prior = bayestestR::distribution_normal(10000, mean = prior_mean, sd = prior_var)
    ,null = prior_mean
  )
  p <- plot(bf_obj)+
    ggplot2::geom_density(
      data = data
      ,aes(x = value, y = ..density..)
      ,adjust = 2
      ,alpha = 0.25
      ,size = 1
    )+
    plotTheme(12)

  return(p)

}
