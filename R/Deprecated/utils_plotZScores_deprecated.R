plotZScores <- function(data
                        ,option_var
                        ,col_mean
                        ,col_n
                        ,col_var
                        ,col_value_1
                        ,col_value_2 = NULL
                        ,plot_height) {

  # Convert CV and SEM to SD
  if (option_var == "CV (%)") {
    data$col_sd <- (data[[col_var]] / 100) * data[[col_var]]
  } else if (option_var == "SEM") {
    data$col_sd <- (data[[col_var]]) * sqrt(data[[col_n]])
  } else {
    data$col_sd <- data[[col_var]]
  }

  # Average duplicates if present
  if (!is.null(col_value_2)) {
    data$mean_sample <- apply(data[,c(col_value_1, col_value_2)], 1, mean)
  } else {
    data$mean_sample <- data[[col_value_1]]
  }

  # Calculate Z-scores using EQA data
  data$z <- (data$mean_sample - data[[col_mean]]) / data$col_sd
  mean_value <- mean(data$z, na.rm = TRUE)
  n_value <- length(data$z)
  sem_value <- sd(data$z, na.rm = TRUE) / sqrt(n_value)
  lwr_value <- mean_value - 1.96 * sem_value
  upr_value <- mean_value + 1.96 * sem_value

  # Generate synthetic normal distribution
  set.seed(123)
  df_synth <- data.frame(x_synth = rnorm(n = 10000, mean = 0, sd = 1))

  # Plot data
  p <- ggplot(data, aes(x = z))+
    geom_density(data = df_synth
                 ,aes(x = x_synth, y = ..scaled..)
                 ,adjust = 2
                 ,alpha = 0.5
                 ,fill = "grey80")+
    geom_density(
      aes(y = ..scaled.. * 0.5)
      ,adjust = 2
      ,alpha = 0.35
      ,fill = "forestgreen"
    )+
    geom_vline(xintercept = 0, colour = "red2", size = 1)+
    geom_vline(xintercept = mean_value, colour = "red2", linetype = "dashed", size = 1)+
    geom_vline(
      xintercept = lwr_value
      ,colour = "mediumorchid4"
      ,linetype = "dashed"
      ,size = 1
    )+
    geom_vline(
      xintercept = upr_value
      ,colour = "mediumorchid4"
      ,linetype = "dashed"
      ,size = 1
    )+
    geom_point(y = 0.1, colour = "cornflowerblue", alpha = 0.75, size = 4)+
    plotTheme(font_size = 14)+
    xlab("Z-score")+
    ylab("Scaled density")
  return(p)

}
