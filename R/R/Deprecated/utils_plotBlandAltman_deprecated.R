#' plotBlandAltman
#'
#' @param data data.frame input
#' @param method character string denoting method
#' @param value_x1 character string denoting column containing method 1 values
#' @param value_x2 character string denoting column containing method 1 duplicate values
#' @param value_y1 character string denoting column containing method 2 values
#' @param value_y2 character string denoting column containing method 2 duplicate values
#' @param x_name character string denoting x-axis label
#' @param y_name character string denoting y-axis label
#' @param plot_height numeric plot height
#'
#' @return plotly object
#' @export
#'
#' @examples
plotBlandAltman <- function(data
                            ,method
                            ,value_x1
                            ,value_x2 = NULL
                            ,value_y1
                            ,value_y2 = NULL
                            ,x_name
                            ,y_name
                            ,plot_height) {

  # Average duplicates if present
  if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean)
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean)
    value_x1 <- "mean_x" # change the name for future calculations
    value_y1 <- "mean_y"
  }

  if (method == "Absolute") {
    data$difference <- data[[value_y1]] - data[[value_x1]]
  } else if (method == "Relative") {
    data$difference <- (data[[value_y1]] - data[[value_x1]]) / data[[value_x1]] * 100
  }

  mean_diff <- mean(data$difference, na.rm = TRUE)
  sd_diff <- sd(data$difference, na.rm = TRUE)
  limit_upr <- mean_diff + 1.96 * sd_diff
  limit_lwr <- mean_diff - 1.96 * sd_diff

  # Plot data
  p <- ggplot(data, aes(x = !!sym(value_x1), y = difference))+
    geom_hline(yintercept = 0, alpha = 0.5, colour = "red2")+
    geom_hline(yintercept = mean_diff, alpha = 0.5, linetype = "dashed", colour = "mediumorchid4")+
    geom_hline(yintercept = limit_upr, alpha = 0.5, linetype = "dashed", colour = "forestgreen")+
    geom_hline(yintercept = limit_lwr, alpha = 0.5, linetype = "dashed", colour = "forestgreen")+
    geom_point(
      alpha = 0.5
      ,aes(
        text = paste0(
          "Mean value: ", round(!!sym(value_x1), 2), "\n"
          ,method, " difference: ", round(difference, 2)
        )
      )
    )+
    plotTheme(font_size = 12)+
    xlab(x_name)+
    ylab(paste0(method, " difference (", x_name, " vs ", y_name,")"))+
    expand_limits(x = 0)

  return(ggplotly(p, tooltip = "text", height = plot_height * 0.6))

}
