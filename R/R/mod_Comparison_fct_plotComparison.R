#' plotComparison
#'
#' @param data data.frame input
#' @param method character string denoting method
#' @param settings list of settings for "Deming" method only
#' @param value_x1 character string denoting column containing method 1 values
#' @param value_x2 character string denoting column containing method 1 duplicate values
#' @param value_y1 character string denoting column containing method 2 values
#' @param value_y2 character string denoting column containing method 2 duplicate values
#' @param x_name character string denoting x-axis label
#' @param y_name character string denoting y-axis label
#' @param model mcr or brms model object
#' @param plot_height numeric plot height
#'
#' @return plotly object
#' @export
#'
#' @examples
plotComparison <- function(data
                           ,method
                           ,settings = NULL
                           ,value_x1
                           ,value_x2 = NULL
                           ,value_y1
                           ,value_y2 = NULL
                           ,x_name
                           ,y_name
                           ,model
                           ,plot_height) {

  # Average duplicates if present
  if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean)
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean)
    data$sd_x <- apply(data[,c(value_x1, value_x2)], 1, sd)
    data$sd_y <- apply(data[,c(value_y1, value_y2)], 1, sd)
    value_x1 <- "mean_x"
    value_y1 <- "mean_y"
    brms_str_1 <- "bsp_me"
    brms_str_2 <- "sd_x"
    coef_str <- paste0(brms_str_1, value_x1, brms_str_2)
  } else {
    brms_str_1 <- "b_value_x1"
    brms_str_2 <- NULL
    coef_str <- paste0(brms_str_1, brms_str_2)
  }

  # Plot data
  min_x <- min(data[[value_x1]], na.rm = TRUE)
  max_x <- max(data[[value_x1]], na.rm = TRUE)

  if (method != "Bayesian") {

    beta_0 <- model@glob.coef[1]
    beta_1 <- model@glob.coef[2]

    p <- ggplot2::ggplot(data, aes(x = !!sym(value_x1), y = !!sym(value_y1)))+
      ggplot2::geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = "dashed", colour = "red2")+
      ggplot2::geom_point(
        alpha = 0.5
        ,aes(
          text = paste0(
          x_name, ": ", round(!!sym(value_x1), 2), "\n"
          ,y_name, ": ", round(!!sym(value_y1), 2)
          )
        )
      )+
      ggplot2::geom_segment(
        x = min_x
        ,xend = max_x
        ,y = beta_0 + beta_1 * min_x
        ,yend = beta_0 + beta_1 * max_x
        ,colour = "dodgerblue2"
      )+
      plotTheme(font_size = 12)+
      ggplot2::xlab(x_name)+
      ggplot2::ylab(y_name)+
      ggplot2::expand_limits(x = 0 , y = 0)

    if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {

      p <- p + ggplot2::geom_errorbar(
        aes(ymin = mean_y - sd_y, ymax = mean_y + sd_y)
        ,width = 0
        ,alpha = 0.5
      )+
        ggplot2::geom_errorbarh(
          aes(y = mean_y, xmin = mean_x - sd_x, xmax = mean_x + sd_x)
          ,width = 0
          ,alpha = 0.5
        )
    }

  } else {

    p <- model %>%
      tidybayes::spread_draws(b_Intercept, !!sym(coef_str), ndraws = 100, seed = 1234) %>%
      dplyr::mutate(
        x = min_x
        ,xend = max_x
        ,y = b_Intercept + !!sym(coef_str) * x
        ,yend = b_Intercept + !!sym(coef_str) * xend
      ) %>%
      ggplot2::ggplot()+
      ggplot2::geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = "dashed", colour = "red2")+
      ggplot2::geom_point(
        data = data
        ,aes(
          x = !!sym(value_x1)
          ,y = !!sym(value_y1)
          ,text = paste0(
            x_name, ": ", round(!!sym(value_x1), 2), "\n"
            ,y_name, ": ", round(!!sym(value_y1), 2)
          )
        )
        ,alpha = 0.5
      )+
      ggplot2::geom_segment(aes(x = x, xend = xend, y = y, yend = yend), alpha = 0.05, colour = "blue2")+
      plotTheme(font_size = 12)+
      ggplot2::xlab(x_name)+
      ggplot2::ylab(y_name)+
      ggplot2::expand_limits(x = 0 , y = 0)

    if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {

      p <- p + ggplot2::geom_errorbar(
        data = data
        ,aes(x = mean_x, ymin = mean_y - sd_y, ymax = mean_y + sd_y)
        ,width = 0
        ,alpha = 0.5
      )+
        ggplot2::geom_errorbarh(
          data = data
          ,aes(y = mean_y, xmin = mean_x - sd_x, xmax = mean_x + sd_x)
          ,height = 0
          ,alpha = 0.5
        )
    }

  }

  return(ggplotly(p, tooltip = "text", height = plot_height * 0.8))

}
