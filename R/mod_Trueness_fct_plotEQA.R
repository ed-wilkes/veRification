#' plotEQA
#'
#' @param data data.frame input
#' @param option_var character string denoting variance type
#' @param col_mean character string denoting column containing mean EQA values
#' @param col_var character string denoting column containing EQA variance values
#' @param col_n character string denoting column containing EQA n values
#' @param col_value_1 character string denoting column containing measurement values
#' @param col_value_2 character string denoting column containing duplicate measurement values
#' @param model brms model object
#' @param plot_height numeric plot height
#'
#' @return plotly object
#' @export
#'
#' @examples
plotEQA <- function(data
                   ,option_var
                   ,col_mean
                   ,col_var
                   ,col_n
                   ,col_value_1
                   ,col_value_2 = NULL
                   ,model
                   ,method
                   ,plot_height) {

  # Average and calculate SD for duplicates if present
  if (!is.null(col_value_2) && col_value_2 != "") {
    data$mean_sample <- apply(data[,c(col_value_1, col_value_2)], 1, mean)
    data$sd_sample <- apply(data[,c(col_value_1, col_value_2)], 1, sd)
  } else {
    data$mean_sample <- data[[col_value_1]]
  }

  # Convert SEM/CV to SD as necessary
  if (option_var == "SEM") {
    data[[col_var]] <- data[[col_var]] * sqrt(data[[col_n]])
  } else if (option_var == "CV (%)") {
    data[[col_var]] <- (data[[col_var]] / 100) * data[[col_mean]]
  }

  # Plot data
  min_x <- min(data[[col_mean]], na.rm = TRUE)
  max_x <- max(data[[col_mean]], na.rm = TRUE)

  if (method != "Bayesian") {

    beta_0 <- model@glob.coef[1]
    beta_1 <- model@glob.coef[2]

    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(col_mean), y = mean_sample))+
      ggplot2::geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = "dashed", colour = "red2")+
      ggplot2::geom_errorbarh(
        ggplot2::aes(
          y = mean_sample
          ,xmin = !!rlang::sym(col_mean) - !!rlang::sym(col_var)
          ,xmax = !!rlang::sym(col_mean) + !!rlang::sym(col_var)
        )
        ,height = 0
        ,alpha = 0.5
      )+
      ggplot2::geom_point(
        alpha = 0.5
        ,ggplot2::aes(
          text = paste0(
            "EQA value: ", round(mean, 2), "\n"
            ,"Measured value: ", round(mean_sample, 2)
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
      ggplot2::xlab("EQA results")+
      ggplot2::ylab("Measured values")+
      ggplot2::expand_limits(x = 0 , y = 0)

    if (!is.null(col_value_2) && col_value_2 != "") {

      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(ymin = mean_sample - sd_sample, ymax = mean_sample + sd_sample)
        ,width = 0
        ,alpha = 0.5
      )

    }

  } else {

    if (!is.null(col_value_2) && col_value_2 != "") {
      coef_str <- "bsp_memeansd"
    } else {
      coef_str <- "b_mean"
    }

    p <- model %>%
      tidybayes::spread_draws(b_Intercept, !!rlang::sym(coef_str), ndraws = 100, seed = 1234) %>%
      dplyr::mutate(
        x = min_x
        ,xend = max_x
        ,y = b_Intercept + !!rlang::sym(coef_str) * x
        ,yend = b_Intercept + !!rlang::sym(coef_str) * xend
      ) %>%
      ggplot2::ggplot()+
      ggplot2::geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = "dashed", colour = "red2")+
      ggplot2::geom_errorbarh(
        data = data
        ,ggplot2::aes(
          y = mean_sample
          ,xmin = !!rlang::sym(col_mean) - !!rlang::sym(col_var)
          ,xmax = !!rlang::sym(col_mean) + !!rlang::sym(col_var)
        )
        ,height = 0
        ,alpha = 0.5
      )+
      ggplot2::geom_point(
        data = data
        ,ggplot2::aes(
          x = mean
          ,y = mean_sample
          ,text = paste0(
            "EQA value: ", round(mean, 2), "\n"
            ,"Measured value: ", round(mean_sample, 2)
          )
        )
        ,alpha = 0.5
      )+
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend), alpha = 0.05, colour = "blue2")+
      plotTheme(font_size = 12)+
      ggplot2::xlab("EQA results")+
      ggplot2::ylab("Measured values")+
      ggplot2::expand_limits(x = 0 , y = 0)


    if (!is.null(col_value_2) && col_value_2 != "") {

      p <- p + ggplot2::geom_errorbar(
        data = data
        ,ggplot2::aes(x = mean, ymin = mean_sample - sd_sample, ymax = mean_sample + sd_sample)
        ,width = 0
        ,alpha = 0.5
      )

    }

  }

  return(plotly::ggplotly(p, tooltip = "text", height = plot_height * 0.8))
}
