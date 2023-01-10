#' plotDiag
#'
#' @param data data.frame input
#' @param model brms model object
#' @param col_value character string representing column of values
#' @param col_label character string representing column of labels
#' @param positive character string representing a "positive" case
#' @param ci_interval numeric CI interval
#' @param plot_height clientData from session
#'
#' @return plotly object
#' @export
#'
#' @examples
plotDiag <- function(data
                     ,model
                     ,col_value
                     ,col_label
                     ,positive
                     ,ci_interval
                     ,plot_dim) {

  ci_lwr <- (0 + ((1 - (ci_interval / 100)) / 2)) * 100
  ci_upr <- (1 - ((1 - (ci_interval / 100)) / 2)) * 100

  # Change outcome to binary integer
  data <- data %>%
    dplyr::mutate(
      outcome_ = dplyr::if_else(!!sym(col_label) == positive, true = 1, false = 0)
    )

  # Get conditional effects
  cond <- brms::conditional_effects(
    model
    ,prob = ci_interval / 100
    ,robust = TRUE
    ,method = "posterior_epred"
    ,resolution = 200
  )[[1]] %>%
    dplyr::mutate(value_scaled = value_centred + median(data[[col_value]], na.rm = TRUE))

  # Make plot
  p <- ggplot2::ggplot(cond, aes(x = value_scaled, y = estimate__))+
    ggplot2::geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.5, fill = "grey70")+
    ggplot2::geom_line(
      colour = "blue2", linewidth = 1, alpha = 0.75
    )+
    ggplot2::geom_point(
      data = data
      ,aes(x = !!sym(col_value), y = outcome_)
      ,alpha = 0.5,
    )+
    ggplot2::geom_vline(
      xintercept = median(data[[col_value]], na.rm = TRUE)
      ,colour = "red2"
      ,alpha = 0.75
    )+
    plotTheme(font_size = 12)+
    ggplot2::xlab(paste0("Measured values of ", col_value))+
    ggplot2::ylab(paste0("Probability of ", positive))

  gg <- plotly::ggplotly(p, width = plot_dim$output_pid_width, height = plot_dim$output_pid_height)

  # geom_ribbon
  gg$x$data[[1]]$text <- paste0(
    col_value, ": ", round(cond$value_scaled, 1), "\n"
    ,"Lower ", ci_lwr, "%", " probability of ", positive, ": ", round(cond$lower__, 2), "\n"
    ,"Upper ", ci_upr, "%", " probability of ", positive, ": ", round(cond$upper__, 2)
  )

  # geom_line
  gg$x$data[[2]]$text <- paste0(
    col_value, ": ", round(cond$value_scaled, 1), "\n"
    ,"Probability of ", positive, ": ", round(cond$estimate__, 2)
  )

  # geom_point
  gg$x$data[[3]]$text <- paste0(
    col_value, ": ", data[[col_value]], "\n"
    ,col_label, ": ", data[[col_label]]
  )

  # geom_vline
  gg$x$data[[4]]$text <- paste0(
    "Median ", col_value, ": ", median(data[[col_value]], na.rm = TRUE)
  )
  return(gg)
}
