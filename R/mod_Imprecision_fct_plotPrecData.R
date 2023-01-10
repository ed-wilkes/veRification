#' plotPrecData
#'
#' @param data data.frame input
#' @param x_var character string denoting column containing x values
#' @param y_var character string denoting column containing y values
#' @param qc_level numeric/character string denoting QC level
#' @param col_level numeric/character string denoting column containing QC levels
#' @param analyte character string denoting analyte name
#' @param plot_dim clientData from session
#'
#' @return plotly object
#' @export
#'
#' @examples
plotPrecData <- function(data, x_var, y_var, qc_level, col_level = NULL, analyte, plot_dim) {

  if (!is.null(col_level) && col_level != "") {
    data <- dplyr::filter(data, !!sym(col_level) == qc_level)
  }

  p <- ggplot2::ggplot(data, aes_string(x = x_var, y = y_var, group = x_var))+
    ggplot2::geom_boxplot()+
    ggplot2::geom_jitter(
      width = 0.1
      ,aes(
        text = paste0(
          "Day: ", !!sym(x_var), "\n"
          ,analyte, ": ", round(!!sym(y_var), 2)
        )
      )
    )+
    ggplot2::stat_summary(
      geom = "point"
      ,fun = mean
      ,colour = "red2"
      ,size = 2
    )+
    ggplot2::theme_bw()+
    ggplot2::xlab("Day")+
    ggplot2::ylab(analyte)+
    plotTheme(font_size = 12)
    # theme(plot.margin = margin(0, 0, 0, l = -3, "cm"))

  p <- ggplotly(p, tooltip = "text", width = plot_dim$output_pid_width, height = plot_dim$output_pid_height)

  return(p)

}
