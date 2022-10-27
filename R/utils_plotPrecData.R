#' plotPrecData
#'
#' @param data data.frame input
#' @param x_var character string denoting column containing x values
#' @param y_var character string denoting column containing y values
#' @param qc_level numeric/character string denoting QC level
#' @param col_level numeric/character string denoting colour
#' @param analyte character string denoting analyte name
#'
#' @return plotly object
#' @export
#'
#' @examples
plotPrecData <- function(data, x_var, y_var, qc_level, col_level = NULL, analyte) {

  if (!is.null(col_level) && col_level != "") {
    data <- dplyr::filter(data, !!sym(col_level) == qc_level)
  }

  p <- ggplot(data, aes_string(x = x_var, y = y_var, group = x_var))+
    geom_boxplot()+
    geom_jitter(
      width = 0.1
      ,aes(
        text = paste0(
          "Day: ", !!sym(x_var), "\n"
          ,"Value: ", round(!!sym(y_var), 2)
        )
      )
    )+
    stat_summary(
      geom = "point"
      ,fun.y = mean
      ,colour = "red2"
      ,size = 2
    )+
    theme_bw()+
    xlab("Day")+
    ylab(analyte)+
    plotTheme(font_size = 12)
  return(ggplotly(p, tooltip = "text"))

}
