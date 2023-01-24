#' plotTheme
#'
#' @param font_size numeric font size
#'
#' @return ggplot2 theme object
#' @export
#'
#' @examples
plotTheme <- function(font_size) {
  return(
    ggplot2:: theme(
      panel.background = ggplot2::element_blank()
      ,panel.grid.minor = ggplot2::element_blank()
      ,panel.grid.major = ggplot2::element_blank()
      ,axis.text = ggplot2::element_text(size = font_size, colour = "black")
      ,axis.title = ggplot2::element_text(size = font_size, colour = "black")
      ,panel.border = ggplot2::element_rect(fill = NA, colour = "black")
      ,strip.background = ggplot2::element_rect(fill = "grey80", colour = "black")
      ,strip.text = ggplot2::element_text(size = font_size, colour = "black")
      ,legend.background = ggplot2::element_rect(fill = NA, colour = "black")
      ,legend.title = ggplot2::element_text(face = "bold", size = font_size, colour = "black")
      ,legend.text = ggplot2::element_text(size = font_size, colour = "black")
    )
  )
}
