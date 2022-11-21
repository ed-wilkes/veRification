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
      panel.background = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.grid.major = element_blank()
      ,axis.text = element_text(size = font_size, colour = "black")
      ,axis.title = element_text(size = font_size, colour = "black")
      ,panel.border = element_rect(fill = NA, colour = "black")
      ,strip.background = element_rect(fill = "grey80", colour = "black")
      ,strip.text = element_text(size = font_size, colour = "black")
      ,legend.background = element_rect(fill = NA, colour = "black")
      ,legend.title = element_text(face = "bold", size = font_size, colour = "black")
      ,legend.text = element_text(size = font_size, colour = "black")
    )
  )
}
