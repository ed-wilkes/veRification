#' yAxis
#'
#' @param title character string denoting axis title
#' @param ...
#'
#' @return list
#' @export
#'
#' @examples
yAxis <- function(title, ...) {
  list(
    title = title
    ,showline = TRUE
    ,zeroline = FALSE
    ,gridline = FALSE
    ,mirror = "ticks"
    ,linecolor = plotly::toRGB("black")
    ,...
  )
}
