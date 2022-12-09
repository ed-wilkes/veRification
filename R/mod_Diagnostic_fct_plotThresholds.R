#' plotThresholds
#'
#' @param data vector of bootstrapped thresholds (output from calcCurves)
#' @param plot_height numeric plot height
#' @param plot_width numeric plot width
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
plotThresholds <- function(data, plot_height, plot_width) {

  # Heuristic for binwidth
  binwidth <- ceiling((max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / ceiling(sqrt(length(data) / 10)))

  # Make into data.frame
  data <- data.frame(threshold = data)

  # Plot data
  p <- ggplot2::ggplot(data, aes(x = threshold, y = ..density..))+
    ggplot2::geom_histogram(binwidth = binwidth, fill = "white", colour = "black")+
    ggplot2::geom_density(adjust = 2, colour = "dodgerblue3", size = 2)+
    ggplot2::geom_vline(aes(xintercept = median(threshold)), col = "red2", size = 2)+
    ggplot2::xlab("Analyte value threshold")+
    ggplot2::ylab("Density (A.U.)")+
    plotTheme(font_size = 14)

  return(p)
}
