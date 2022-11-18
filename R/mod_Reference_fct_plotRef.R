#' plotRef
#'
#' @param data data.frame containing input_file data
#' @param model brms fit from fitModelRef()
#' @param col_value_1 character string containing column name
#' @param col_value_2 character string containing column name
#' @param bayes_factor bayesfactor object returned from getBayesFactor()
#'
#' @return
#' @export
#'
#' @examples
plotRef <- function(data
                   ,model
                   ,col_value_1
                   ,col_value_2 = NULL
                   ,bayes_factor) {

  # Wrangle data prior to plotting
  if (!is.null(col_value_2) && col_value_2 != "") {
    data <- tidyr::pivot_longer(
      data
      ,cols = c(any_of(col_value_1), any_of(col_value_2))
      ,values_to = "value"
    ) %>%
      dplyr::mutate(Distribution = "data")
  } else {
    data$value <- data[[col_value_1]]
    data$Distribution = "data"
  }

  # Plot
  p <- plot(bayes_factor)+
    ggplot2::geom_density(
      data = data
      ,aes(x = value, y = ..density..)
      ,adjust = 2
      ,alpha = 0.25
      ,size = 1
    )+
    plotTheme(12)

  return(p)

}
