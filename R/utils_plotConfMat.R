#' plotConfMat
#'
#' @param data data.frame input
#' @param value character string denoting column containing values
#' @param label character string denoting column containing labels
#' @param positive character string represeting "positive" case
#' @param threshold numeric denoting the threshold output from calcCurves()
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
plotConfMat <- function(data, value, label, positive, threshold) {

  # Cases
  cases <- as.character(unique(data[[label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }
  negative <- cases[which(cases!= positive)]
  order <- c(negative, positive)
  data[[label]] <- factor(data[[label]], levels = order)

  # Get classification predictions given the chosen threshold
  data <- data %>%
    mutate(
      pred = dplyr::if_else(
        !!sym(value) > threshold
        ,true = positive
        ,false = negative
      )
    )
  data$pred <- factor(data$pred, levels = order)
  df_freq <- table(data[[label]], data[["pred"]]) %>% as.data.frame
  df_freq$Var2 <- factor(df_freq$Var2, levels = order)
  df_freq$Var1 <- factor(df_freq$Var1, levels = rev(order))
  df_freq$label <- c("(True negatives)", "(False negatives)", "(False positives)", "(True positives)")

  p <- ggplot2::ggplot(df_freq, aes(x = Var2, y = Var1))+
    ggplot2::geom_tile(aes(fill = Freq))+
    ggplot2::geom_text(aes(label = Freq), vjust = 0, size = 7)+
    ggplot2::geom_text(aes(label = label), vjust = 2, size = 5)+
    ggplot2::scale_fill_gradient(low = "white", high = "red2")+
    plotTheme(font_size = 14)+
    ggplot2::theme(legend.position = "none")+
    ggplot2::xlab("Predicted outcome")+
    ggplot2::ylab("Expected outcome")
  return(p)

}
