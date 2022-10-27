#' plotAUC
#'
#' @param data data.frame input
#' @param type character string denoting curve type
#' @param value character string denoting column containing values
#' @param label character string denoting column containing labels
#' @param positive character string denoting "positive" case
#' @param plot_height numeric plot height
#' @param plot_width numeric plot width
#' @param threshold numeric curve threshold
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
plotAUC <- function(data, type, value, label, positive, plot_height, plot_width, threshold) {

  # Check curve type
  if (type == "ROC") {
    y_label <- "Sensitivity"
    x_label <- "1 - Specificity"
  } else if (type == "PR (precision-recall)") {
    y_label <- "Precision (PPV)"
    x_label <- "Recall (sensitivity)"
    type <- "PRC"
  }

  # Cases
  cases <- as.character(unique(data[[label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }
  negative <- cases[which(cases!= positive)]
  order <- c(negative, positive)
  data[[label]] <- factor(data[[label]], levels = order)

  # Make curve object
  curves <- precrec::evalmod(
    scores = data[[value]]
    ,labels = data[[label]]
    ,posclass = positive
  ) %>%
    as.data.frame

  # Get classification predictions given the chosen threshold
  data <- data %>%
    dplyr::mutate(
      pred = dplyr::if_else(
        !!sym(value) > threshold
        ,true = positive
        ,false = negative)
    )
  data$pred <- factor(data$pred, levels = order)
  df_freq <- table(data[[label]], data[["pred"]]) %>% as.data.frame
  df_freq$label <- c("(TN)", "(FN)", "(FP)", "(TP)")

  # Calculate LR+, LR-, sensitivity, specificity, PPV, NPV
  true_neg <- df_freq$Freq[which(df_freq$label == "(TN)")]
  true_pos <- df_freq$Freq[which(df_freq$label == "(TP)")]
  false_neg <- df_freq$Freq[which(df_freq$label == "(FN)")]
  false_pos <- df_freq$Freq[which(df_freq$label == "(FP)")]

  if (type == "ROC") {
    point_x <- 1 - (true_neg / (true_neg + false_pos)) #spec
    point_y <- true_pos / (true_pos + false_neg) #sens
    slope <- 1
    intercept <- 0
  } else {
    point_x <- true_pos / (true_pos + false_neg) #sens
    point_y <- true_pos / (true_pos + false_pos) #ppv
    slope <- 0
    intercept <- (true_pos + false_neg) / (true_pos + false_neg + true_neg + false_pos)
  }

  # Plot data
  p <- ggplot2::ggplot(dplyr::filter(curves, type == !!type), aes(x = x, y = y))+
    ggplot2::geom_abline(
      slope = slope
      ,intercept = intercept
      ,colour = "red2"
      ,linetype = "dashed"
      ,alpha = 0.5
      ,size = 1
    )+
    ggplot2::geom_line(alpha = 0.5, size = 1)+
    ggplot2::geom_point(x = point_x, y = point_y, colour = "red2", size = 5)+
    ggplot2::xlab(x_label)+
    ggplot2::ylab(y_label)+
    ggplot2::xlim(c(0,1))+
    ggplot2::ylim(c(0,1))+
    plotTheme(font_size = 14)

  return(p)
}
