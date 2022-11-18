#' plotCurves
#'
#' @param data data.frame
#' @param value character string representing column of values
#' @param label character string representing column of labels
#' @param positive character string representing a "positive" case
#' @param curves_data data.frame input (output from calcCurves())
#' @param threshold numeric threshold from calcCurves()
#' @param type character denoting curve type
#' @param plot_height numeric plot height
#' @param plot_width numeric plot width
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
plotCurves <- function(data, value, label, positive, curves_data, threshold, type, plot_height, plot_width) {

  # Get curve from data alone
  cases <- as.character(unique(data[[label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }
  negative <- cases[which(cases!= positive)]
  order <- c(negative, positive)
  data[[label]] <- factor(data[[label]], levels = order)

  # Get optimal threshold from data
  if (type == "ROC") {

    df_curve <- yardstick::roc_curve(
      data, !!sym(value), truth = data[[label]], event_level = "second"
    ) %>%
      dplyr::rename(x = specificity, y = sensitivity)

  } else {

    df_curve <- yardstick::pr_curve(
      data, !!sym(value), truth = data[[label]], event_level = "second"
    ) %>%
      dplyr::rename(x = recall, y = precision)

  }

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

  # Calculate TN, TP, FN, FP
  true_neg <- df_freq$Freq[which(df_freq$label == "(TN)")]
  true_pos <- df_freq$Freq[which(df_freq$label == "(TP)")]
  false_neg <- df_freq$Freq[which(df_freq$label == "(FN)")]
  false_pos <- df_freq$Freq[which(df_freq$label == "(FP)")]

  if (type == "ROC") {
    point_x <- 1 - (true_neg / (true_neg + false_pos)) # spec
    point_y <- true_pos / (true_pos + false_neg) # sens
    slope <- 1
    intercept <- 0
    y_label <- "Sensitivity"
    x_label <- "1 - Specificity"
    curves_data$x <- 1 - curves_data$x
    df_curve$x <- 1 - df_curve$x
  } else {
    point_x <- true_pos / (true_pos + false_neg) # sens
    point_y <- true_pos / (true_pos + false_pos) # ppv
    slope <- 0
    intercept <- (true_pos + false_neg) / (true_pos + false_neg + true_neg + false_pos)
    y_label <- "Precision (PPV)"
    x_label <- "Recall (sensitivity)"
  }

  # Plot data
  set.seed(123)
  p <- ggplot2::ggplot(dplyr::filter(curves_data, iter %in% sample(iter, size = 200)))+
    ggplot2::geom_abline(
      slope = slope
      ,intercept = intercept
      ,colour = "red2"
      ,linetype = "dashed"
      ,alpha = 0.5
      ,size = 1
    )+
    ggplot2::geom_path(aes(x = x, y = y), alpha = 0.15, size = 0.5)+
    ggplot2::geom_path(data = df_curve, aes(x = x, y = y), alpha = 0.7, col = "dodgerblue3", size = 1)+
    ggplot2::geom_point(aes(x = point_x, y = point_y), size = 5, alpha = 0.7, col = "dodgerblue")+
    ggplot2::xlab(x_label)+
    ggplot2::ylab(y_label)+
    ggplot2::xlim(c(0,1))+
    ggplot2::ylim(c(0,1))+
    plotTheme(font_size = 14)

  return(p)
}
