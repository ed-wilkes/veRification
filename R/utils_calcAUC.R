#' calcAUC
#'
#' @param data data.frame input
#' @param type curve type
#' @param value character string representing column of values
#' @param label character string representing column of labels
#' @param positive character string representing a "positive" case
#' @param threshold numeric representing threshold value
#'
#' @return HTML text summarising curve characteristics
#' @export
#'
#' @examples
calcAUC <- function(data, type, value, label, positive, threshold) {

  cases <- as.character(unique(data[[label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }
  negative <- cases[which(cases!= positive)]
  order <- c(negative, positive)
  data[[label]] <- factor(data[[label]], levels = order)
  curves <- precrec::evalmod(
    scores = data[[value]]
    ,labels = data[[label]]
    ,posclass = positive
  )

  # Calculate AUC
  if (type == "ROC") {
    auc <- format(round(attr(curves$rocs[[1]], "auc"), 2), nsmall = 2)
    roc_obj <- pROC::roc(data[[label]], data[[value]], levels = order, ci = TRUE)
    auc_ci_lwr <- format(round(as.numeric(paste(roc_obj$ci))[1], 2), nsmall = 2)
    auc_ci_upr <- format(round(as.numeric(paste(roc_obj$ci))[3], 2), nsmall = 2)
    auc_str <- paste0("<b>AUC</b> = ", auc, " (", auc_ci_lwr, ", ", auc_ci_upr, ")")
  } else if (type == "PR (precision-recall)") {
    auc <- format(round(attr(curves$prcs[[1]], "auc"), 2), nsmall = 2)
    auc_str <- paste0("<b>AUC</b> = ", auc)
  }

  # Get classification predictions given the chosen threshold
  data <- data %>%
    dplyr::mutate(
      pred = dplyr::if_else(
        !!sym(value) > threshold
        ,true = positive
        ,false = negative)
    )
  data$pred <- factor(data$pred, levels = rev(order))
  data[[label]] <- factor(data[[label]], levels = rev(order))
  df_freq <- table(data$pred, data[[label]])
  stats <- epiR::epi.tests(df_freq)

  # Calculate LR+, LR-, sensitivity, specificity, PPV, NPV
  sens <- format(round(stats$detail$est[which(stats$detail$statistic == "se")] * 100, 1), nsmall = 1)
  sens_lwr <- format(round(stats$detail$lower[which(stats$detail$statistic == "se")] * 100, 1), nsmall = 1)
  sens_upr <- format(round(stats$detail$upper[which(stats$detail$statistic == "se")] * 100, 1), nsmall = 1)
  spec <- format(round(stats$detail$est[which(stats$detail$statistic == "sp")] * 100, 1), nsmall = 1)
  spec_lwr <- format(round(stats$detail$lower[which(stats$detail$statistic == "sp")] * 100, 1), nsmall = 1)
  spec_upr <- format(round(stats$detail$upper[which(stats$detail$statistic == "sp")] * 100, 1), nsmall = 1)
  ppv <- format(round(stats$detail$est[which(stats$detail$statistic == "pv.pos")] * 100, 1), nsmall = 1)
  ppv_lwr <- format(round(stats$detail$lower[which(stats$detail$statistic == "pv.pos")] * 100, 1), nsmall = 1)
  ppv_upr <- format(round(stats$detail$upper[which(stats$detail$statistic == "pv.pos")] * 100, 1), nsmall = 1)
  npv <- format(round(stats$detail$est[which(stats$detail$statistic == "pv.neg")] * 100, 1), nsmall = 1)
  npv_lwr <- format(round(stats$detail$lower[which(stats$detail$statistic == "pv.neg")] * 100, 1), nsmall = 1)
  npv_upr <- format(round(stats$detail$upper[which(stats$detail$statistic == "pv.neg")] * 100, 1), nsmall = 1)
  lr_pos <- format(round(stats$detail$est[which(stats$detail$statistic == "lr.pos")], 1), nsmall = 1)
  lr_pos_lwr <- format(round(stats$detail$lower[which(stats$detail$statistic == "lr.pos")], 1), nsmall = 1)
  lr_pos_upr <- format(round(stats$detail$upper[which(stats$detail$statistic == "lr.pos")], 1), nsmall = 1)
  lr_neg <- format(round(stats$detail$est[which(stats$detail$statistic == "lr.neg")], 1), nsmall = 1)
  lr_neg_lwr <- format(round(stats$detail$lower[which(stats$detail$statistic == "lr.neg")], 1), nsmall = 1)
  lr_neg_upr <- format(round(stats$detail$upper[which(stats$detail$statistic == "lr.neg")], 1), nsmall = 1)

  sens_str <- paste0("<br><b>Sensitivity</b> = ", sens, "% (", sens_lwr, "%, ", sens_upr, "%)")
  spec_str <- paste0("<br><b>Specificity</b> = ", spec, "% (", spec_lwr, "%, ", spec_upr, "%)")
  ppv_str <- paste0("<br><b>PPV</b> = ", ppv, "% (", ppv_lwr, "%, ", ppv_upr, "%)")
  npv_str <- paste0("<br><b>NPV</b> = ", npv, "% (", npv_lwr, "%, ", npv_upr, "%)")
  lr_pos_str <- paste0("<br><b>Positive likelihood ratio</b> = ", lr_pos, " (", lr_pos_lwr, ", ", lr_pos_upr, ")")
  lr_neg_str <- paste0("<br><b>Negative likelihood ratio</b> = ", lr_neg, " (", lr_neg_lwr, ", ", lr_neg_upr, ")")

  # Return text
  text <- HTML(
    paste0(
      auc_str
      ,sens_str
      ,spec_str
      ,ppv_str
      ,npv_str
      ,lr_pos_str
      ,lr_neg_str
      ,sep = "<br/>"
    )
  )
  return(text)

}
