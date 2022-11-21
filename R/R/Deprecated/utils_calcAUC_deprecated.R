#' calcCurves
#'
#' @param data data.frame data
#' @param type curve type
#' @param value character string representing column of values
#' @param label character string representing column of labels
#' @param positive character string representing a "positive" case
#' @param ci_interval numeric CI interval
#'
#' @return list of:
#' - data.frame containing curves
#' - data.frame containing bootstrap results
#' - HTML text summarising curve characteristics
#'
#' @export
#'
#' @examples
calcCurves <- function(data, type, value, label, positive, ci_interval) {

  cases <- as.character(unique(data[[label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }
  negative <- cases[which(cases!= positive)]
  order <- c(negative, positive)
  data[[label]] <- factor(data[[label]], levels = order)

  # Bootstrap function to determine uncertainty of estimates
  bootstrapFun <- function(i, data, type, value, label, positive) {

    results <- data.frame(
      auc = NA
      ,sens = NA
      ,spec = NA
      ,ppv = NA
      ,npv = NA
      ,lrp = NA
      ,lrn = NA
      ,cutoff = NA
    )

    sample <- dplyr::sample_n(data, size = nrow(data), replace = TRUE)

    if (type == "ROC") {

      curve <- yardstick::roc_curve(
        data = sample, !!sym(value), truth = sample[[label]], event_level = "second"
      ) %>%
        dplyr::mutate(index = sensitivity + specificity - 1, iter = i) %>%
        dplyr::filter(index == max(index))

      results$cutoff <- mean(curve$.threshold) # avoid pairs
      results$auc <- yardstick::roc_auc(
        data = sample, !!sym(value), truth = sample[[label]], event_level = "second"
      )$.estimate

    } else {

      curve <- yardstick::pr_curve(
        data = sample, !!sym(value), truth = sample[[label]], event_level = "second"
      ) %>%
        dplyr::mutate(index = recall + precision - 1, iter = i) %>%
        dplyr::filter(index == max(index))

      results$cutoff <- mean(curve$.threshold) # avoid pairs
      results$auc<- yardstick::pr_auc(
        data = sample, !!sym(value), truth = sample[[label]], event_level = "second"
      )$.estimate

    }

    sample <- dplyr::mutate(
      sample
      ,pred = factor(
        dplyr::if_else(
          !!sym(value) > curve$.threshold, true = positive, false = negative
        )
        ,levels = c(negative, positive)
      )
    )

    stats <- caret::confusionMatrix(
      sample$pred, reference = sample$outcome, mode = "everything", positive = positive
    )

    results$sens <- stats$byClass["Sensitivity"]
    results$spec <- stats$byClass["Sensitivity"]
    results$ppv <- stats$byClass["Pos Pred Value"]
    results$npv <- stats$byClass["Neg Pred Value"]
    results <- dplyr::mutate(results, lrp = sens / (1 - spec), lrn = (1 - sens) / spec)

    return(results)

  }

  # Bootstrap 1000 times
  set.seed(1234)
  df_results <- lapply(1:1000, bootstrapFun, data = data, type = type, value = value, label = label, positive = positive) %>%
    dplyr::bind_rows()

  # Summarise results into medians and given ci_interval
  ci_lwr <- 0 + ((1 - (ci_interval / 100)) / 2)
  ci_upr <- 1 - ((1 - (ci_interval / 100)) / 2)
  getQuantiles <- function(x, digits) {
    format(round(quantile(x, c(lwr, 0.5, upr)), digits), nsmall = digits)
  }

  auc <- quantile(df_results$auc, quantiles)
  sens <- quantile(df_results$sens, quantiles) * 100
  spec <- quantile(df_results$spec, quantiles) * 100
  ppv <- quantile(df_results$ppv, quantiles) * 100
  npv <- quantile(df_results$npv, quantiles) * 100
  lrp <- quantile(df_results$lrp, quantiles)
  lrn <- quantile(df_results$lrn, quantiles)
  cutoff <- quantile(df_results$cutoff, quantiles)

  # Create strings
  auc_str <- paste0("<br><b>AUC</b> = ", auc[2], " (", auc[1], ", ", auc[3], ")")
  sens_str <- paste0("<br><b>Sensitivity</b> = ", sens[2], "% (", sens[1], "%, ", sens[3], "%)")
  spec_str <- paste0("<br><b>Specificity</b> = ", spec[2], "% (", spec[1], "%, ", spec[3], "%)")
  ppv_str <- paste0("<br><b>PPV</b> = ", ppv[2], "% (", ppv[1], "%, ", ppv[3], "%)")
  npv_str <- paste0("<br><b>NPV</b> = ", npv[2], "% (", npv[1], "%, ", npv[3], "%)")
  lrp_str <- paste0("<br><b>Positive likelihood ratio</b> = ", lrp[2], " (", lrp[1], ", ", lrp[3], ")")
  lrn_str <- paste0("<br><b>Negative likelihood ratio</b> = ", lrn[2], " (", lrn[1], ", ", lrn[3], ")")

  # Return text
  text <- HTML(
    paste0(
      auc_str
      ,sens_str
      ,spec_str
      ,ppv_str
      ,npv_str
      ,lrp_str
      ,lrn_str
      ,sep = "<br/>"
    )
  )

  return(list(curve, df_results, text))

}
