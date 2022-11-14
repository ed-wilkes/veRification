#' optimThreshold
#'
#' @param data data.frame input
#' @param type curve type
#' @param value character string representing column of values
#' @param label character string representing column of labels
#' @param positive character string representing a "positive" case
#'
#' @return numeric value representing the optimal threshold
#' @export
#'
#' @examples
optimThreshold <- function(data, type, value, label, positive) {

  cases <- as.character(unique(data[[label]]))
  if (length(cases) > 2) {
    stop("Your data contain > 2 label types! Edit your data and try again.")
  }
  negative <- cases[which(cases!= positive)]
  order <- c(negative, positive)
  data[[label]] <- factor(data[[label]], levels = order)
  curves <- as.data.frame(
    precrec::evalmod(
      scores = data[[value]]
      ,labels = data[[label]]
      ,posclass = positive
    )
  )

  # Calculate AUC
  if (type == "ROC") {

    roc_obj <- pROC::roc(data[[label]], data[[value]], levels = order, ci = TRUE)
    optimal_threshold <- pROC::coords(
      roc_obj
      ,x = "best"
      ,ret = "threshold"
      ,best.method = "youden"
    )[,1][1] # take first value if not single maximum

  } else if (type == "PR (precision-recall)") {

    df_prc <- yardstick::pr_curve(
      data = data
      ,truth = !!sym(label)
      ,!!sym(value)
      ,event_level = "second" # always looking for a "positive"
    ) %>%
      mutate(index = recall + precision - 1) %>%
      filter(index == max(index))

    optimal_threshold <- df_prc$.threshold[1] # take first value if not single maximum

  }

  return(optimal_threshold)

}
