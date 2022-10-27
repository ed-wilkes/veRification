#' calcCompCor
#'
#' @param data data.frame input
#' @param model mcr or brms model object
#' @param value_x1 character string denoting column containing method 1 values
#' @param value_y1 character string denoting column containing method 2 values
#' @param value_x2 character string denoting column containing method 1 duplicate values
#' @param value_y2 character string denoting column containing method 2 duplicate values
#' @param coef_type character string denoting coefficient type
#'
#' @return HTML text summarising analysis
#' @export
#'
#' @examples
calcCompCor <- function(data
                        ,model
                        ,value_x1
                        ,value_y1
                        ,value_x2 = NULL
                        ,value_y2 = NULL
                        ,coef_type) {

  # Average duplicates if present
  if (!is.null(value_x2) && value_x2 != "") {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean)
    value_x1 <- "mean_x"
  }

  if (!is.null(value_y2) && value_y2 != "") {
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean)
    value_y1 <- "mean_y"
  }

  # Choose coefficient method
  if (coef_type == "Pearson") {

    letter <- "r"
    cor_obj <- cor.test(
      x = data[[value_x1]]
      ,y = data[[value_y1]]
      ,use = "pairwise.complete.obs"
      ,method = "pearson"
    )
    cor_value <- format(round(as.numeric(cor_obj$estimate), 2), nsmall = 2)
    cor_lwr <- format(round(as.numeric(cor_obj$conf.int[1]), 2), nsmall = 2)
    cor_upr <- format(round(as.numeric(cor_obj$conf.int[2]), 2), nsmall = 2)

  } else if (coef_type == "Spearman") {

    letter <- "rho"
    cor_obj <- DescTools::SpearmanRho(
      x = data[[value_x1]]
      ,y = data[[value_y1]]
      ,use = "pairwise.complete.obs"
      ,conf.level = 0.95
    )
    cor_value <- format(round(as.numeric(cor_obj[1]), 2), nsmall = 2)
    cor_lwr <- format(round(as.numeric(cor_obj[2]), 2), nsmall = 2)
    cor_upr <- format(round(as.numeric(cor_obj[3]), 2), nsmall = 2)

  } else if (coef_type == "Kendall") {

    letter <- "tau"
    cor_obj <- DescTools::KendallTauB(
      x = data[[value_x1]]
      ,y = data[[value_y1]]
      ,conf.level = 0.95
    )
    cor_value <- format(round(as.numeric(cor_obj[1]), 2), nsmall = 2)
    cor_lwr <- format(round(as.numeric(cor_obj[2]), 2), nsmall = 2)
    cor_upr <- format(round(as.numeric(cor_obj[3]), 2), nsmall = 2)

  } else if (coef_type == "Bayesian") {

    letter <- "R2"
    cor_obj <- brms::bayes_R2(model)[c(1,3:4)]
    cor_value <- format(round(as.numeric(cor_obj[1]), 2), nsmall = 2)
    cor_lwr <- format(round(as.numeric(cor_obj[2]), 2), nsmall = 2)
    cor_upr <- format(round(as.numeric(cor_obj[3]), 2), nsmall = 2)

  }

  # Create text
  cor_str <- paste0("<b>", coef_type, " ", letter, ":</b><br>", cor_value, " (", cor_lwr, ", ", cor_upr, ")")
  text <- HTML(paste(cor_str, sep = "<br/>"))
  return(text)

}
