#' calcCompCoef
#'
#' @param data data.frame input
#' @param method character string denoting analysis method type
#' @param model mcr or brms object
#'
#' @return HTML text summarising model coefficients
#' @export
#'
#' @examples
calcCompCoef <- function(data
                         ,method
                         ,model) {

  if (method != "Bayesian") {

    parameters <- as.data.frame(model@para)
    slope <- format(round(parameters$EST[2], 2), nsmall = 2)
    slope_lwr <- format(round(parameters$LCI[2], 2), nsmall = 2)
    slope_upr <- format(round(parameters$UCI[2], 2), nsmall = 2)
    inter <- format(round(parameters$EST[1], 2), nsmall = 2)
    inter_lwr <- format(round(parameters$LCI[1], 2), nsmall = 2)
    inter_upr <- format(round(parameters$UCI[1], 2), nsmall = 2)

    # Generate HTML
    slope_str <- paste0("<br><b>", method, " slope estimate:</b><br>", slope, " (", slope_lwr, ", ", slope_upr, ")")
    inter_str <- paste0("<br><b>", method, " intercept estimate:</b><br>", inter, " (", inter_lwr, ", ", inter_upr, ")")
    text <- HTML(paste(slope_str, inter_str, sep = "<br/>"))

  } else {

    text <- HTML("")

  }


  return(text)

}
