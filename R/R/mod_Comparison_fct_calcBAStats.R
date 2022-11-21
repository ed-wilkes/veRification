#' calcBAStats
#'
#' @param data data.frame input
#' @param value_x1 character string denoting column containing method 1 values
#' @param value_y1 character string denoting column containing method 2 values
#' @param value_x2 character string denoting column containing method 1 duplicate values
#' @param value_y2 character string denoting column containing method 2 duplicate values
#' @param method character string denoting difference type
#'
#' @return HTML text summarising analysis
#' @export
#'
#' @examples
calcBAStats <- function(data
                        ,value_x1
                        ,value_y1
                        ,value_x2 = NULL
                        ,value_y2 = NULL
                        ,method) {

  # Average duplicates if present
  if (!is.null(value_x2) && value_x2 != "" && !is.null(value_y2) && value_y2 != "") {
    data$mean_x <- apply(data[,c(value_x1, value_x2)], 1, mean)
    data$mean_y <- apply(data[,c(value_y1, value_y2)], 1, mean)
    value_x1 <- "mean_x" # change the name for future calculations
    value_y1 <- "mean_y"
  }

  if (method == "Absolute") {
    data[[paste0(method, " difference")]] <- data[[value_y1]] - data[[value_x1]]
    nb_str <- ""
  } else if (method == "Relative") {
    data[[paste0(method, " difference")]] <- log2(data[[value_y1]]) - log2(data[[value_x1]])
    nb_str <- "<i><br><font size='2'>NB: Relative differences are presented as log<sub>2</sub> fold-ratios of method 2 vs 1.
    A relative difference of 1 would therefore mean that method 2 provides results double that (2<sup>1</sup>) of method 1 and a
    relative difference of -1 would mean that method 2 provides results half that (2<sup>-1</sup>) of method 1.</font></i>"
  }

  # Calculate summary statistics
  mean_diff <- mean(data[[paste0(method, " difference")]], na.rm = TRUE)
  sd_diff <- sd(data[[paste0(method, " difference")]], na.rm = TRUE)
  sem_diff <- sd(data[[paste0(method, " difference")]], na.rm = TRUE) / sqrt(dim(data)[1])
  mean_str <- paste0("<b>Mean (", tolower(method), " difference):</b><br>", format(round(mean_diff, 2), nsmall = 2))
  sd_str <- paste0("<br><b>SD (", tolower(method), " difference):</b><br>", format(round(sd_diff, 2), nsmall = 2))
  sem_str <- paste0("<br><b>SEM (", tolower(method), " difference):</b><br>", format(round(sem_diff, 2), nsmall = 2))

  # Create text
  text <- HTML(paste(mean_str, sd_str, sem_str, nb_str, sep = "<br/>"))
  return(text)

}
