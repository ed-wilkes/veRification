#' checkInputFile
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
checkInputFile <- function(input_env, input_id) {

  file <- input_env[[input_id]]

  if (!is.null(file)) {

    if (!grepl(".csv|.xlsx|.xls", file$datapath)) {
      shinyFeedback::showFeedbackDanger(
        inputId = input_id
        ,text = "File must be .csv, .xlsx, or .xls format!"
      )
    } else {
      shinyFeedback::hideFeedback(input_id)
    }

  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
