#' checkInputNull
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
checkInputNull <- function(input_env, input_id) {

  if (is.null(input_env[[input_id]]) || is.na(input_env[[input_id]])) {
    shinyFeedback::showFeedbackWarning(
      inputId = input_id
      ,text = "This value must be defined!"
    )
  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
