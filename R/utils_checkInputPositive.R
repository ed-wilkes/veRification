#' checkInputPositive
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
checkInputPositive <- function(input_env, input_id) {

  if (!is.na(input_env[[input_id]]) && input_env[[input_id]] < 0)  {
    shinyFeedback::showFeedbackWarning(
      inputId = input_id
      ,text = "Value must be a positive number!"
    )
  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
