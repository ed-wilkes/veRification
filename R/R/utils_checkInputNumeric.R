#' checkInputNumeric
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#' @param input_data reactive shiny object representing data
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
checkInputNumeric <- function(input_env, input_id, input_data) {

  if (!is.null(input_env[[input_id]]) && !is.numeric(input_data[[input_env[[input_id]]]])) {
    shinyFeedback::showFeedbackWarning(
      inputId = input_id
      ,text = "Data must be numeric!"
    )
  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
