#' checkInputLevels
#'
#' @param input_env shiny input object
#' @param input_id character string for inputId
#' @param input_data reactive shiny object representing data
#'
#' @return shinyFeedback object
#' @export
#'
#' @examples
checkInputLevels <- function(input_env, input_id, input_data) {

  if (!is.null(input_env[[input_id]]) && length(unique(input_data[[input_env[[input_id]]]])) > 2) {
    shinyFeedback::showFeedbackDanger(
      inputId = input_id
      ,text = "Your label has more than 3 levels!"
    )
  } else {
    shinyFeedback::hideFeedback(input_id)
  }

}
