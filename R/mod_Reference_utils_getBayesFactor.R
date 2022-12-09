#' getBayesFactor
#'
#' @param model brms fit from fitModelRef()
#'
#' @return
#' @export
#'
#' @examples
getBayesFactor <- function(model, prior_location) {

  bf_obj <- bayestestR::bayesfactor(model, null = prior_location)
  return(bf_obj)

}
