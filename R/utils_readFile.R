#' readFile
#'
#' @param file character string fileInput
#' @param headings boolean denoting whether to use headings
#'
#' @return data.frame
#' @export
#'
#' @examples
readFile <- function(file, headings){

  inFile <- file
  if (is.null(inFile)) return(NULL)

  # Check file type
  if (grepl(".csv", inFile$datapath)) {
    df <- read.csv(inFile$datapath, header = headings)
  } else if (grepl(".xls", inFile$datapath)) {
    df <- readxl::read_excel(inFile$datapath, sheet = 1, col_names = headings)
  }
  # else {
  #   stop("Please upload your data in .csv/.xlsx/.xls format.")
  # }
}
