
#' Tidy Observation Data
#'
#' Cleans observation data for use with biomonitoR
#'
#' @param x data frame Must contain column Taxa (not case) with named observation columns to the right
#'
#' @return data frame with Taxa and Observation columns
#' @export tidy_observation_data
#'
#' @examples
#'
#' data(macro_ex, package = "bioindicatoR)
#'
#' tidy_observation_data(macro_ex)
#'
tidy_observation_data <- function(x) {
  names.in <- colnames(x)
  names.clean <- make.names(names.in)
  colnames(x) <- names.clean
  find.taxa <- which(names(x) == "Taxa")
  find.ncol <- ncol(x)
  x.out <- x[,find.taxa:find.ncol]
  return(x.out)
}
