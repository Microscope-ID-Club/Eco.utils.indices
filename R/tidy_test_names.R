#' Title
#'
#' @param x data
#'
#' @return list comprising problem.logic True or False. suggested.taxa as a
#'   dataframe of suggested taxa.
#' @export tidy_test_names
#'
#'
tidy_test_names <- function(x){

  SampleData <- x %>% tidy_observation_data()

  data_bio <- biomonitoR::as_biomonitor(SampleData, group = "mi", traceB = TRUE)

  problem.taxa  <-  "suggested_taxa_names" %in% unlist(attributes(data_bio))

  outlist <- list(
    problem.logic = problem.taxa,
    suggested.taxa = data_bio$suggested_taxa_names
  )

  return(outlist)
}
