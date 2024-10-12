
#' Tidy Function To Make Observation Data
#'
#' Requires two named lists to work
#'
#' @param meta.data.list Named list that must contain recordID
#' @param observation.list Named list of families and numbers.
#'
#' @return dataframe of observations
#' @export tidy_observation_maker
#'
#' @examples
#'
#' my.meta.data <- list(recordID = "my.records")
#' my.observations <- list(Dryopidae = 1)
#'
#' tidy_observation_maker(my.meta.data, my.observations)
#'
#'
#'
tidy_observation_maker <- function (meta.data.list, observation.list) {
  observation.meta.data <- meta.data.list
  observation.sample <- observation.list

  # Make the metadata dataframe
  observation.meta.data.df <- dplyr::as_tibble(observation.meta.data)


  SampleData <- data.frame( # datumEntity= recordID ,
    Taxa = names(observation.sample),
    Sample.Name = unlist(observation.sample))
  row.names(SampleData) <- NULL
  names(SampleData)[names(SampleData) == "Sample.Name"] <- observation.meta.data$recordID

  # Substract 1 for the row we already have.
  number.of.taxa <- length(observation.sample) -1

  # Expand metadata
  expand.meta.data.df <- rbind(observation.meta.data.df,
                               observation.meta.data.df[rep(1, number.of.taxa), ])
  data.out.df <- cbind(expand.meta.data.df, SampleData)

  return(data.out.df)

}
