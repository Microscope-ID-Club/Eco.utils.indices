#' Tidy BMWP
#'
#' Calculate BMWP index
#'
#' @param x column of taxon names followed by one or more columns of
#'   observations as number of individuals.
#' @param aspt Average Score Per Taxon
#' @param ... Other parameters
#'
#' @return dataframe of values
#' @export tidy_bmwp
#'
#' @examples
#'
#' data(macro_ex)
#'
#'
#'
#'
tidy_bmwp <- function(x, aspt = TRUE,  ...) {

  # import macroinvertebrate data group mi
  data_bio <- biomonitoR::as_biomonitor(x, group = "mi")
  rows_data_bio <- nrow(data_bio$taxa_db)
  data_agg <- biomonitoR::aggregate_taxa(data_bio)

  #sample  <- round(bmwp(data_agg)  / rows_data_bio,2)

  if(aspt) {
    sample  <-  round(biomonitoR::bmwp(data_agg)  / rows_data_bio , 2)
    sample.df <- dplyr::as_tibble(as.list(sample))
    indicator_names <- dplyr::tibble(Indicator = "BMWP ASPT")
    df.out <- cbind(indicator_names,sample.df )

  }else{
    sample  <- biomonitoR::bmwp(data_agg)
    sample.df <- round(dplyr::as_tibble(as.list(sample)),2)
    indicator_names <- dplyr::tibble(Indicator = "BMWP")
    df.out <- cbind(indicator_names,sample.df )
  }

  df.out[2:ncol(df.out)] <- lapply(df.out[2:ncol(df.out)], as.numeric)

  return(df.out)

}
