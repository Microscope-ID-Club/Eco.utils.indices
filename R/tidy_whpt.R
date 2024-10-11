#' Tidy WHPT
#'
#' Calculate WHPT index
#'
#' @param x column of taxon names followed by one or more columns of
#'   observations as number of individuals.
#' @param aspt Average Score Per Taxon
#' @param ... Other parameters
#'
#' @return dataframe of values
#' @export tidy_whpt
#'
#'
#'
#'
#'
tidy_whpt <- function(x, aspt = TRUE, ...) {

  # import macroinvertebrate data group mi
  data_bio <- biomonitoR::as_biomonitor(x, group = "mi")
  rows_data_bio <- nrow(data_bio$taxa_db)
  data_agg <- biomonitoR::aggregate_taxa(data_bio)

  sample  <- biomonitoR::whpt(data_agg)

  df.out <- round(dplyr::as_tibble(as.list(sample)),2)

  if(aspt) {

    indicator_names <- dplyr::tibble(Indicator = "WHPT ASPT")
    df.out <- cbind(indicator_names,df.out )

  }else{
    df.out <- round(df.out * rows_data_bio, 2)
    indicator_names <- dplyr::tibble(Indicator = "WHPT ")
    df.out <- cbind(indicator_names,df.out )

  }

  df.out[2:ncol(df.out)] <- lapply(df.out[2:ncol(df.out)], as.numeric)

  return(df.out)

}
