
#' Tidyverse Get All Indicators
#'
#' A tidyvwerse form of calculating all the indicators in one go.
#'
#' @param x The data
#'
#' @return A dataframe of all the indicators
#' @export tidy_indicators
#'
#'
tidy_indicators <- function(x) {

  SampleData <- x

  whpt_df <- SampleData %>% tidy_whpt(aspt = FALSE)
  whpt_df_aspt <- SampleData %>% tidy_whpt(aspt = TRUE)
  bmwp_df <- SampleData %>% tidy_bmwp(aspt = FALSE)
  bmwp_df_aspt <- SampleData %>% tidy_bmwp(aspt = TRUE)
  coleoptera_taxa <- SampleData %>% tidy_coleoptera()
  dragonfly_and_allies <- SampleData %>% tidy_dragonfly_allies()

  indicator_scores <- rbind(whpt_df,whpt_df_aspt,bmwp_df,bmwp_df_aspt, coleoptera_taxa, dragonfly_and_allies)


}
