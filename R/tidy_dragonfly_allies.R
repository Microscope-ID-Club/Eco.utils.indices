
#' Dragonflies and Allies
#'
#' Count of dragonflies, alderflies, mayflies and damselflies.
#'
#' @param x dataframe of data.
#'
#' @return dataframe containing count of dragonflies and allies
#'
#' @export tidy_dragonfly_allies
#'
#'
tidy_dragonfly_allies <- function(x){

  SampleData <- x %>% tidy_observation_data()
  test.names <- SampleData %>% tidy_test_names()
  if(test.names$problem.logic) {
    indicator_scores <- test.names$suggested.taxa

  }else {

  data_bio <- biomonitoR::as_biomonitor(x, group = "mi")

  macro_ex.df <- as.data.frame(data_bio) # Demo convert to df

  target <- c("Megaloptera", "Odonata" )

  dragonfly.df <- macro_ex.df %>% dplyr::filter( .data$Order %in% target)

 # dragonfly.df <- macro_ex.df[dragonfly.rows,]
  # We need to remove the extra columns to do the count correctly

  dragonfly.df.cols <- ncol( dragonfly.df)
  x.cols <- ncol(x) -1
  keep.col.start <- dragonfly.df.cols - x.cols
  keep.col.end <- dragonfly.df.cols
  count.dragonfly.df <- dragonfly.df[,keep.col.start:keep.col.end]

  dragonfly.count <- colSums(count.dragonfly.df != 0)


  count_dragonfly_df <- data.frame(keyName=names(dragonfly.count), value= dragonfly.count, row.names=NULL)

  count_dragonfly_df_1 <- t(count_dragonfly_df)
  count_dragonfly_df_2 <- as.data.frame(count_dragonfly_df_1)
  rownames(count_dragonfly_df_2) <- NULL

  count_dragonfly_df <- count_dragonfly_df_2  %>%
    janitor::row_to_names(row_number = 1)

  colnames(count_dragonfly_df)[1] <- "Indicator"
  count_dragonfly_df[1,1] <- "Dragonfly + Allies"
  rownames(count_dragonfly_df) <- NULL

  # the_names <- colnames(dragonfly.count)
  # the_names[1] <- "Indicator"
  # colnames(dragonfly.count) <- the_names
}
  return(count_dragonfly_df)
}
