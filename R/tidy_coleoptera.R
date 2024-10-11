#' Count Coleoptera Families
#'
#' Count the number of coleoptera families in the data
#'
#' @param x data
#'
#' @return dataframe of coleoptera
#'
#' @export tidy_coleoptera
#'
#'
tidy_coleoptera <- function(x){

  data_bio <- biomonitoR::as_biomonitor(x, group = "mi")

  macro_ex.df <- as.data.frame(data_bio) # Demo convert to df

  coleoptera.rows <- grepl("Coleoptera",macro_ex.df$Order)

  coleoptera.df <- macro_ex.df[coleoptera.rows,]
  # We need to remove the extra columns to do the count correctly

  coleoptera.df.cols <- ncol( coleoptera.df)
  x.cols <- ncol(x) -1
  keep.col.start <- coleoptera.df.cols - x.cols
  keep.col.end <- coleoptera.df.cols
  count.coleoptera.df <- coleoptera.df[,keep.col.start:keep.col.end]

  coleoptera.count <- colSums(count.coleoptera.df != 0)


  count_coleoptera_df <- data.frame(keyName=names(coleoptera.count), value= coleoptera.count, row.names=NULL)

  count_coleoptera_df_1 <- t(count_coleoptera_df)
  count_coleoptera_df_2 <- as.data.frame(count_coleoptera_df_1)
  rownames(count_coleoptera_df_2) <- NULL

  count_coleoptera_df <- count_coleoptera_df_2  %>%
    janitor::row_to_names(row_number = 1)

  colnames(count_coleoptera_df)[1] <- "Indicator"
  count_coleoptera_df[1,1] <- "Coleoptera Families"
  rownames(count_coleoptera_df) <- NULL

  # the_names <- colnames(coleoptera.count)
  # the_names[1] <- "Indicator"
  # colnames(coleoptera.count) <- the_names

  return(count_coleoptera_df)
}
