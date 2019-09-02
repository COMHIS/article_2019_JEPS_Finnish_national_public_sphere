
df_factors_to_string <- function(dataframe_to_convert) {
  for (colname in colnames(dataframe_to_convert)) {
    if (is.factor(dataframe_to_convert[, colname])) {
      dataframe_to_convert[, colname] <- as.character(dataframe_to_convert[, colname])
    }  
  }
  return(dataframe_to_convert)
}
