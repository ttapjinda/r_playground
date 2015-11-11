library('jsonlite')
getDataFromAPI <- function(url) {
  data_df <- NULL
  attempt <- 0
  while ( is.null(data_df) && attempt < 3){
    attempt <- attempt + 1
    try(
      data_df <- fromJSON(url)
    )
  }
  return (data_df)
}
RJSONIO.getDataFromAPI <- function(url) {
  data_df <- NULL
  attempt <- 0
  while ( is.null(data_df) && attempt < 3){
    attempt <- attempt + 1
    try(
      data_df <- RJSONIO::fromJSON(url)
    )
  }
  return (data_df)
}