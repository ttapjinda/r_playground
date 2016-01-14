factorToNumeric <-function(data){
	if(class(data)=="factor"){
		data<-as.numeric(levels(data))[data]
	}
	else if(class(data)=="character"){
		data<-as.numeric(data)
	}
	return(data)
}

AllColumnToNumeric <- function(my_df) {
  my_col_names <- names(my_df)
  for(n in my_col_names) {
    if(class(my_df[,n])=="factor"){
      my_df[,n]<-as.numeric(levels(my_df[,n]))[my_df[,n]]
    } else {
      my_df[,n] <- as.numeric(my_df[,n])
    }
  }
  return(my_df)
}