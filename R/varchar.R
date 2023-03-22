#' Check data coverage and max string length
#' 
#' Find the max length & type & coverage of each column in a data.frame. Handy for when you're creating a new table.
#' 
#' @name varchar
#' @export varchar
#' @keywords varchar database coverage
#' @importFrom magrittr %>%
#' 
#' @usage varchar(x)
#' 
#' @param x Whole data table or data frame.

varchar <- function(x) {
  as.data.frame(x) -> x
  output <- data.table::as.data.table(
    data.frame(
      var = c(),
      nchars = c(),
      type = c(),
      coverage = c()
    )
  )
  for( i in 1:length(names(x)) ){
	  if( typeof(x[,i])=="character" ) { blank = sum(x[,i]=="") } else { blank = 0 }
	  #if( typeof(x[,i])=="logical" ) { trues = sum(x[,i]) } else { blank = 0 }
    
    r <- data.frame(
      var = names(x)[i],
      nchars = x[, i] %>% nchar %>% max(0, na.rm=T),
      type = typeof(x[, i]),
      coverage = round( sum(nrow(x), -sum(is.na(x[,i])), -blank, na.rm=T)/nrow(x)*100 , 1)
      )
    output <- rbind(output, r)
  }
  return(output)
}