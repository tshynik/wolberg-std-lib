#' Database-Safe Names
#' 
#' Make names db safe: no '.' or other illegal characters, all lower case and unique.
#' From: http://www.win-vector.com/blog/2016/02/using-postgresql-in-r/
#' 
#' @name dbsafenames
#' @export dbsafenames
#' 
#' @usage dbsafenames(x)
#' 
#' @param x Whole data table or data frame.

dbsafenames = function(x) {
	names(x) <- sub("[\\(].*", "", names(x))
	names(x) <- gsub('#','num',names(x))
	names(x) <- trimws(names(x))
	names(x) <- gsub('[^a-z0-9]+','_',tolower( names(x) ))
	names(x) <- make.names(names(x), unique=TRUE, allow_=TRUE)
	names(x) <- gsub('.','_',names(x), fixed=TRUE)
	return(x)
}