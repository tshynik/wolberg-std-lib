#' Escape Data
#' 
#' Escape data for use in SQL queries.
#' Returns:
#' * NULL for NA,
#' * single quoted string for strings, factors, & dates,
#' * just the number for numbers.
#' 
#' @name data.escape
#' @keywords sql query data escape
#' @export data.escape
#' 
#' @usage data.escape(x)
#' 
#' @param x Data to escape.
#' 


data.escape <- function(x) {
  if(is.na(x)) return('NULL')
  else if(inherits(x,"character") || inherits(x,"factor") || inherits(x,"Date") || inherits(x,"POSIXt") ) return(sQuote(x,FALSE))
  else return(x)
}


`%notin%` <- Negate(`%in%`)