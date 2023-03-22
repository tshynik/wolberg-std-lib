#' Get names of columns in a db table
#' 
#' @name getcolnames
#' @export getcolnames
#' @keywords postgres table column names query
#' 
#' @param cnxn Connection, default con
#' @param table_schema Schema
#' @param table_name Table
#' 
#' @usage getcolnames(table_schema, table_name, cnxn=con)

getcolnames <- function(table_schema, table_name, cnxn=con) {
  q = paste0("SELECT column_name FROM information_schema.columns ",
             "WHERE table_schema = '",table_schema,
             "' AND table_name   = '",table_name,"';")
  return( DBI::dbGetQuery(cnxn, q)$column_name )
}