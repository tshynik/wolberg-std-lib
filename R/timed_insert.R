#' Simplified dbWriteTable()
#'
#' Wrapper for \code{DBI::dbWriteTable()} with some typical defaults.
#'
#' @name timed_insert
#' @export timed_insert
#'
#' @usage timed_insert(data, schema = NA, table, append = TRUE, temp = FALSE, cnxn = con)
#'
#' @param data The data, as something coercible to a data.frame. If it's a data.frame already, the variables should be named identically to the table you're inserting into.
#' @param schema The schema to insert to. For temporary tables, use \code{NA}.
#' @param table The table to insert to.
#' @param append Quoting \code{dbWriteTable()} help: "If the append argument is TRUE, the rows in an existing table are preserved, and the new data are appended. If the table doesn't exist yet, it is created." Default \code{TRUE}.
#' @param temp Create temporary table? If so, schema will be set to \code{NA} and temporary table will be named to parameter \code{table}'s value. Warning: calling a temp table the same name as an existing table can cause unexpected results!
#' @param cnxn Connection, defaults to a global variable named \code{con}.

utils::globalVariables(c("con"))

timed_insert <- function(
  data,
  schema = NA,
  table,
  append = TRUE,
  temp = FALSE,
  cnxn = con
){
  if (is.na(schema) | temp == TRUE){
    tablename = DBI::Id(table = table)
    tablename_print = table
  } else {
    tablename = DBI::Id(schema = schema, table = table)
    tablename_print = paste(schema, table, sep=".")
  }
  t <- system.time(
    result <- DBI::dbWriteTable(
      conn = cnxn,
      name = tablename,
      value = data,
      append = append,
      temporary = temp # should it be a temporary table? default F
    )
  )
  print(t)
  if (result) {
    message("Appended ", nrow(data), " rows ",
      "to ", ifelse(temp, "temporary ", ""),
      "table ", tablename_print, ".")
  }
  if (typeof(result) == "S4") {
    # not essential, but prevents a disconcerting warning message.
    # https://nuitrcs.github.io/databases_workshop/r/r_databases.html
    DBI::dbClearResult(result)
  }
}

#' Simplified materialized view refresh.
#'
#' Times materialized view refreshes, and also clears results automatically.
#'
#' @name timed_refresh
#' @export timed_refresh
#'
#' @usage timed_refresh(schema, table, cnxn = con )
#'
#' @param schema The schema of the materialized view to refresh
#' @param table The table of the materialized view to refresh
#' @param cnxn Connection, defaults to a global variable named \code{con}.

timed_refresh <- function(schema, table, cnxn = con) {
  if(is.na(schema) | is.na(table)){
    stop('Please provide both schema and table.')
  }

  result <- timed_query(
    query = paste0("REFRESH MATERIALIZED VIEW ",schema,".",table,";"),
    cnxn=cnxn,
    select=F
    )

  if(typeof(result)=="S4") {
    # not essential, but prevents a disconcerting warning message.
    # https://nuitrcs.github.io/databases_workshop/r/r_databases.html
    DBI::dbClearResult(result)
  }
}