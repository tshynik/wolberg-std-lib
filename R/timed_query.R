#' Timed database queries
#' 
#' Simple wrapper for running and timing database queries. prints diagnostics; returns the data.
#' 
#' @name timed_query
#' @export timed_query
#' @importFrom glue glue
#' 
#' @usage timed_query(
#'     query, 
#'     cnxn=con, 
#'     params = NA, 
#'     preview = FALSE, 
#'     glue = TRUE, 
#'     env = environment(), 
#'     select = TRUE
#'   )
#' 
#' @param query The query, as a string
#' @param cnxn Connection, defaults to a global variable named \code{con}.
#' @param params Data for parameterized queries; in the form of a list. See examples for details.
#' @param preview Print the first few lines of the results? Default FALSE.
#' @param glue Use \code{glue()} to fill in variables. Default is \code{TRUE}. See \code{glue()} documentation for more details and usage examples.
#' @param env Environment for \code{glue()} to use. Default is the environment that calls \code{timed_query()}.
#' @param select Is it a SELECT query (does user expect data to be returned)? Default is \code{TRUE}. If \code{select=TRUE}, \code{timed_query()} uses \code{DBI::dbGetQuery()}, if \code{select=FALSE}, \code{timed_query()} uses \code{DBI::dbSendQuery()}.
#' 
#' @examples 
#' 
#' # NOT RUN:
#' # timed_query(
#' #   query = "UPDATE schema.table SET first_name = $1, last_name = $3 WHERE id = $3"
#' #   cnxn = con,
#' #   params = list(
#' #     data$first_names,
#' #     data$last_names,
#' #     data$ids,
#' #   )
#' # ) 

utils::globalVariables(c("con"))

timed_query <- function(
  query, 
  cnxn = con, 
  params = NA, 
  preview = FALSE, 
  glue = TRUE, 
  env = environment(), 
  select = TRUE) 
{
  
  ### actually do the query, and time it.
  t <- system.time({
    if (select) {
      if(use_params) {
        x <- DBI::dbGetQuery(conn = cnxn, statement = query, params = params)
      } else {
        x <- DBI::dbGetQuery(conn = cnxn, statement = query)
      }
      x <- data.table::as.data.table(x)
      message("Retrieved ", nrow(x), " existing records")
    }
    else {
      if(use_params) {
        x <- DBI::dbSendQuery(conn = cnxn, statement = query, params = params)
      } else {
        x <- DBI::dbSendQuery(conn = cnxn, statement = query)
      }
      message("Success.")
    }
  })
  print(t)
  if (!select) {
    attributes(x)$sql <- ""
    print(x)
  }
  if (preview) {
    print(utils::head(x))
  }
  return(x)
}


timed_query <- function(
  query, 
  cnxn = con, 
  params = NA,  
  preview = FALSE, 
  glue = TRUE, 
  env = environment(), 
  select = TRUE)
{
  
  ### are we using params?
  if(typeof(params)=='list') use_params = TRUE
  else if(!is.na(params)) stop(
    "Error: Provided params must be formatted as a list. See documentation for help."
    )
  else use_params = FALSE
  
  ### glue! use calling environment by default.
  if(glue) {
    #print( environment() )
    #print( env )
    query <- glue::glue(query, .envir = env)
  }
  
  # if it's hella long, print a truncated version:
  if(nchar(query) > 1000){
    cat( substr(query,1,500),
         "\n ...\n",
         substr(query,nchar(query)-500,nchar(query)),
         "\n" )
  } else {
    cat(paste0(query, "\n"))
  }
  
  ### run it and time it
  t <- system.time( { 
    if (select) {
      if(use_params) {
        x <- DBI::dbGetQuery(conn = cnxn, statement = query, params = params)
      } else {
        x <- DBI::dbGetQuery(conn = cnxn, statement = query)
      }
      x <- data.table::as.data.table(x)
      message("Retrieved ", nrow(x), " existing records")
    } else {
      if(use_params) {
        x <- DBI::dbSendQuery(conn = cnxn, statement = query, params = params)
      } else {
        x <- DBI::dbSendQuery(conn = cnxn, statement = query)
      }
      message("Success.")
    }
  } )
  
  ### print diagnostics
  print(t)
  if(!select) {
    attributes(x)$sql <- "" # this just contains another copy of the SQL query, which is clutter.
    print(x) # prints actual rows changed.
  }
  if(preview & select) { try(print(utils::head(x))) }
  
  ### return results
  return(x)
}