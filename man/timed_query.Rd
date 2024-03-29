% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timed_query.R
\name{timed_query}
\alias{timed_query}
\title{Timed database queries}
\usage{
timed_query(
    query, 
    cnxn=con, 
    params = NA, 
    preview = FALSE, 
    glue = TRUE, 
    env = environment(), 
    select = TRUE
  )
}
\arguments{
\item{query}{The query, as a string}

\item{cnxn}{Connection, defaults to a global variable named \code{con}.}

\item{params}{Data for parameterized queries; in the form of a list. See examples for details.}

\item{preview}{Print the first few lines of the results? Default FALSE.}

\item{glue}{Use \code{glue()} to fill in variables. Default is \code{TRUE}. See \code{glue()} documentation for more details and usage examples.}

\item{env}{Environment for \code{glue()} to use. Default is the environment that calls \code{timed_query()}.}

\item{select}{Is it a SELECT query (does user expect data to be returned)? Default is \code{TRUE}. If \code{select=TRUE}, \code{timed_query()} uses \code{DBI::dbGetQuery()}, if \code{select=FALSE}, \code{timed_query()} uses \code{DBI::dbSendQuery()}.}
}
\description{
Simple wrapper for running and timing database queries. prints diagnostics; returns the data.
}
\examples{

# NOT RUN:
# timed_query(
#   query = "UPDATE schema.table SET first_name = $1, last_name = $3 WHERE id = $3"
#   cnxn = con,
#   params = list(
#     data$first_names,
#     data$last_names,
#     data$ids,
#   )
# ) 
}
