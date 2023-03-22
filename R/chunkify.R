#' Chunkify: Split Lists for Texting Upload
#'
#' @name chunkify
#' @export chunkify
#'
#' @usage chunkify(data, chunk = 80000, filename)
#'
#' @param data Data frame or table
#' @param chunk Final list output size
#' @param filename Filenames for output. Don't need to include today's date or `part#` or `.csv`

chunkify <- function(data, chunk = 80000, filename) {
  for(start in seq(from=1, to=nrow(data), by=chunk) ) {
    end <- min((start+chunk), nrow(data) ) - 1
    data.table::fwrite(
      data[start:end],
      glue::glue("{filename}.{Sys.Date()}-part{ ceiling(start/chunk) }.csv")
    )
    message(
      glue::glue("{filename}.{Sys.Date()}-part{ ceiling(start/chunk) }.csv")
    )
    if(end==nrow(data)-1) {
      message( ceiling(start/chunk) , " done")
      message('last file has ',nrow(data[start:end]),' rows.')
    }
  }
}
