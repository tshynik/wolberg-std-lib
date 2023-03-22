# fr: fast file read 2 -----------------------------

#' Fast File Read 2
#'
#' Common settings for \code{fread()}. \code{...} lets you pass additional arguments to \code{data.table::fread()}. Built in support for reading and combining a list of files.
#' 
#' @name fr
#' @keywords file import fread rm_na_cols
#' @export fr
#' 
#' @usage fr(filename, dbsafe=TRUE, progress = FALSE, ...)
#' 
#' @param filename The name/location of the file, as a string. OR, a \code{list()} or \code{c()} of filenames, which will be bound together row-wise with an added column \code{file} that gives the filename it came from. NOTE: does use \code{glue()} on filenames by default!
#' @param dbsafe Clean column names using \code{workingamerica::dbsafenames()}?
#' @param progress Only used when reading a list of files, and used automatically.
#' @param ...	further arguments passed to or from other methods.
#' 

rm_na_cols <- function(dt) {
  dt <- data.table::as.data.table(dt)
  # from here: https://stackoverflow.com/questions/2643939/
  dt <- dt[,which(unlist(lapply(dt, function(x)!all(is.na(x))))),with=F]
  return(dt)
}

fr <- function(filename, dbsafe = TRUE, progress = FALSE, ...) {
  if (length(filename) == 1) {
    filename <- glue::glue(filename)
    x <- data.table::fread(filename, integer64 = "character", ...)
    if (dbsafe)
      x <- dbsafenames(x)
    if (progress)
      message(filename," - loaded!")
    return(x)
  }
  else {
    # ensure it's a list:
    files <- as.list(filename)
    
    # # do all files exist?
    # for(file in files){
    #   if(!file.exists(file)) {
    #     warning("File ",file," doesn't exist. Check the filenames again.")
    #   }
    # }
    
    # read files; return a list of data.tables
    l <- lapply(files, fr, progress = TRUE, ...)
    
    # set the names of the list to be the filenames
    names(l) <- files
    
    # drop columns that only have NAs in them. fread() interprets them as logical,
    # and then rbindlist() freaks out when it tries to combine a "logical" (empty)
    # column with a column with data in it.
    l <- lapply(X=l, FUN=rm_na_cols)
    
    # try rbindlist, but still return the results if it doesn't work!
    x <- try(
      data.table::rbindlist(l, idcol = "file", use.names=TRUE, fill=TRUE),
      silent = TRUE
    )
    if(!inherits(x, "try-error")) return(x)
    else {
      warning("rbindlist() failed; returning a list of data.tables. Check column types and try again.")
      return(l)
    }
  }
}


# clntxt: clean text for grepl -----------------------------

#' Clean text for easier grep
#'
#' Convert to lower, remove punctuation and special characters, trim whitespace, etc. 
#' 
#' TODO: figure out how to remove emoji/unicode
#'
#' @keywords file parse grep grepl
#' @export clntxt
#' 
#' @usage clntxt(text)
#' 
#' @param text String to clean.
#' 

clntxt <- function(text) {
  text <- trimws(text)
  text <- tolower(text)
  text <- gsub(x=text, pattern="\n", replacement="")
  text <- gsub(x=text, pattern="!", replacement="", fixed=T)
  text <- gsub(x=text, pattern="?", replacement="", fixed=T)
  text <- gsub(x=text, pattern=".", replacement="", fixed=T)
  text <- gsub(x=text, pattern=",", replacement="", fixed=T)
  text <- gsub(x=text, pattern="'", replacement="", fixed=T)
  return(text)
}