#' Zip3 extraction
#' 
#' @name zipthree
#' @export zipthree
#' @keywords zip code zipthree zip3
#' 
#' Zip codes are messy, and sometimes you need to extract the first 3 digits to impute the US state. Even if it's missing the leading zeroes or has all kinds of weird punctuation.
#' 
#' TODO: Also import the zip3 mapping table and do the match.
#' 
#' @param zip A vector of (ostensible) zip codes.
#' @examples 
#' df <- data.frame(name = letters[1:6], 
#'   zip = c("4024","04011","20010","359-0000","80209-4444","90210"))
#' sapply(df$zip, zipthree) -> df$zip3

### TODO: insert the full zip3 table and just make that part of the output??

zipthree <- function(zip) {
    as.character(zip) -> zip #just in case
    if(is.na(zip)) return(NA) #so it doesn't get stuck
    unlist(strsplit(zip,"-"))[1] -> zip
    if(nchar(zip) == 8) { #if it's a Zip+4 that starts with zero
        return(substr(zip, 1,2))
    }
    else if(nchar(zip) > 5) {
        substr(zip, 1,5) -> zip
    }
    return(substr(zip, 1, nchar(zip)-2))
}