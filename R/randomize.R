#' Automatic Householding and Randomization
#' 
#' 
#' @name randomize
#' @export randomize
#' 
#' @usage
#' randomize(data, household = T, maxhhsize = 5, conditions = c("Control", "Treatment"), 
#'   weight = rep(1/length(conditions), length(conditions)), verbose=FALSE)
#'   
#' @param data Data frame or table
#' @param household True or False
#' @param maxhhsize Max household size (if household=TRUE); default 5. Larger "households" are excluded from the final randomized file on the suspicion of being uncanvassable apartment buildings.
#' @param conditions Character vector of condition names. Default is c("Control", "Treatment")
#' @param weight Numeric vector of the same length as the conditions vector, with weights assigned to each condition. Weights must add to 1. Must be in same order as conditions! Default is equal weights.
#' @param verbose Default is FALSE, so it displays all diagnostics.

randomize_cond <- function(n, weight, conditions, verbose=FALSE){
    # find conditions size (equal by default):
    cond_size <- round(n*weight)
    cond_labels = c()
    message("Conditions:")
    for(i in 1:length(conditions)) {
      message("\t",conditions[i],"\t", cond_size[i])
      cond_labels <- c(cond_labels, rep(conditions[i], cond_size[i]) )
    }
    if(verbose) {
      # data.table::data.table(cond_labels = cond_labels) %>%
      #   dplyr::group_by(cond_labels) %>%
      #   dplyr::tally
      ## rewrite without pipes:
      temp <- data.table::data.table(cond_labels = cond_labels)
      temp <- dplyr::group_by(temp, .data$cond_labels)
      temp <- dplyr::tally(temp)
      print( temp )
      rm(temp)
    }
    
    #if rounding gave the wrong # of labels, just keep adding one -- sampling maintains the original proportions.
    while(length(cond_labels)<n){
      cond_labels <- c(cond_labels, sample(cond_labels, size=1, replace=T) )
    }
    if(verbose) {
      # data.table::data.table(cond_labels = cond_labels) %>%
      #   dplyr::group_by(cond_labels) %>%
      #   dplyr::tally
      ## rewrite without pipes:
      temp <- data.table::data.table(cond_labels = cond_labels)
      temp <- dplyr::group_by(temp, .data$cond_labels)
      temp <- dplyr::tally(temp)
      print( temp )
      rm(temp)
    }
    return(cond_labels)
}


randomize <- function(
  data, 
  household = T,
  maxhhsize = 5, 
  conditions = c("Control", "Treatment"),
  weight = rep(1/length(conditions), length(conditions)),
  verbose = FALSE
) {  
  if(length(weight) != length(conditions)) stop("Check that number of weights is the same as number of conditions.")
  message("Weights: ")
  names(weight) <- conditions
  print(weight)
  if(sum(weight) != 1) stop("Sum of weights must be equal to 1.")
  
  ## first, household (if specified)
  if(household){
    if(is.null(data$Zip5) || is.null(data$Address) ) {
      stop("Please make sure that columns Zip5 and Address exist in the dataset, for householding purposes.")
    }
    # show distribution:
    # data %>%
    #   dplyr::group_by(.data$Zip5, .data$Address) %>%
    #   dplyr::summarize(hhsize = n() ) %>%
    #   dplyr::group_by(hhsize) %>%
    #   dplyr::summarize(n = n()) %>%
    #   data.table::as.data.table -> hh.temp
    ## rewrite of the above without pipes, for speed:
    hh.temp <- dplyr::group_by(data, .data$Zip5, .data$Address)
    hh.temp <- dplyr::summarize(hh.temp, hhsize = dplyr::n() )
    hh.temp <- dplyr::group_by(hh.temp, .data$hhsize)
    hh.temp <- dplyr::summarize(hh.temp, n = dplyr::n() )
    hh.temp <- data.table::as.data.table(hh.temp)
    
    message("Household size distribution:")
    print(hh.temp)
    message("Excluding households larger than ", maxhhsize)
    
    # data %>%
    #   dplyr::group_by(.data$Zip5, .data$Address) %>%
    #   dplyr::summarize(hhsize = n() ) %>%
    #   dplyr::filter(hhsize <= maxhhsize) %>%
    #   data.table::as.data.table -> data.hh
    ## rewrite of the above without pipes, for speed:
    data.hh <- dplyr::group_by(data, .data$Zip5, .data$Address)
    data.hh <- dplyr::summarize(data.hh, hhsize = dplyr::n() )
    data.hh <- dplyr::filter(data.hh, .data$hhsize <= .data$maxhhsize )
    data.hh <- data.table::as.data.table(data.hh)
    
    hh.n <- nrow(data.hh) 
    n <- sum(data.hh$hhsize)
    message("Households: ", hh.n, " | Individuals: ", n)
    
    # generate labels we'll use to randomize the full dataset
    cond_labels <- randomize_cond(n = hh.n, weight, conditions, verbose)
    
    # apply condition labels and household IDs to householded data
    data.hh.r <- cbind(data.hh,
          condition = sample(cond_labels, size=hh.n, replace=F),
          hhid = c(1:hh.n)
          )
    
    # merge back to larger dataset
    merge(data.hh.r, data, by=c("Zip5", "Address"), all.x=T, all.y=F) -> data.r
    
    # data.hh.r %>%
    #   dplyr::group_by(condition) %>%
    #   dplyr::summarize(hh = n(),
    #             ind = sum(hhsize)) %>%
    #   dplyr::mutate(avg_hh = ind/hh) -> bal
    ## rewrite of the above without pipes, for speed:
    bal <- dplyr::group_by(data.hh.r, .data$condition)
    bal <- dplyr::summarize(bal, hh = dplyr::n(), ind = sum(.data$hhsize))
    bal <- dplyr::mutate(bal, avg_hh = .data$ind/.data$hh)
    
    message("Person/household balance:")
    print(bal)
    
    return(data.r)
  }
  else {
    nrow(data) -> n
    message("Individuals: ", n)
    
    # generate labels we'll use to randomize the full dataset
    cond_labels <- randomize_cond(n = n, weight, conditions, verbose)
    
    # apply condition labels to data
    cbind(data,
          condition = sample(cond_labels, size=n, replace=F)
    ) -> data.r
    
    # data.r %>%
    #   dplyr::group_by(condition) %>%
    #   dplyr::summarize(ind = n()) %>%
    #   dplyr::mutate(weight = ind/n) -> bal
    ## rewrite of the above without pipes, for speed:
    bal <- dplyr::group_by(data.r, .data$condition)
    bal <- dplyr::summarize(bal, ind = dplyr::n())
    bal <- dplyr::mutate(bal, weight = .data$ind/.data$n)
    
    message("Person balance across conditions:")
    print(bal)
    
    return(data.r)
  }
}