#' Vectorized YAML parsing
#'
#' Extract a single variable at a time from a field in a table that holds YAML data.
#' 
#' @name yaml.colparse
#' @keywords yaml actionnetwork action network
#' @export yaml.colparse
#' 
#' @usage yaml.colparse(data, varname = c())
#' 
#' @param data YAML string---can be a vector of strings.
#' @param varname String of the variable name you want to extract. If no varname is given, or if it's a vector of variable names, the function will instead print a list of the available variables. If the varname doesn't exist in a given row, function will return NA.
#' 
#' @examples
#' donors <- data.table(
#'   id = c(1:6),
#'   yaml_values = c(
#'     "---\n- :type: Group\n  :id: 48\n  :amount: '16.25'\n",
#'     "---\n- :type: User\n  :id: 257\n  :amount: '10.00'\n",
#'     "---\n- :type: User\n  :id: 257\n  :amount: '15.00'\n",
#'     "---\n- :type: Group\n  :id: 48\n  :amount: '25.00'\n",
#'     "---\n- :type: Group\n  :id: 48\n  :amount: '10.00'\n",
#'     "---\n- :type: User\n  :id: 257\n  :amount: '15.00'\n"
#'   )
#' )
#' yaml.colparse(donors$yaml_values)
#' donors[, amount1 := yaml.colparse(yaml_values, ":amount") ]
#' donors[, amount2 := yaml.colparse(yaml_values, "amount") ]
#' donors

yaml.getvar <- function(yamlstring, varname){
  temp <- yaml::yaml.load(yamlstring)
  temp <- unlist(temp)
  temp <- as.vector(temp[varname])
  return(temp)
}

yaml.colparse <- function(data, varname = c()){
  if(length(varname)!=1) {
    data <- utils::head(data,10)
    vars <- c()
    for(i in length(data)) {
      temp <- yaml::yaml.load(data[i])
      temp <- unlist(temp)
      temp <- names(temp)
      vars <- c(vars, temp)
    }
    message("Available variables in first 10 rows: ")
    vars <- levels(as.factor(vars))
    print(vars)
    
    warning("varname must be a single string; you can only extract one variable at a time.")
    return()
    }
  temp <- sapply(data, yaml.getvar, varname)
  names(temp) <- NULL
  return(temp)
}

#### old version:
# yaml.getvar <- function(yamlthings, varname) {
  # return( yaml::yaml.load(yamlthings)[[varname]] )
# }

# yaml.colparse <- function(data, varname) {
  # if(length(varname)!=1) {stop("Error: varname must be a single string; you can only extract one variable at a time.")}
  # temp <- sapply(data, yaml.getvar, varname)
  # temp <- tolower(temp)
  # temp <- as.factor(temp)
  # return(temp)
  
  # # data %>% 
    # # sapply(yaml.getvar, varname) %>%
    # # tolower() %>%
    # # as.factor() %>%
    # # return()
# }