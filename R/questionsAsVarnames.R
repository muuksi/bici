###########################################
# Original question as variable name
# Author: Maximilian Hinse
# Date: 29.01.2015
############################################
#' Adding the questions form survey to the end of variable names in dataframe
#'
#' @param dataframe A dataframe containing the data
#' @param questions A list with the questions for the new variable names
#'
#' @return new_df
#' @export
#' @importFrom reshape rename
#'
#' @examples
#' # Ausformulierte Fragen angeben. Gleiche Anzahl, gleiche Reihenfolge
#' head(cars)
#' questions <- c("How fast is the car?", "How far can you you go with it? ")
#' new.df <- questionsAsVarnames(dataframe=cars, questions)
#' head(new.df)
#'
questionsAsVarnames <- function(dataframe,questions){

  # Variablen werden nun umbenannt in "Varname: Frage"
  vars <- variable.names(dataframe)
  n <- length(vars)
  oldvars <- variable.names(dataframe)
  some.names <- structure(paste0(oldvars,": ",questions), names=oldvars)
  new_df  <- reshape::rename(dataframe, replace=some.names)
  return(new_df)
}
