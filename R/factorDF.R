#' factorDF
#'          Factoring all variables in the given data.frame
#'
#' @param data daraframe
#' @param levels defining all levels=c(0,1)
#' @param labels labels of levels.
#'        labels=c("Nein","Ja")
#'
#' @return data the new dataframe
#' @export
#'
#' @examples
factorDF <- function(data, levels=c(1:6), labels=skala){
  df <- data
  #i=1
  for(i in 1:(length(data))){
    f <- factor(data[i], levels=levels, labels = labels)
    f
    data[i] <- f
  }
  #names(df) <- names(data)
  return(data)
}