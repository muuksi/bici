
#' loadAndCite packages
#'
#' Load and if not installed yet install packges in the list
#'
#' @param packages A list of packages to install and cite
#' @param bib bibtex file with packages to export
#'
#'
#' @export
#' @importFrom repmis LoadandCite
#'
#' @examples
#' packages <- c("devtools", "likert", "reshape", "psych", "questionr", "foreign", "GPArotation", "knitr", "corrplot")
#' bib <- "loaded_packages.bibtex"
#' loadAndCite(packages, bib)
#'
loadAndCite <- function(packages=c("devtools", "likert", "reshape", "psych", "questionr", "foreign", "GPArotation", "knitr", "corrplot"), bibtex="packages.bibtex" ){
  # checking for required packages
  require(repmis)
  list.of.packages <- packages
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {repmis::LoadandCite(pkgs=list.of.packages, bib=TRUE, file=bibtex, install=TRUE) } else {repmis::LoadandCite(pkgs = list.of.packages, bib = TRUE, file = bibtex) }

}

