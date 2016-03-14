
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
getwd()
setwd("./bici")
document()


setwd("..")
getwd()
install("bici")

??bici
