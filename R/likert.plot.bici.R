#' A custom likert plot
#' saves a ggplot2 png file in current working directory
#'
#' @param data.levels dataframe
#' @param name_vars list of questions
#' @param labels labels of the scale
#' @param beschriftung title
#' @param pCite copyright or citing
#' @param center number
#' @param print true or false
#' @param percent percents in the graph?
#' @param histogram histogram? true or false
#' @param save save plot? true or false
#' @param width width of the plot in cm
#' @param height height of the plot in cm
#' @param nlevels How many levels do the variables have?
#'
#' @return
#' @export
#' @import likert
#' @import ggplot2 grid lubridate
#'
#' @examples
#'
#'
likertBar <- function(data.levels, name_vars, labels, beschriftung="text", pCite=paste("M.Hinse Quelle: Hinse & Lüdicke //",lubridate::today()), nlevels=5, center=3, print=TRUE, percent=FALSE, histogram=FALSE, save=TRUE, width=30, height=25){
  n=nlevels
  c=center
  t=beschriftung
  l=labels
  hg=histogram
  if (!all(sapply(data.levels, function(x) "factor" %in% class(x)))) {
    warning("Will convert to factors")
    for (i in 1:ncol(data.levels)) {
      data.levels[, i] <- factor(data.levels[, i], levels = 1:nlevels, labels = l)
    }
  }
  temp.df <- questionsAsVarnames(data.levels, name_vars)
  #head(temp.df)
  grouping = NULL
  likert.df <- likert::likert(temp.df, nlevels = n)
  p <- plot.new()
  p <- plot(likert.df,  include.histogram = hg, ordered=FALSE, legend="Antwortformat", legend.position="bottom", text.size=5, plot.percents=percent) +
    ggtitle(t) + 
    labs(y="Prozente") +
    theme(text = element_text(size=12, family = "Helvetica"),
          axis.text.y  = element_text(vjust=0.5, size=15),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  #p <- p +  annotation_custom(grob = grid::textGrob(label = pCite,   gp = gpar(cex = 0.5)),
  #                            ymin = -3.5,
  #                            ymax = -3.5,
  #                            xmin = 1,
  #                            xmax = 1)
  #gt <- ggplot_gtable(ggplot_build(p))
  #gt$layout$clip[gt$layout$name == "panel"] <- "off"
  #grid.draw(gt)
  if(save==TRUE){
    ggsave(paste(t,".png", sep = ""), plot=p, width = width, height = height, units = "cm", device = "png")
  } 
  p
  if(print==TRUE){
    return(kable(summary(likert.df),col.names = c("Frage", "Ablehnung", "Neutral", "Zustimmung", "Mittelwert", "SD"), digits = 2))
  }
  #return(gt)
  #p <- mtext(paste("© IZES gGmbH - ",t), side = 1, line=3, adj=0, cex = 0.4)
}