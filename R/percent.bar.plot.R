
#' percent.bar.plot
#'        Funktion zur Erstellung einer Prozent BalkenGrafik
#'
#' @param xx data as data.frame
#' @param title The title
#' @param ylab Description Y-axes
#' @param xlab Description X-axses
#' @param statistic "count" or "bin" only one
#' @param bin  binwidth parameter only for "bin"
#' @param save if TRUE a 20x20cm png will be saved 
#'
#' @return
#' @export
#' @import ggplot2 scales lubridate grid
#'
#' @examples
#' pCite <- paste("© @muuksi",lubridate::today(),"| Quelle: M.Hinse // hinse.eu")
#' percent.bar.plot(xx=berlin.bikes$frame,  title = "Rahmen ", ylab = "Häufigkeit", 
#'                xlab = "Material", pCite=paste("© @muuksi",lubridate::today(),"| Quelle: M.Hinse // hinse.eu"), 
#'                statistic = "count", save=TRUE)
#' ggsave("Verwendete Rahmen Materialien.png", units="cm", width = 20, height = 15, dpi=300)
#' 
#' 
#' 
# library(grid)
#
percent.bar.plot <- function(xx, title, ylab, xlab, pCite=paste("© @muuksi",lubridate::today()), statistic=c("bin", "count"), save=TRUE, bin=NA){
  
  q=ggplot(data=data.frame(xx),aes(x=xx))+
    geom_bar(aes(y = (..count..)),fill="orange", if(statistic=="bin"){binwidth=bin},width=1)+
    theme(plot.margin = unit(c(1,1,2,1), "lines")) +
    #scale_x_discrete(breaks=levels(xx),labels=levels(xx),limits=nlevels(xx)) +
    ggtitle(title) + 
    ylab(ylab)   + 
    xlab(xlab) +
    geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",paste(..count..,"\n",scales::percent((..count..)/sum(..count..))))), stat=statistic, colour="darkgreen") 
  q + theme(text = element_text(size=15),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.y  = element_text(vjust=0.5, size=15),
        plot.title = element_text(size = 20, family = "Helvetica")) 
  q <- q +  annotation_custom(grob = grid::textGrob(label = pCite,  hjust = "rigth", gp = gpar(cex = 0.5)),
                      ymin = -3.5,      # Vertical position of the textGrob
                      ymax = -3.5,
                      xmin = 1,         # Note: The grobs are positioned outside the plot area
                      xmax = 1) 
  q
  #mtext(pCite, adj=1, side=1, cex=0.7, line=3)
  #scale_fill_manual(values=nlevels(xx), breaks=levels(xx), labels=levels(xx))
  gt <- ggplot_gtable(ggplot_build(q))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
  if(save==TRUE){
    ggsave(paste(title,".png"), plot=gt, device = "png", units="cm", width = 20, height = 15, dpi=300)
  }
  return(q)
}

