#' bpDF Boxploting one or two whole dataframes with variables and means
#'
#' @param df
#'              a data.frame with variables
#' @param df2
#'              the second dataframe if desired
#' @param df.name
#'              A string to plot left for identifiying the data
#' @param df2.name
#'              For the second dataframe
#' @param bpVarNames
#'              Variable names. Ideally the whole question
#' @param bpTitle
#'              The title
#' @param bpSubtitle
#'              The subtitle
#' @param bpSkala
#'              The scale: A list with strings
#' @param nlevels
#'              How many levels have the variables?
#' @param colors
#'              Colors for means. A list with five colors
#' @param bg
#'              Color for the background. Defaults to first color in colors
#' @param bpCite
#'              The source of the Data or copyrigth notice
#' @param ...
#'              Params passing througth to boxplot(...)
#'
#' @return
#' @export
#' @importFrom psych describe
#'
#' @examples
#'
#'
#' formulierungen <- c("Ich befürworte es, wenn die Stadt versucht, \nbei der Straßen- und Gehwegbeleuchtung \nStrom zu sparen.",
#'                      "Es ist gut, wenn die Stadt versucht,\n in den öffentlichen Gebäuden \nEnergie zu sparen.",
#'                      "Die Stadt sollte versuchen, \n sich auf neunen Wegen \nfür den Klimaschutz einzusetzen.")
#' bpTitle = "Möglichkeiten der Energieeinsparung"
#' bpSubtitle = ""
#' bpSkala = c("Trifft überhaupt nicht zu", "Trifft überwiegend zu", "Trifft eher zu", "Trifft gerade noch zu", "Trifft überwiegend zu", "Trifft voll und ganz zu")
#' bpCite=paste("© @muuksi",lubridate::today(),"| Quelle: M.Hinse & F.Lüdicke, Saarlouis-Steinrausch // IZES.de")
#' nlevels=6
#' df = LED.df
#'
#' # Funktion aufrufen
#' bpDF(df, bpVarNames, bpTitle, bpSubtitle="LED", bpSkala, nlevels, color, bpCite)
#'
#'
#' bpDF(KONV.df, df2 = LED.df, df.name = "HQL", df2.name="LED", bpTitle=bpTitle, bpSubtitle = bpSubtitle, bpVarNames = formulierungen, bpSkala = bpSkala, nlevels = 6, bpCite = bpCite)

bpDF <- function(df,
                  df2=NULL,
                  df.name="",
                  df2.name="",
                  bpVarNames=names(df),
                  bpTitle=names(df),
                  bpSubtitle="Subtitle",
                  bpSkala=NULL,
                  nlevels=max(df[1]),
                  colors=brewer.pal(5, "Greens"),
                  bg=colors[1],
                  bpCite=paste("© @muuksi",lubridate::today()), ...){
        #TextBreite <- length(bpVarNames)
        #TextBreite <- 5.5*TextBreite;
        par(mar=c(4, 18, 4, 4), bg=bg)
        plot.new()
        desc1 <- psych::describe(df)
        #Zeilenhoehe <- 0.66
        #titelhoch <- length(bpVarNames) + length(bpTitle)
        #plot.window(xlim=c(10, 50), ylim=c(20, 40))
        bp <- boxplot(df, horizontal=TRUE, show.names=FALSE,
                       border = colors[4], frame.plot = TRUE,
                      pars = list(outpch = 1, outwex=0.2, boxwex=0.4, boxfill=NA, yaxt="n", xaxt="n",
                                  boxcol=colors[4] , whiskcol=colors[4] , staplecol=colors[4],
                                  medlty=1, medcol=colors[4], bty="1",pty="m", outlty="blank",outcol="grey"), ...)
        ## Add title
        title(main=bpTitle, cex.main = 1, line=1.5)
        ## Define locations for additional chart elements
        at <- c(1:length(bp$names))
        ## Get Group means and plot them with points and text

        ## IF a DF2 is there
        if(!is.null(df2)){
                desc2 <- psych::describe(df2)
                bp2 <- boxplot(df2, horizontal=TRUE, add = T, show.names=FALSE, varwidth=T,frame.plot =FALSE, border = colors[4],
                              pars = list(outpch = 1, outwex=0.2, boxwex=0.4, boxcol=colors[5], boxfill=NA,
                                          whiskcol=colors[5] , staplecol=colors[5], yaxt="n", xaxt="n",
                                          medlty=1, medcol=colors[5], bty="1",pty="m", outlty="blank",outcol="grey"), ...)
                j=1
                means2 <- sapply(df2, mean, na.rm=TRUE)
                for (i in means2) {
                        points( i, (j-.10), pch = 21, cex = 1.8, bg = colors[5], col = colors[5])
                        text(i , (j-.35), labels = formatC(i, format = "f", digits = 2), pos = 3, cex = 1, col = colors[5])
                        j<-j+1
                }
                mtext(paste("N =",desc2$n), side = 4, at = at , cex=.75, line = 0, col = colors[5])
                mtext(df2.name, side = 3, adj = 1, cex=.75, line = 0, col = colors[5])
        }
        means1 <- sapply(df, mean, na.rm=TRUE)
        j=1
        for (i in means1) {
                points( i, (j+.10), pch = 21, cex = 1.8, bg = colors[4], col=colors[4])
                text(i , (j+.15), labels = formatC(i, format = "f", digits = 2), pos = 3, cex = 1, col = colors[4])
                j<-j+1
        }

        ## Print group labels left at y axis
        axis(2, at=at, labels=bpVarNames, las=2, cex.axis=0.9, line=1)
        ## Print scale labels under x axis
        axis(1, at=(1:nlevels), labels=bpSkala, las=1, cex.axis=0.7, line=1)

        mtext(paste("N =",desc1$n), side = 2, at = at , cex=.75, line = 0, col = colors[4])
        mtext(df.name, side = 3, adj = 0, cex=.75, line = 0, col = colors[4])
        mtext(bpSubtitle, side=3, cex=0.8, line=-1)
        mtext(bpCite, adj=1, side=1, cex=0.7, line=3)
}
