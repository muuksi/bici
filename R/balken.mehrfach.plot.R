###################################
#
# Balkendiagram wie likert
#
###################################
# inc_daten_za4753.r
#library(memisc)
#ZA4753<-spss.system.file("ZA4753_v1-1-0.sav")
#daten<-subset(ZA4753,select=c(v106,v159,v160,v161,v162,v163,v164,v165))
# str(daten)
# attach(daten)
# detach(daten)
# data = daten
#
#d=NULL
#for(i in daten){
#         y<-100*table(as.matrix(i))/length(i)
#         d<-cbind(d,y)
#}

# d <- questionsAsVarnames(dataframe = as.data.frame(d), questions = c("Spar_Hell_100", "Spar_Lat_101", "Spar_Weit_102", "Spar_Eff_103"))
# d
# y<-100*table(as.matrix(v165))/length(v165)
# z<-rbind(z,y)
# y<-100*table(as.matrix(v164))/length(v164)
# z<-rbind(z,y)
# y<-100*table(as.matrix(v163))/length(v163)
# z<-rbind(z,y)
# y<-100*table(as.matrix(v162))/length(v162)
# z<-rbind(z,y)
# y<-100*table(as.matrix(v161))/length(v161)
# z<-rbind(z,y)
# y<-100*table(as.matrix(v160))/length(v160)
# z<-rbind(z,y)
# y<-c(0,100*table(as.matrix(v159))/length(v159))
# z<-rbind(z,y)
# antworten<-c("k.A./weiss nicht","stimme voll und ganz zu","stimme zu","stimme nicht zu","stimme überhaupt nicht zu")
#
# daten0<-cbind(z[,1]+z[,2],z[,3],z[,4],z[,5],z[,6])
# daten0
# daten1<-t(daten0)
# daten1
#
# f_v159<-"Eine berufstätige Mutter kann ihrem Kind\ngenauso viel Wärme und Sicherheit geben\nwie eine Mutter,die nicht arbeitet"
# f_v160<-"Ein Kleinkind wird wahrscheinlich darunter\nleiden,wenn die Mutter berufstätig ist"
# f_v161<-"Ein Beruf ist gut,aber was die meisten\nFrauen wirklich wollen,ist ein Heim und Kinder"
# f_v162<-"Hausfrau zu sein,ist genauso\nbefriedigend wie eine Berufstätigkeit"
# f_v163<-"Berufstätigkeit ist der beste Weg\nfür eine Frau,um unabhängig zu sein"
# f_v164<-"Beide,Mann und Frau,sollten\nzum Haushaltseinkommen beitragen"
# f_v165<-"Im allgemeinen sind Väter genauso geeignet,\nsich um die Kinder zu kümmern wie Mütter"
# f_v166<-"Männer sollten für das zu Hause und für die Kinder\ngenauso viel Verantwortung übernehmen wie Frauen"
# namen<-c(f_v165,f_v164,f_v163,f_v162,f_v161,f_v160,f_v166)
#
# colors<-c("grey98","#C3BB2B", "#FFF988", "#DBD452","#9D9615","#746E00")
# library(RColorBrewer)
 


#' Balkendiagram von mehrfachen Variabeln untereinander
#'
#' @param df The data-frame that holds all variables
#' @param varnames A list of Variable names you like to get printed
#' @param out Saving the graph as a file? Then opting in for out="png" or out="pdf"
#' @param filename The filename for the graph
#' @param colors The color palette. Using RColorBrewer as example
#' @param bg The backgound color
#'
#' @return
#' @export
#' @importFrom psych describe
#' @import RColorBrewer
#'
#' @examples
#' vars <- c("Vis_Gut_1",
#'            "Vis_Zurecht_2",
#'            "Vis_Umg_3",
#'            "Vis_Spaz_4",
#'            "Vis_Übers_5",
#'            "Vis_Stimm_6",
#'            "Vis_Einsehb_7",
#'            "Vis_Abw_8",
#'            "Vis_Schutz_9",
#'            "Vis_Nat_10")
#' df <- data.frame(SLS[vars])
#' #names(SLS)
#' Vars 1 -10  
#'filename="Visuelle Bewertung der Wohnumgebung.png"
#' varnames <- c("Die Umgebeung gefällt mir hier sehr gut.",
#'              "Ich finde mich hier in der Umgebung gut zurecht.",
#'              "Ich finde die Umgebung hier ist gut zu überschauen.",
#'              "Die Umgebung hier lädt zum Spazierengehen ein.",
#'              "Ich empfinde die Umgebung als sehr übersichtlich.",
#'              "Ich empfinde die Umgebung als stimmig und organisiert.",
#'              "Es gibt hier viele schlecht einsehbare Ecken.",
#'              "Ich empfinde die Umgebung als sehr vielfältig und abwechslungsreich.",
#'              "Es gibt in der umgebung viele Möglichkeiten, Schutz vor Gefahren zu finden.",
#'              "Ich denke, die Umgebung hier ist sehr natürlich.")
#' colors <- brewer.pal(5, "Greens") # colors from RColorBrewer package. 5 Colors
#' balken.mehrfach.plot(df, varnames=varnames, out = "png", filename=filename)
#' skala <- c("Trifft überhaupt nicht zu", "Trifft überwiegend zu", "Trifft eher zu", "Trifft gerade noch zu", "Trifft überwiegend zu", "Trifft voll und ganz zu")

balken.mehrfach.plot <- function(df,
                                 varnames=names(df),
                                 skala=c("Trifft überhaupt nicht zu", "Trifft überwiegend zu", "Trifft eher zu", "Trifft gerade noch zu", "Trifft überwiegend zu", "Trifft voll und ganz zu"),
                                 out=c("png","pdf"),
                                 filename="name",
                                 colors=brewer.pal(5, "Greens"),
                                 bg=colors[1]){
        daten <- bici::questionsAsVarnames(df, varnames)
        col <- colors
        desc <- psych::describe(daten)
       
        n <- is.na(daten)
        length(n[,1])
        no.na <- na.exclude(daten)
        no.na$na.action
        length(na.action(no.na))
        d=NULL
        i=1
        for(i in daten){
          no.na <- na.exclude(daten[i])
          n <- table(desc$n[i]/length(na.action(no.na)))
          d<-cbind(d,y)
        }
        d=NULL
        for(i in daten){
                y<-100*table(as.matrix(i))/length(i)
                d<-cbind(d,y)
        }
        daten1 <- d

        if(out == "png"){
                 png(filename, width = 2000, height = 2000, res = 300, bg = bg)
        }
        if(out == "pdf"){
                cairo_pdf(bg=bg, filename, width=13, height=10.5)
        }

        par(omi=c(0.25,0.75,1,0.75),mai=c(1.8,3.75,0.25,0),lheight=1.15,
            family="Helvetica",las=1, cex=0.8, bg=bg)
        plot.new()
        
        barplot(-rep(100,length(varnames)),names.arg="",cex.names=1,horiz=T,
                border=par("bg"),xlim=c(-100,70),col="grey",axes=F)
        desc
        barplot(-(100-daten1[1,]),names.arg="",cex.names=1,horiz=T,
                border=par("bg"),xlim=c(-100,70),col=bg,axes=F,add=T)
        barplot(-daten1[3:2,],names.arg="",cex.names=1,horiz=T,
                border=NA,xlim=c(-100,70),col=col[3:2],axes=F,add=T)
        for(i in 1:length(daten1)){
                mtext(paste(round(daten1[i], 2), "%"), side=1, line=-1, at=i+2)
        }
        mtext(paste(round(daten1[8], 2), "%"), side=2, cex=0.8,line=-1, at=8)
        
        barplot(daten1[4:5,],names.arg=varnames,cex.names=1,horiz=T,
                border=NA,xlim=c(-100,70),col=col[4:5],axes=F,add=T)

        # weitere Elemente
        arrows(0,-0.1,0,8.6,lwd=2.5,length=0,xpd=T,col=col[length(col)])
        px<-c(-98,-87,-41,15,65); tx<-c(-105,-48,-21,8,60); y<-rep(-1,5)
        points(px,y,pch=15,cex=4,col=col,xpd=T)
        text(tx,y,skala,adj=1,xpd=T,font=3)
        mtext(c(80,60,40,20,0,20,40,60),at=c(-80,-60,-40,-20,0,20,40,60),1,
              line=0,cex=0.95)
        # Betitelung
        mtext("Man spricht ja oft davon, dass sich heutzutage die Rollen von Mann und Frau verändern",3,line=2.2,adj=0,cex=1.8,outer=T)
        mtext("Alle Angaben in Prozent",3,line=1,adj=1,cex=0.95,font=3)
        mtext("Quelle: European Values Study 2008 Deutschland, ZA4753. www.gesis.org. Design: Stefan Fichtel, ixtract",1,line=5.2,adj=1,cex=0.95,font=3)
        mtext("N=2.075",3,line=1,adj=0,cex=1.15,family="Lato",font=3)


        dev.off()
}





