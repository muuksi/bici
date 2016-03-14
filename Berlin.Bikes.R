# Construction dataset berlin.bikes

berlin.bike.maufacturers <- data.frame("Schindelhauer", "Standert", "8bar", "Pasculli")

manufacturer <- c("Schindelhauer")
bikes <- c("Viktor", "Siegfried", "Siegfried Road", "Ludwig VIII", 
           "Ludwig XI", "Lotte", "Ludwig XIV", "Wilhelm 9-Gang-Pinion", 
           "Wilhelm 12-Gang-Pinion", "Wilhelm 18-Gang-Pinion", 
           "Friedrich VIII", "Friedrich XI", "Frieda", "Jacob", 
           "ThinBike", "Hektor Frameset")
speed <- c(1, 1, 1, 8, 11, 8, 14, 9, 12, 18, 8, 11, 8, 1, 2, NA)
frame <- c("aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium","aluminium")
gram <- c(8600, 8800,9100,11100, 11000, 11000, 11500, 12200, 12300, 12700,13400, 13300, 13400, 9700, 9900, 1670)
bottom.bracket <- c("BSA","BSA","BSA","BSA","BSA","BSA","BSA","Pinion", "Pinion", "Pinion", "BSA", "BSA", "BSA","BSA", "BSA","BSA")

schindelhauer <- data.frame(manufacturer, bikes, speed, frame, gram, bottom.bracket)

# Pasculli
manufacturer <- c("Pasculli")
bikes <- c("Angelone", "Bagnolo", "Altissimo", "Tomarlo", "Cremona", "Zovallo", "Pradello", "Pianazze", "Freccia")
speed <- c(NA)
frame <- c("aluminium", "steel","carbon","carbon","carbon","carbon","carbon","aluminium","carbon" )
gram <- c(1540,1620,680,780,1000,860,960,1480,1500)
bottom.bracket <- c("BSA","PF30","PF30","PF30","PF30","PF30","BB30","BSA","BB30")
pasculli <- data.frame(manufacturer, bikes,speed, frame, gram, bottom.bracket)


# Zusammenführen
library(dplyr)
berlin.bikes <- dplyr::full_join(schindelhauer, pasculli)
save(berlin.bikes, file="berlin.bikes.Rdata")
