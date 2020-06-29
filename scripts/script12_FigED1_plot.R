source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")

#dhs locations
dhsLatLon <- read_csv("data/inputs/dhs/dhs_locations.csv")    

#Bodele location
bodLatLon <- read_csv("data/inputs/spatial_boundaries/BodeleLocation.csv")


#africa shape
africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")





knn_results1 <- RANN::nn2(query = dhsLatLon[,c("lon","lat")],
                          data = bodLatLon[,c("lon","lat")],k = nrow(bodLatLon),eps = .1)

dhsLatLon$dist2bod <- knn_results1$nn.dists[,1]*111 #dist in ~km


#note: in paper we merged with data and plotted one obs for every birth so hist #s weighted by # of births
#not allowed to post DHS locations matched with births here so this version just plots one obs per location
#looks similar but not identical

dhsLatLon$dist_bin <- round(round(dhsLatLon$dist2bod)/250, 0)*250

tdb <- dhsLatLon %>% group_by(dist_bin) %>% 
      summarise(n_births = sum(n_births))



#set countries to gray if they're not in our sample and white if they are
col <- rep('gray70', nrow(africa))
#cote d'ivoire spelled differently in two sources to manually change to white
col[africa@data$NAME_0 %in% unique(data$country)]<-"white"
col[africa@data$NAME_0 %in% "Côte d'Ivoire"]<-"white"

#set point colors
col1 <- rep('coral', nrow(dhsLatLon))
col1[dhsLatLon$region==0]<-"blue"



pdf("figures/raw/FigED1a_raw.pdf",width = 5, height = 5)
par(mar = c(0,0,0,0))
plot(africa, col =col, lwd = 0.25,border = 'white')
points(dhsLatLon[,c("lon","lat")], col = col1, pch = 16, cex=0.2)
dev.off()



pdf("figures/raw/FigED1b_raw.pdf",width = 5, height = 3)
par(mar = c(5,5,2,2))
barplot(tdb$n_births, axes =F, ylim = c(0, 200001), col = 'gray90')
axis(1,at = c(1,seq(5, 27,5)), labels = c(500,seq(1000,5000,1000)),tick = F, cex.axis=1,line=-0.5)
axis(2,las=2, at = seq(0,200000,50000), labels = seq(0,200000,50000)/1000)
mtext(side = 1, text = 'Distance to Bodele Depression (km)',line=3,cex=1.3)
mtext(side = 2, text = "Observed births (thousands)",line=3,cex=1)
dev.off()






