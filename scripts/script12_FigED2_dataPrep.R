source("scripts/0_loadPackages.R")
source("scripts/0_loadFunctions.R")

data <- read_rds("data/inputs/analysis_data.rds")
africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")

 

################## Setup some useful info and functions     ################# 

# Figure out timing of day
    
    doy <- data.frame(doy = 1:365, 
                      month =c(rep(1,31), rep(2, 28), rep(3, 31), rep(4, 30), rep(5, 31), rep(6, 30), rep(7,31), rep(8,31), rep(9, 30), rep(10, 31), rep(11,30), rep(12, 31)),
                      dom = c(1:31, 1:28, 1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31),
                      month_name =c(rep("jan",31), rep("feb", 28), rep("mar", 31), rep("apr", 30), rep("may", 31), rep("jun", 30), rep("jul",31), rep("aug",31), rep("sep", 30), rep("oct", 31), rep("nov",30), rep("dec", 31)))
    
    doy_leap <- data.frame(doy = 1:366, 
                           month =c(rep(1,31), rep(2, 29), rep(3, 31), rep(4, 30), rep(5, 31), rep(6, 30), rep(7,31), rep(8,31), rep(9, 30), rep(10, 31), rep(11,30), rep(12, 31)),
                           dom = c(1:31, 1:29, 1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31),
                           month_name =c(rep("jan",31), rep("feb", 29), rep("mar", 31), rep("apr", 30), rep("may", 31), rep("jun", 30), rep("jul",31), rep("aug",31), rep("sep", 30), rep("oct", 31), rep("nov",30), rep("dec", 31)))
    
    #want to be able to pull out month mid points from doy
    doy_ave <- doy %>% group_by(month_name) %>% summarise(doy = mean(doy))

    #simplified version of bodele cells for this grid. 
    bod_cells <- c(1734, 1735, 1736, 1822, 1823, 1824, 1825, 1911, 1912, 1913)





###########  ####### LOAD dust data ############### ##############
  
   files <- list.files("data/inputs/daod/", pattern = "\\.nc$")	#get file names 

    
    rb <- list()
    for(i in 1:length(files)){
      rb[[i]] <- raster::crop(brick(paste("data/inputs/daod/", files[i], sep = "")), extent(africa))  
    }
    
    dust <- rb %>% do.call(stack, . )
    grid <- raster(dust[[1]])

    
    ndays <- data.frame(years = 2001:2016, ndays= NA)
    for (i in 1:length(files)){
      
      ndays$ndays[i] <- dim(brick(paste("data/inputs/daod/", files[i], sep = "")))[3]
                                  }


    
    
yr <- list()
for(i in 1:16){yr[[i]]<-rep(ndays$year[i], ndays$ndays[i])}
yr <- unlist(yr)



mth <- list()
for (i in 1:16){
  doy_count<- data.frame(doy = as.numeric(substr(names(brick(paste("data/inputs/daod/", files[i], sep = ""))), 2,10)) - min(as.numeric(substr(names(brick(paste("data/inputs/daod/", files[i], sep = ""))), 2,10)))+1)
  
  if(i %in% c(4,8,12,16)){
    mth[[i]] <- left_join(doy_count, doy_leap ) 
  }else{
    mth[[i]] <- left_join(doy_count, doy) 
    
  }
  
}
mth <- data.frame(data.table::rbindlist(mth))


dts <- data.frame(year = yr, mth)



  
###initialize dust data
tmp <- dust[bod_cells] %>% apply(2,function(x){max(x, na.rm =T)})
tmp[is.infinite(tmp)]<-NA

data_full <- data.frame(dts, bod = tmp)
rownames(data_full) <- 1:nrow(data_full)       

data_full$harm <- as.numeric(data_full$month %in% c(10:12, 1:3))
data_full$season <- NA
data_full$season[data_full$month %in% 10:12] <- data_full$year[data_full$month %in% 10:12] - 2000
data_full$season[data_full$month %in% 1:3] <- data_full$year[data_full$month %in% 1:3] - 2001
data_full <- data_full %>% filter(harm == 1 & season > 0)




comp <- data_full %>% mutate(child_birth_year = year, child_birth_month = month) %>%  dplyr::select(starts_with("child"), "bod")
aod_bod <- comp %>%  group_by(child_birth_year) %>% summarise(bod = mean(bod, na.rm= T))

elsewhere <- data %>% group_by(child_birth_year, child_birth_month) %>% summarise(pm = mean(pm25_post), bod_vd = mean(dust_bod_post))
bod_vand <- data %>% group_by( child_birth_year) %>% summarise(bod_vd = mean(dust_bod_post))


bod_vand <- left_join(bod_vand, aod_bod) %>% filter(!is.na(bod))
bod_vand$r_vd <- summary(lm(bod_vd ~ child_birth_year, data = bod_vand))$residuals
bod_vand$r_aod <- summary(lm(bod ~ child_birth_year, data = bod_vand))$residuals






# 
# loc <- elsewhere[,c("lon","lat")]
# loc <- loc[!duplicated(loc),]
# 
# aod_else <- dust[cellFromXY(grid, as.matrix(loc))]



################################        Work with different locations to get data ################################################################        





bfs_cells <- get_cells(-2.416584, 13.579979 )
data_full$bfs <- get_series(bfs_cells)

kno_cells <- get_cells(8.544332, 11.995854)
data_full$kno <- get_series(kno_cells)

nmy_cells <- get_cells(2.119039, 13.533891)
data_full$nmy <- get_series(nmy_cells)

bmk_cells <- get_cells(-7.997006, 12.616147)
data_full$bmk <- get_series(bmk_cells)

nrb_cells <- get_cells(36.828159, -1.263360)
data_full$nrb <- get_series(rbind(c(36.828159, -1.263360),c(36.828159, -0.263360),c(36.828159, -2.263360),c(37.828159, -1.263360) ))

dkr_cells <- get_cells(-15.874903, 14.875502)
data_full$dkr <- get_series(dkr_cells)

djo_cells <- get_cells(1.669671, 9.697895)
data_full$djo <- get_series(djo_cells)

mtn_cells <- get_cells(-12.460984, 18.73)
data_full$mtn <- get_series(mtn_cells)

kms_cells <- get_cells(-1.617377, 6.642829)
data_full$kms <- get_series(kms_cells)

lagos_cells <- get_cells(3.348516, 6.524115)
data_full$lagos <- get_series(lagos_cells)






day_aves <- data_full  %>% group_by(doy) %>% dplyr::select(-harm, -season) %>%  summarise_at(vars(bod:lagos), mean, na.rm = T ) 
day_sd <- data_full  %>% group_by(doy) %>% dplyr::select(-harm, -season) %>%  summarise_at(vars(bod:lagos), sd, na.rm = T )          

names(day_aves)[2:12]<-paste( names(day_aves)[2:12], "_ave", sep ="")
names(day_sd)[2:12]<-paste( names(day_sd)[2:12], "_sd", sep ="")

data_full2 <- left_join(data_full, day_aves, by = "doy") %>% left_join(day_sd, by = "doy")

nms <- c("bod","bfs","nmy","bmk","nrb","dkr","djo","mtn","kms","lagos","kno")

data_full2[,paste(nms,"_z",sep="")]<- (data_full2[,nms] - data_full2[,paste(nms,"_ave",sep="")])/data_full2[,paste(nms,"_sd",sep="")]


nms1<-nms[2:length(nms)]
cors <- list(); length(cors) <- length(nms1); names(cors)<-nms1
for(i in 1:length(cors)){
  
  cors[[i]]<-get_cor(as.numeric(data_full2[,nms1[i]]))
}




######### TEMPORAL FIGURE ############


pdf("figures/raw/FigED2b_insert_raw.pdf",width = 6, height = 3.5)       
par(mar = c(0,0,0,0))

plot(africa, lwd = 0.01, col = 'gray90', border = 'gray30')
points(xyFromCell(grid, bod_cells), pch = 15, col = 'red', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, nmy_cells)), pch = 15, col = 'forestgreen', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, kno_cells)), pch = 15, col = 'orange', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, nrb_cells)), pch = 15, col = 'navy', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, bfs_cells)), pch = 15, col = 'red3', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, bmk_cells)), pch = 15, col = 'royalblue', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, dkr_cells)), pch = 15, col = 'purple', cex = 0.4)
dev.off()




pdf("figures/raw/FigED2b_raw.pdf", width = 7.5, height = 5)      
par(mar = c(5,5,4,4))    

xy <- data.frame(x = -2:6, y1 = cors[["nmy"]], y2 = cors[["kno"]], y3 = cors[["mtn"]], y4 = cors[["nrb"]], y5 = cors[["dkr"]], y6 = cors[["kms"]], y7 = cors[["lagos"]], y8 = cors[["bmk"]], y9 = cors[["djo"]], y10 = cors[["bfs"]]) %>% filter(x>=0)

plot(0:6, predict(loess(y1 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), type = "l", xlab = "", axes=F,ylab = "", ylim = c(-.025, 0.25), col = NA,lwd = 2)

abline(h = seq(0, 0.25, 0.05), col = 'gray75', lwd = 0.75, lty = 4)
abline(h = 0, lty = 4, col = 'black', lwd = 0.75)

lines(0:6, predict(loess(y1 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'forestgreen', lwd = 2)
lines(0:6, predict(loess(y2 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'orange', lwd = 2)
lines(0:6, predict(loess(y4 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'navy', lwd = 2)
lines(0:6, predict(loess(y10 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'red3', lwd = 2)
lines(0:6, predict(loess(y8 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'royalblue', lwd = 2)
lines(0:6, predict(loess(y5 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'purple', lwd = 2)


axis(1, at = 0:6, labels = c("day t", paste("day t+", 1:6, sep = "")))
axis(2, tick = T, las =2)

mtext(side = 1, text = "Lagged day",line = 3.5, cex =1.25)
mtext(side = 2, text = "Daily correlation ",line = 3.5, cex = 1.25)

dev.off()






#######################


#  dts gives date info for layers
#   dust is raster brick   
grid[] <- 1:ncell(grid)  

africa_cells <- raster::extract(grid, africa, small = T) %>% unlist() %>% unique() %>% sort()
all_cells <- 1:ncell(grid)
non_africa_cells <- all_cells[all_cells %in% africa_cells == F]
dust_backup <- dust

dust[non_africa_cells] <- NA
dust[dust>2.5]<-2.5 #ub
dust[dust<0]<-0 #lb
dust[1]<-3 #set a max
dust[2]<-0 #set a min
#now have range 0-3 for color scale





pal1 <- colorRampPalette(colors = add.alpha(c("#404096","#63AD99","#BEBC48","#E66B33","#D92120"), 0.5))

start_pt<- tp<-which(dts$year == 2009 & dts$month == 2 & dts$dom == 10)


pdf("figures/raw/FigED2a_raw.pdf", width = 3, height = 4)
par(mfrow = c(3,2))
for(tp in (start_pt):(start_pt+5)){
  par(mar = c(0,0,0,0))
  plot(africa, border = NA)
  plot(dust[[tp]], add = T, legend = F, col = pal1(256))
  plot(africa, add=T, lwd = 0.1)
  rect(xleft = -40, xright = -19, ybottom = -40, ytop = 40, border = NA, col ='white')
  rect(xleft = -40, xright = 0, ybottom = -40, ytop = 0, border = NA, col ='white')
  rect(xleft = 60, xright = 70, ybottom = -40, ytop = 40, border = NA, col ='white')
  rect(xleft = -40, xright = 60, ybottom = -60, ytop = -40, border = NA, col ='white')
  rect(xleft = 52, xright = 80, ybottom = -60, ytop = 40, border = NA, col ='white')
  
  mtext(side = 3, text = paste(simpleCap(as.character(dts[tp,"month_name"]))," ",as.character(dts[tp,"dom"]),", ", as.character(dts[tp,"year"]), sep = "" ) ,cex=0.25,line = -0.5) 
}


dev.off()


