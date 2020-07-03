source("scripts/loadPackages.R")
source("scripts/loadFunctions.R")

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
  
    #script 00 needs to have been run to download these data
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

    #data frame of dates info corresponding to layers in raster brick dust data
    dts <- data.frame(year = yr, mth)



  
  ###process dust data
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
  
    


################################        Work with different locations to get data ################################################################        



#this is a stupid way to do this but just manually found lat lon for cities and pull in time series for each here

#note: get_series() ignores missing values from clouder cover and this generates a warning message because some periods all observations are missing


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






day_aves <- data_full  %>% group_by(doy) %>% dplyr::select(-harm, -season) %>%  summarise_at(vars(bod:dkr), mean, na.rm = T ) 
day_sd <- data_full  %>% group_by(doy) %>% dplyr::select(-harm, -season) %>%  summarise_at(vars(bod:dkr), sd, na.rm = T )          

names(day_aves)[2:12]<-paste( names(day_aves)[2:12], "_ave", sep ="")
names(day_sd)[2:12]<-paste( names(day_sd)[2:12], "_sd", sep ="")

data_full2 <- left_join(data_full, day_aves, by = "doy") %>% left_join(day_sd, by = "doy")

nms <- c("bod","bfs","nmy","bmk","nrb","dkr")

data_full2[,paste(nms,"_z",sep="")]<- (data_full2[,nms] - data_full2[,paste(nms,"_ave",sep="")])/data_full2[,paste(nms,"_sd",sep="")]


nms1<-nms[2:length(nms)]
cors <- list(); length(cors) <- length(nms1); names(cors)<-nms1
for(i in 1:length(cors)){
  
  cors[[i]]<-get_cor(as.numeric(data_full2[,nms1[i]]))
}


#write out data
save(cors, 
     bod_cells, nmy_cells, kno_cells, nrb_cells, bfs_cells, bmk_cells,dkr_cells,
     file = "data/figure_data/figED2_panel_b_data_new.RData")




############################################
## prepare spatial data for panel a

#  dts gives date info for layers
#   dust is raster brick   
grid[] <- 1:ncell(grid)  

africa_cells <- raster::extract(grid, africa, small = T) %>% unlist() %>% unique() %>% sort()
all_cells <- 1:ncell(grid)
non_africa_cells <- all_cells[all_cells %in% africa_cells == F]
dust_backup <- dust

dust[non_africa_cells] <- NA
dust[dust>2.5]<-2.5 #define upper bound
dust[dust<0]<-0 #define lower bound
dust[1]<-3  #set a max (for convenient raster color scale plotting)
dust[2]<-0  #set a min (for convenient raster color scale plotting)
#now have range 0-3 for color scale


start_pt <- which(dts$year == 2009 & dts$month == 2 & dts$dom == 10)
dust_plot <- dust[[(start_pt):(start_pt+5)]]
dates <- dts[(start_pt):(start_pt+5),]
names(dust_plot) <- paste(simpleCap(as.character(dates[,"month_name"]))," ",as.character(dates[,"dom"]),", ", as.character(dates[,"year"]), sep = "" )


writeRaster(dust_plot, filename = "data/figure_data/figED2_dust_data_new.nc", format = "CDF", overwrite= T)




