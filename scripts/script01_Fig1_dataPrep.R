source("scripts/0_loadPackages.R")
source("scripts/0_loadFunctions.R")
africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")


### [1] Load data ###

          ##### Bring in pm.nodust.5 data
          dat.files <- list.files("data/inputs/pm25/", pattern = "\\.nc$")	#get file names 
          
          #separate total pm.nodust.5 from version with dust and sea salt removed			
          dat.files.nodust <- dat.files[grep(x = dat.files, pattern = "_NoDust_NoSalt")]
          dat.files <- dat.files[dat.files%in% dat.files.nodust==F]
          
          #define raster brick to store all years
          pm <- brick(x = raster(raster("data/inputs/pm25/GlobalGWRcwUni_PM25_GL_199801_199812-RH35_Median.nc")),nl = length(1998:2016))
          pm.nodust <- brick(x = raster(raster("data/inputs/pm25/GlobalGWRcwUni_PM25_GL_199801_199812-RH35_Median_NoDust_NoSalt.nc")),nl = length(1998:2016))
          
          #load years
          for (f in 1:length(dat.files.nodust)){
            pm[[f]] <- raster(paste("data/inputs/pm25/",dat.files[f],sep=""))
            pm.nodust[[f]] <- raster(paste("data/inputs/pm25/",dat.files.nodust[f],sep=""))
          }		
          
          #crop to africa
          pm <- raster::crop(pm, extent(africa))
          pm.nodust <- raster::crop(pm.nodust, extent(africa))
          pm_dust <- pm - pm.nodust
          pm_dust_raster <- pm_dust

          #identify cells that overlap african continent land mass
          grid <- raster(pm[[1]])
          grid[] <- 1:ncell(grid)
          
          overlap <- raster::extract(grid, africa, small = F, cellnumbers = T, weights = F)
          africa.cells <- sort(unique(unlist(lapply(overlap, function(x){x[,1]}))))
          all.cells <- 1:ncell(grid)
          not.africa.cells <- all.cells[all.cells %in% africa.cells == F]        
          
          #set cells outside africa to missing
          pm[not.africa.cells]<-NA
          pm_dust[not.africa.cells]<-NA
          
          save(pm, pm_dust, file = "data/intermediate_outputs/pm_brick_data.RData")

          
### [2] calculate share of total pm2.5 from dust ###          
        
        #get averages for dust and total pm2.5  
            pm <- as.array(pm)
            pm.nodust <- as.array(pm.nodust)
            pm_dust <- pm - pm.nodust
            pm_dust_share <- 1 - (pm.nodust/pm)
            pm_dust_ave <- apply(pm_dust, c(1,2), function(x){mean(x, na.rm = T)})
            pm_dust_share_ave <- apply(pm_dust_share, c(1,2), function(x){mean(x, na.rm = T)})
            pm_dust_ave <- raster(pm_dust_ave, template = grid)
            pm_dust_share_ave <- raster(pm_dust_share_ave, template = grid)
            pm_dust_share_ave[not.africa.cells]<-NA
            pm_ave <- apply(pm, c(1,2), function(x){mean(x, na.rm = T)})
            pm_ave <- raster(pm_ave, template = grid)
            pm_ave[not.africa.cells]<-NA

        #read in locations of DHS clusters
            dhs_loc <- read_csv("data/inputs/dhs/dhs_locations.csv")    
            
            dhs_loc$pm_cell <- cellFromXY(grid, as.matrix(dhs_loc[,c("lon","lat")])) #get pm cells for each DHS location
            dhs_loc$dust_share_i <- pm_dust_share_ave[dhs_loc$pm_cell]
            
        #fill in missings (not used for analysis just for visualization in fig 1)
            dhs_loc$dust_share_i[is.na(data$dust_share_i)] <- pm_dust_share_ave[dhs_loc$pm_cell+1][is.na(data$dust_share_i)] #move one cell over
            dhs_loc$dust_share_i[is.na(data$dust_share_i)] <- pm_dust_share_ave[dhs_loc$pm_cell-(845)][is.na(data$dust_share_i)] #move one cell down
            
    
        #identify Bodele depression dust
            bodMask <- raster(pm_dust_ave)
            bodLatLon <- read_csv("data/inputs/BodeleLocation.csv")
            bodMask[]<-0
            bodMask[cellFromXY(bodMask, as.matrix(bodLatLon[,1:2]))]<-1
            bod_cells <- data.frame(cell = 1:ncell(bodMask), bodDummy = bodMask[]) %>% filter(bodDummy==1)
        

       
        #pull dust values for Bodele depression    
            dust_bod_all <- pm_dust_raster[bod_cells$cell]
            dust_bod <- data.frame(year = 1998:2016, dust = as.numeric(apply(dust_bod_all, 2, function(x){mean(x, na.rm = T)})))
            


    #since only temporal we can calculate once for every birth month then just join in with data
        month_year <- data.frame(child_birth_year = rep(2000:2015, each=12), child_birth_month = 1:12)

        #define weights
        #year t    
        #next yr is (month birth)/12. 
        month_year$next_yr_weight_post <- month_year$child_birth_month/12
        #we don't have 2016 pm.nodust5 so 2015 births have to have 0 weights for 2016 pm. Can drop all 2015 births and/or can include then check
        
        
        #this yr is left over
        month_year$current_yr_weight_post <- 1-month_year$next_yr_weight_post
        
        #check that weights add to 1
        if(!all.equal(rep(1, nrow(month_year)), month_year$next_yr_weight_post + month_year$current_yr_weight_post)){stop("weights don't add up to 1")}
        
        #year t-1    
        month_year$last_yr_weight_pre <- (12-month_year$child_birth_month)/12
        
        #this yr is left over
        month_year$current_yr_weight_pre <- month_year$child_birth_month/12
        
        #check that weights add to 1
        if(!all.equal(rep(1, nrow(month_year)), month_year$current_yr_weight_pre + month_year$last_yr_weight_pre)){stop("weights don't add up to 1")}



    
      #pull data function
            get_dust_my <- function(row){
              month = row["child_birth_month"] %>% as.numeric()
              year = row["child_birth_year"] %>% as.numeric()
              keep1 <- as.numeric(c(year, year + 1))
              out1 <- weighted.mean(x = dust_bod[(dust_bod$year %in% keep1),"dust" ], w = row[c("current_yr_weight_post","next_yr_weight_post")], na.rm = T)
              
              keep2 <- as.numeric(c(year-1, year))
              out2 <- weighted.mean(x = dust_bod[(dust_bod$year %in% keep2),"dust" ], w = row[c("last_yr_weight_pre","current_yr_weight_pre")], na.rm = T)
              
              return(c(out1, out2))
              
            }
            
  
  # pull contemporaneous and lagged dust in bodele for each year-month
    bod_dat <- t(apply(month_year,1,function(x){get_dust_my(x)}))
    bod_dat <- month_year %>% dplyr::select(starts_with("child")) %>% mutate(dust_bod_t = bod_dat[,1], dust_bod_t_lag = bod_dat[,2])


################################# write out all of the data objects used to construct figure 1 #################################
save(pm_dust_ave, pm_ave, pm_dust_share_ave, africa,bod_dat, file = "data/intermediate_outputs/fig1_data.RData")    
################################################################################################################################
