source("scripts/loadPackages.R")
source("scripts/loadFunctions.R")


## PM2.5 
        #Data download: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140#V4.GL.02
        #Accompanying paper: https://pubs.acs.org/doi/abs/10.1021/acs.est.5b05833?journalCode=esthag  

            for (y in 1998:2016){
              #total pm2.5  
              url <- paste("http://fizz.phys.dal.ca/~atmos/datasets/EST2016/GlobalGWRcwUni_PM25_GL_",y,"01_",y,"12-RH35_Median.nc", sep ="")
              dest <- paste("data/inputs/pm25/GlobalGWRcwUni_PM25_GL_",y,"01_",y,"12-RH35_Median.nc", sep ="")                    
              download.file(url = url, destfile = dest)
              
              #pm2.5 with dust and sea salt removed
              url <- paste("http://fizz.phys.dal.ca/~atmos/datasets/EST2016/GlobalGWRcwUni_PM25_GL_",y,"01_",y,"12-RH35_Median_NoDust_NoSalt.nc", sep ="")
              dest <- paste("data/inputs/pm25/GlobalGWRcwUni_PM25_GL_",y,"01_",y,"12-RH35_Median_NoDust_NoSalt.nc", sep ="")                    
              download.file(url = url, destfile = dest)
                                }

    
        
        
        ## Rainfall
        #Data download: https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_monthly/tifs/
        #Accompanying paper: https://www.nature.com/articles/sdata201566    
        
                for (y in 2000:2016){
                    for(m in c(paste0("0",1:9),10:12)){
                        url <- paste("https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_monthly/tifs/chirps-v2.0.",y,".",m,".tif.gz", sep  = "")
                        dest <- paste("data/inputs/chirps/Chirps_",y,"_",m,".tif.gz", sep ="")                    
                        download.file(url = url, destfile = dest)
                        gunzip(dest) #unzip tif files
                    }
                }
                
                
        ## DAOD        
        #Data download: https://doi.pangaea.de/10.1594/PANGAEA.909140?format=html#download
        #Accompanying paper: https://journals.ametsoc.org/doi/full/10.1175/JAMC-D-19-0194.1   
               
                 for (y in 2001:2016){
                    url <-  paste("http://hs.pangaea.de/model/VossK-EvanA_2019/DAOD_MODIS_",y,".nc", sep = "")
                    dest <- paste("data/inputs/daod/DAOD_MODIS_",y,".nc", sep ="")                    
                    download.file(url = url, destfile = dest)
                                    }
                
              
## Birth outcomes
        #*** We do not have the right to post full DHS data here so we provide an anonymized version of the data for replication as well as
        #    instructions for constructing our sample from the raw DHS data. ***
        
        # Constructing infant mortality outcomes from raw DHS data
            
            # [1]   read in list of surveys used
                    svy_list <- read_csv("data/inputs/dhs/survey_list.csv")
            # [2]   register for access to all surveys here: https://dhsprogram.com/data/new-user-registration.cfm
            # [3]   Once access is approved, download raw data for all surveys here: https://dhsprogram.com/data/available-datasets.cfm
            # [4]   using the individual recode (**IR***.dta) files, merge in gis data and reshape the data to have 1 row for each child (native format is 1 row for each individual in hh)        
            #       The sample used in this analysis can be reconstructed by including all births ocurring from 2000-2015 in the listed surveys and dropping children alive for less than 12 months at time of survey.
            #       Any clarification questions on sample construction for replication (or general DHS data questions) can be directed to Sam Heft-Neal (sheftneal@stanford.edu).
                    
    # For the purpose of processing the above data to DHS locations we have included a list of all cluster locations from our sample, however, 
    # we do not provide any information that can be used to link the locations to the anonymized DHS data.
    
    # dhs locations:
                dhs_loc <- read_csv("data/dhs/dhs_locations.csv")    
                    
                    
        