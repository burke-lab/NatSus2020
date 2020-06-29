source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

#aeronet-
#citation:    Dubovik, O., A. Sinyuk, T. Lapyonok, B. N. Holben, M. Mishchenko, P. Yang, T. F. Eck, 
#             H. Volten, O. Munoz, B. Veihelmann, W. J. van der Zande, J-F Leon, M. Sorokin, and I. Slutsker, 2006: 
#             Application of spheroid models to account for aerosol particle nonsphericity in remonte sensing of desert dust. 
#             J. Geophys. Res., 111, doi:10.1029/2005JD006619.
#aerodata data available here: https://aeronet.gsfc.nasa.gov/new_web/aerodata.html
# We thank the PIs and their staff for establishing and maintaining the sites used here.
# See methods and SI of paper for more details

#satellite derived pm2.5 aerodata-
#citation:  van Donkelaar, A., R.V Martin, M.Brauer, N. C. Hsu, R. A. Kahn, R. C Levy, A. Lyapustin, A. M. Sayer, and D. M Winker, 
#           Global Estimates of Fine Particulate Matter using a Combined Geophysical-Statistical Method with Information from Satellites, Models, and Monitors, 
#           Environ. Sci. Technol, doi: 10.1021/acs.est.5b05833, 2016. 
#pm2.5 data available here: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140#V4.GL.02   

  
#Note: when re-processing from scratch for replication code we got slightly different R2 in panel b (0.786 vs 0.788). Not sure why but takeaways the same.  
      
      #africa shapefile
      africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")

    
      #read in van Donkelaar et al pm2.5 data
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
          
          #estimate share of pm2.5 from dust
          pm_dust <- pm - pm.nodust
          dust_share <- pm_dust/pm
          
          writeRaster(dust_share, filename = "data/figure_data/figED3_dust_share.nc", format = "CDF", overwrite= T)
          
