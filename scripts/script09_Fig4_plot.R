source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")
################################################################################

#this data frame has decadal average GCM rainfal by model for relevant periods & seasons
rain_projections <- read_rds("data/inputs/rainfall_projections/GCM_rainfall_projections.rds")

##load stored bootstrap runs. 
  #If code from script 07 has been run to generate estimates those are used
  #otherwise pre-processed estimates are used. If new estimates are used figure may not look identical but should be similar regardless.
if(file.exists("data/figure_data/fig4_bootstrap_runs_new.RData")){
      load("data/figure_data/fig4_bootstrap_runs_new.RData")}else{
      load("data/figure_data/fig4_bootstrap_runs.RData")          
      }
#loaded objects include: 
            # beta_imr: 1,000 sampled betas estimated in equation (2)     
            # beta_pm: 1,000 sampled coefficients from pm_loc ~ rainfall_bod + controls + FE described in methods
            
          

  ##panel (a) distribution of rainfall projections   
  
          rain_projections$baseline <- apply(rain_projections[,c("pr.2010","pr.2000")],1,function(x){mean(x,na.rm= T)})
          rain_projections$delta_2050 <- (rain_projections$pr.2050 - rain_projections$baseline)
          rain_projections$delta_2100 <- (rain_projections$pr.2100 - rain_projections$baseline)

          rp_bod_harm <- filter(rain_projections, var=="bod_harm" ) %>% dplyr::select(model, starts_with("delta"))
          rp_bod_rainy <- filter(rain_projections, var=="bod_rainy") %>% dplyr::select(model, starts_with("delta"))

          rp_bod_harm$delta_2050[rp_bod_harm$delta_2050>15]<-15 #censor on right side to account for outliers
          
          dist_rain <- rp_bod_harm$delta_2050
          
          
  ## panel (b) rainfall and bootstrapped beta_pm   
          
          
          dist_pm <- list()
          
          for(i in 1:1000){
            
            dist_pm[[i]]<- sample(x=dist_rain, size = length(dist_rain), replace = T)*
                          sample(x = beta_pm, size = 1)
              
            
          }
          dist_pm <- dist_pm %>% unlist()
          
          
         
        
          
  ## panel (c) take all the sampled pm from rainfall and sampled imr and multiply to get imr        
            
            dist_imr<- as.matrix(sample(x=dist_pm, size = 36000, replace = T))%*%
              t(as.matrix(sample(x=beta_imr, size = 1000, replace = T)))
            dist_imr <- as.numeric(dist_imr)
          
           
          
          
          
         
    
          
  
          
          
          pdf(file = "figures/raw/Fig4_raw.pdf", width =4, height = 12)
                    
          
                    par(oma = c(5,0,3,0))
                    par(mar = c(4,6,2,2))
                    par(mfrow =c (3,1))
                    
                  
          #(a)
                    hist(dist_rain, xlim = c(-15, 15),col = add.alpha('gray25', 0.5), main = "",ylab = "", xlab="", freq=F, breaks = c(-100,seq(-15,15,2),100),  axes=F, ylim = c(0, 0.21))
                    axis(1, at = seq(-15, 15, 5),tick = T, cex.axis = 1.1)
                    axis(2, las = 2, cex.axis = 1.25, labels = seq(0, 40, 10), at= seq(0, 0.2, 0.2/4))
                    mtext(side = 2, text ="",line=3)
                    mtext(side = 1, text ="Change in seasonal rainfall total (mm)", cex=1.1,line=3.5)

          #(b)      
                    hist(dist_pm, xlim = c(-15, 15), col = add.alpha('gray50', 0.5), main = "",ylab = "", xlab="", freq=F, breaks = c(-100,seq(-15,15,2),100),  axes=F)
                    axis(1, at = seq(-15, 15, 5),tick = T, cex.axis = 1.1)
                    axis(2, las = 2, cex.axis = 1.25, at = seq(0,0.2,0.2/3), labels = seq(0,30,10))
                    mtext(side = 2, text ="",line=3)
                    mtext(side = 1, text ="Change in annual PM2.5 (deaths per 1000)", cex=1.1,line=3.5)
                    

                    
          #(c)      
                    dist_imr[dist_imr < -0.025]<- 0.025; dist_imr[dist_imr>0.025]<-0.025
                    dd <- hist(dist_imr, plot = F)$density
                    
                    hist(dist_imr*1000, xlim = c(-25, 25), col = add.alpha('gray75', 0.5), main = "",ylab = "", xlab="", freq=F, breaks = c(100,seq(-25,25,5), 100),  axes=F)
                    axis(1, at = seq(-25, 25, 5),tick = T, cex.axis = 1.1)
                    axis(2, las = 2, cex.axis = 1.25, at = seq(0, 0.08, 0.0266666), labels = seq(0, 30,10))
                    mtext(side = 2, text ="",line=3)
                    mtext(side = 1, text ="Change in annual IMR (ug/m3)", cex=1.1,line=3.5)
                    
                    
                    

          dev.off()
          
          
        
          
      