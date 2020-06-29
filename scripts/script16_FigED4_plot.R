source("scripts/0_loadFunctions.R")
source("scripts/0_loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")
data_west <- filter(data, region==1)
fs=list() #list for storing results from first stage models

#### run first stage regression with contemporaneous and lagged instruments for dust and rain across 6 models ###
### run each for both west africa and for full sample

# models 1-3 use dust instruments
# models 4-6 analagous models with rainfall instruments

    ### Dust instruments ###

      #(model 1)
      fs_fmla1 <- make_equation(
            controls="instrument_dust + instrument_dust_lag  + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
            fe="fe_cluster + fe_country_month + fe_year",
            depvar="pm25_post",
            cluster_var="fe_cluster",
            iv="0"
        )
        
        fs[[1]] <- felm(fs_fmla1, data = data_west, weights = data_west[,"reg_wt"])

        fs[[2]] <- felm(fs_fmla1, data = data, weights = data[,"reg_wt"])
        
        

     #(model 2)
     fs_fmla2 <- make_equation(
            controls="instrument_dust + instrument_dust_lag  + enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
            fe="fe_cluster + fe_country_month + fe_year",
            depvar="pm25_post",
            cluster_var="fe_cluster",
            iv="0"
        )
       
        fs[[3]] <- felm(fs_fmla2, data = data_west, weights = data_west[,"reg_wt"])   
    
        fs[[4]] <- felm(fs_fmla2, data = data, weights = data[,"reg_wt"])   
        

     #(model 3)
     fs_fmla3 <- make_equation(
          controls="instrument_dust + instrument_dust_lag  + enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
          fe="fe_mom + fe_country_month + fe_year",
          depvar="pm25_post",
          cluster_var="fe_cluster",
          iv="0"
        )
        
        fs[[5]] <- felm(fs_fmla3, data = data_west, weights = data_west[,"reg_wt"])
    
        fs[[6]] <- felm(fs_fmla3, data = data, weights = data[,"reg_wt"])
        
        

    ### Rainfall instruments ###
        
      #(model 4)
      fs_fmla4 <- make_equation(
          controls="instrument_rain + instrument_rain_lag  + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
          fe="fe_cluster + fe_country_month + fe_year",
          depvar="pm25_post",
          cluster_var="fe_cluster",
          iv="0"
        )
        
        fs[[7]] <- felm(fs_fmla4, data = data_west, weights = data_west[,"reg_wt"])
        
        fs[[8]] <- felm(fs_fmla4, data = data, weights = data[,"reg_wt"])
        
        
        
      #(model 2)
      fs_fmla5 <- make_equation(
          controls="instrument_rain + instrument_rain_lag  + enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
          fe="fe_cluster + fe_country_month + fe_year",
          depvar="pm25_post",
          cluster_var="fe_cluster",
          iv="0"
        )
        
        fs[[9]] <- felm(fs_fmla5, data = data_west, weights = data_west[,"reg_wt"])   
        
        fs[[10]] <- felm(fs_fmla5, data = data, weights = data[,"reg_wt"])   
        
        
      #(model 3)
      fs_fmla6 <- make_equation(
          controls="instrument_rain + instrument_rain_lag  + enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
          fe="fe_mom + fe_country_month + fe_year",
          depvar="pm25_post",
          cluster_var="fe_cluster",
          iv="0"
        )
        
        fs[[11]] <- felm(fs_fmla6, data = data_west, weights = data_west[,"reg_wt"])
        
        fs[[12]] <- felm(fs_fmla6, data = data, weights = data[,"reg_wt"])
        
     
        
    
      
      #function for pulling out point estimate and standard error from regression results
      pull_coefficient=function(x, coef){ data.frame(summary(x)$coefficients[coef,c("Estimate", "Cluster s.e.")] )}
      
      fs_results <- 
                  rbind(
                        rbindlist(lapply(fs[1:6], function(x){pull_coefficient(x, c("instrument_dust","instrument_dust_lag"))})),
                        rbindlist(lapply(fs[7:12], function(x){pull_coefficient(x, c("instrument_rain","instrument_rain_lag"))}))
                  ) %>% as.data.frame() %>% rename(coefficient = Estimate, se = Cluster.s.e.) %>% 
                  mutate(
                    lowerbound = coefficient - 3*se,
                    upperbound = coefficient + 3*se,
                    model = rep(1:6, each = 4),
                    region = rep(c("west","full"),12),
                    instrument = rep(c("dust","rain"), each = 12),
                    N = rep(c(432542, 990696),12),
                    col = rep(c("white","white","black","black"),6),
                    variable = c(rep(c("Dust Instrument","Lagged Dust Instrument"), 6),rep(c("Rain Instrument","Lagged Rain Instrument"), 6)),
                    fe =   rep(c("clust, yr, cntry-mth","",
                             "clust, yr, cntry-mth","",
                             "mom, yr, cntry-mth", "",
                             "clust, yr, cntry-mth","",
                             "clust, yr, cntry-mth","",
                             "mom, yr, cntry-mth", ""),2) ,
                    ci = paste(formatC(round(coefficient,2),digits=2,format="f")," (",formatC(round(lowerbound,2),digits=2,format="f"),",", formatC(round(upperbound,2),digits=2,format="f"),")", sep ="") 
                    
                  )
      






      nn=nrow(fs_results):1 #lines
      xx=seq(-3,2,0.5) #x axis

      
      
      
      pdf(file="figures/raw/FigED4_raw.pdf",width=8,height=5,useDingbats = F)
            
            par(mgp=c(2,0.8,0),mar=c(5,4,4,3),lend=1)
            
            plot(1,type="n",xlim=c(-11,5),ylim=c(1,dim(fs_results)[1]),axes=F,ylab="",xlab="change in PM2.5")
            
            abline(v=xx,lwd=0.5,col="grey",lty=2)
            abline(v=0,lwd=0.5)
            
            segments(fs_results$lowerbound,nn,fs_results$upperbound,nn)
            points(fs_results$coefficient,nn,pch=21,bg=fs_results$col,cex=1.2)
            
            abline(h=seq(4,24,4) +0.5,lwd=0.75,lty=1,col="grey") #with subgroup ordering
            segments(x0 = -7, x1 = 5, y0 = seq(2,22,4)+0.5, col = 'gray')
          
            axis(1,at=seq(-3,2,1), cex.axis=0.6)
            
          #add labels            
            text(-7,nn,format(fs_results$N,big.mark = ","),pos=4,cex=0.45)  #this adds comma in number
            text(-5.5, nn, fs_results$variable, pos = 4, cex=0.4)
            text(-8.5, seq(22,2,-4)+0.5, rep(c("hh + weather","hh + weather + enso","hh + weather + enso"),2), cex= 0.4)
            text(-10.5, seq(22,2,-4)+0.5, rep(c("cluster, yr, \n country-mth","cluster, yr, \n country-mth","mother, yr, \n country-mth"),2), cex= 0.4)
            text(x = 4, nn, fs_results$ci,cex=0.4)
      
      dev.off()
      




