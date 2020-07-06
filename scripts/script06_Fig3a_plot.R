source("scripts/loadPackages.R")
source("scripts/loadFunctions.R")

data <- read_rds("data/inputs/analysis_data.rds")
data_west <- filter(data, region==1)


### Generate IV and OLS estimates separately for Sub-Saharan Africa and for West Africa using rain and dust instruments

      
      #(IV - dust instrument - west africa)
            iv_fmla1 <- make_equation(
              controls <- "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights ",
              fe = "fe_cluster + fe_country_month + fe_year",
              depvar <- "child_die_age1",
              cluster_var <- "fe_cluster",
              iv <- "(pm25_post ~   instrument_dust)"
            )
            
            
            iv1 <- felm(iv_fmla1, data = data_west, weights = data_west[,"reg_wt"])
      
      
      
      #(IV - rain instrument - west africa)
            iv_fmla2 <- make_equation(
              controls <- "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights ",
              fe = "fe_cluster + fe_country_month + fe_year",
              depvar <- "child_die_age1",
              cluster_var <- "fe_cluster",
              iv <- "(pm25_post ~   instrument_rain )"
            )
            
            iv2 <- felm(iv_fmla2, data = data_west, weights = data_west[,"reg_wt"])   
      
      
      
      #(IV - dust instrument - SSA)
            iv_fmla3 <- make_equation(
              controls <- "  rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights ",
              fe = "fe_cluster + fe_country_month + fe_year",
              depvar <- "child_die_age1",
              cluster_var <- "fe_cluster",
              iv <- "(pm25_post ~   instrument_dust)"
            )
            
            
            iv3 <- felm(iv_fmla3, data = data, weights = data[,"reg_wt"])
      
      
      #(IV - rain instrument - SSA)
            iv_fmla4 <- make_equation(
              controls <- "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights ",
              fe = "fe_cluster + fe_country_month + fe_year",
              depvar <- "child_die_age1",
              cluster_var <- "fe_cluster",
              iv <- "(pm25_post ~   instrument_rain + instrument_rain_lag  )"
            )
            
            iv4 <- felm(iv_fmla4, data = data, weights = data[,"reg_wt"])   
            

      
      # OLS for SSA and west africa
            
      ols_fmla <- make_equation(
        controls <- "pm25_post+ pm25_pre + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
        fe = "fe_cluster + fe_country_month + fe_year",
        depvar <- "child_die_age1",
        cluster_var <- "fe_cluster",
        iv <- "0"
      )
      
      ols1 <- felm(ols_fmla, data = data, weights = data[,"reg_wt"])
      ols2 <- felm(ols_fmla, data = data_west, weights = data_west[,"reg_wt"])
      
    
      
  #put all results into data frame  
    iv_results <- rbind(
      summary(iv1)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(iv2)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(iv3)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(iv4)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(ols1)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(ols2)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric()) %>% as.data.frame()
    names(iv_results) <- c("coefficients","se")




    #specify plot features
    
            #add identifying information about each model
            iv_results$model <- 1:nrow(iv_results)
            iv_results$region <- c("west","west","full","full","full","west")
            iv_results$base_rate <- c(rep(mean(data_west$child_die_age1),2),rep(mean(data$child_die_age1),3),mean(data_west$child_die_age1)) 
        
            iv_results <- left_join(iv_results, data.frame(region = c("west","full"), sample.size = c(nrow(data_west), nrow(data))))
            
            
            #calculate % change imr per 10ug/m3 increase in pm2.5 with 95% CI
            iv_results$xleft <- 100*10*(iv_results$coefficients + qnorm(0.025)*iv_results$se)/iv_results$base_rate
            iv_results$xright <- 100*10*(iv_results$coefficients + qnorm(0.975)*iv_results$se)/iv_results$base_rate
            iv_results$xmid <- 100*10*(iv_results$coefficients)/iv_results$base_rate
            iv_results$col <- 'black'
            iv_results$col[iv_results$region=="west"] <- 'white'
            iv_results <- iv_results[c(1,3,2,4,6,5),]
            
              
              
              nn=nrow(iv_results):1
              xx <- seq(-20,50,10)
              lz <- c(2.5, 4.5)  #horizontal break lines to divide up the different subgroups
              
              txt1 <- c("IV-dust","","IV-rain","","OLS","")
              txt2 <- c("clust, yr, cntry-mth","",
                        "clust, yr, cntry-mth","",
                        "clust, yr, cntry-mth","")
              
                    
              
              ci <-  formatC(round(iv_results$xmid,2),digits=2,format="f")%&%" ("%&%formatC(round(iv_results$xleft,2),digits=2,format="f")%&%","%&%formatC(round(iv_results$xright,2),digits=2,format="f")%&%")" #this makes sure lenght of character string is the same

              
              
### plot fig 3a ###
      
      pdf(file="figures/raw/Fig3a_raw.pdf",width=7,height=4,useDingbats = F)
      
            par(mgp=c(2,0.8,0),mar=c(5,2,2,2),lend=1)
            
            plot(1,type="n",xlim=c(-50,70),ylim=c(1,dim(iv_results)[1]),axes=F,ylab="",xlab="change in infant mortality rate",line=3)
            
            abline(v=xx,lwd=0.5,col="grey",lty=2)
            abline(v=0,lwd=0.5)
            segments(iv_results$xleft,nn,iv_results$xright,nn)
            
            points(iv_results$xmid,nn,pch=21,bg=iv_results$col,cex=1.2)
            
            abline(h=lz,lwd=0.75,lty=1,col="grey") #with subgroup ordering
            axis(1,at=xx,labels=xx, cex.axis=0.6, line=1)
            
            text(-55,nn,txt1,cex=0.65,pos=4)
            
            text(-35,nn,format(iv_results$sample.size,big.mark = ","),pos=4,cex=0.65)  #this adds comma in number
            
            
            text(65.5,nn,ci,cex=0.65)
            
      dev.off()
      
      
      
