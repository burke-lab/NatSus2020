source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")
data_west <- filter(data, region==1)

 
#(panel a)
    #(top)  
    fs_fmla1 <- make_equation( #function helps consruct equations. Defined in 0_loadFunctions.R
          controls = "instrument_dust + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
          fe = "fe_cluster + fe_country_month + fe_year",
          depvar = "pm25_post",
          cluster_var = "fe_cluster",
          iv = "0"
      )
      
      rf1 <- felm(fs_fmla1, data = data_west, weights = data_west[,"reg_wt"])
      rf2 <- felm(fs_fmla1, data = data, weights = data[,"reg_wt"])
  
    #(bottom)
    fs_fmla2 <- make_equation(
        controls <- "instrument_rain   + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights",
        fe <- "fe_cluster + fe_country_month + fe_year",
        depvar <- "pm25_post",
        cluster_var <- "fe_cluster",
        rf <- "0"
      )
      
      rf3 <- felm(fs_fmla2, data = data_west, weights = data_west[,"reg_wt"])   
      rf4 <- felm(fs_fmla2, data = data, weights = data[,"reg_wt"])   


    fs_results <- rbind(
      summary(rf1)$coefficients["instrument_dust",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(rf2)$coefficients["instrument_dust",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(rf3)$coefficients["instrument_rain",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
      summary(rf4)$coefficients["instrument_rain",c("Estimate", "Cluster s.e.")] %>% as.numeric()) %>% as.data.frame()
    
    names(fs_results) <- c("coefficient","se")



fs_results$model <- 1:nrow(fs_results)
fs_results$type <- c("west","full","west","full")
fs_results$mean <- mean(data$child_die_age1)
fs_results <- left_join(fs_results, data.frame(type = c("west","full"), sample.size = c(nrow(data_west), nrow(data))))


fs_results$xleft <- (fs_results$coefficient + -3.25*fs_results$se)
fs_results$xright <- (fs_results$coefficient + 3.25*fs_results$se)
fs_results$col <- c("white","black","white","black")



txt1 <- c("dust","","rain","")
txt2 <- c("clust, yr, cntry-mth","",
          "clust, yr, cntry-mth","")


ci <-  paste(round(fs_results$coefficient,2)," (",round(fs_results$xleft,2), ",",round(fs_results$xright,2),")", sep= "")
  



pdf(file="figures/raw/Fig2a_raw.pdf",width=7,height=3.5,useDingbats = F)

par(mgp=c(2,0.8,0),mar=c(5,4,4,3),lend=1)


nn=nrow(fs_results):1
xx <- seq(-2,2,0.5)
lz <- c(2, 4) +0.5 #horizontal break lines to divide up the different subgroups

plot(1,type="n",xlim=c(-3.75,3.5),ylim=c(0.9,dim(fs_results)[1]+0.4),axes=F,ylab="",xlab="")
mtext(side = 1, text = "Delta PM2.5 Concentration (ug/m3)",line=3)
abline(v=xx,lwd=0.5,col="grey",lty=2)
abline(v=0,lwd=0.5)
segments(fs_results$xleft,nn,fs_results$xright,nn)
points(fs_results$coefficient,nn,pch=21,bg=fs_results$col,cex=1.2)

abline(h=lz,lwd=0.75,lty=1,col="grey") #with subgroup ordering
axis(1,at=seq(-2,2,1),labels=seq(-2,2,1), cex.axis=1,line=1)



text(x= -3, y = c(1.5,3.5), labels = c("Rainfall Instrument","Dust Instrument"), cex=0.9)


text(2.9,nn,ci,cex=0.65)

dev.off()
