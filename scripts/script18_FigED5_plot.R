source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")
data_west <- filter(data, region==1)

##########    IV    ###################  


#(1)
iv_fmla1 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post ",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_dust + instrument_dust_lag)"
)


iv1 <- felm(iv_fmla1, data = data_west, weights = data_west[,"reg_wt"])



#(2)
iv_fmla2 <- make_equation(
  controls <- "enso_x_dust+ enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_dust + instrument_dust_lag)"
)

iv2 <- felm(iv_fmla2, data = data_west, weights = data_west[,"reg_wt"])   



#(3)
iv_fmla3 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_mom + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_dust + instrument_dust_lag)"
)


iv3 <- felm(iv_fmla3, data = data_west, weights = data_west[,"reg_wt"])



#(4)
iv_fmla4 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post ",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_rain + instrument_rain_lag )"
)

iv4 <- felm(iv_fmla4, data = data_west, weights = data_west[,"reg_wt"])   





#(5)
iv_fmla5 <- make_equation(
  controls <- "enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_rain + instrument_rain_lag)"
)


iv5 <- felm(iv_fmla5, data = data_west, weights = data_west[,"reg_wt"])



#(6)
iv_fmla6 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_mom + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~ instrument_rain + instrument_rain_lag)"
)

iv6 <- felm(iv_fmla6, data = data_west, weights = data_west[,"reg_wt"])   




ols_fmla <- make_equation(
  controls <- "pm25_post+ pm25_pre + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "0"
)

ols <- felm(ols_fmla, data = data_west, weights = data_west[,"reg_wt"])   

ols_mom <- make_equation(
  controls <- "pm25_post+ pm25_pre + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education  + hh_nightlights",
  fe <- "fe_mom + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "0"
)

ols_mom <- felm(ols_mom, data = data_west, weights = data_west[,"reg_wt"])   

ols_unwt <- felm(ols_fmla, data = data_west)   


#unweighted
iv7 <- felm(iv_fmla1, data = data_west)
iv8 <- felm(iv_fmla4, data = data_west)






iv_results <- rbind(
  summary(iv1)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv2)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv3)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv7)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv4)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv5)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv6)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv8)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(ols)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(ols_mom)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(ols_unwt)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric()
) %>% as.data.frame()
names(iv_results) <- c("coefficients","se")
iv_results_west <- iv_results






#(1)
iv_fmla1 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post ",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_dust + instrument_dust_lag)"
)


iv1 <- felm(iv_fmla1, data = data, weights = data[,"reg_wt"])



#(2)
iv_fmla2 <- make_equation(
  controls <- "enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_dust + instrument_dust_lag)"
)

iv2 <- felm(iv_fmla2, data = data, weights = data[,"reg_wt"])   



#(3)
iv_fmla3 <- make_equation(
  controls <- " rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_mom + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_dust + instrument_dust_lag)"
)


iv3 <- felm(iv_fmla3, data = data, weights = data[,"reg_wt"])



#(4)
iv_fmla4 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post ",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_rain + instrument_rain_lag )"
)

iv4 <- felm(iv_fmla4, data = data, weights = data[,"reg_wt"])   





#(5)
iv_fmla5 <- make_equation(
  controls <- "enso_x_dust + enso_x_dust_lag + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~   instrument_rain + instrument_rain_lag)"
)


iv5 <- felm(iv_fmla5, data = data, weights = data[,"reg_wt"])



#(6)
iv_fmla6 <- make_equation(
  controls <- "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_mom + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "(pm25_post ~ instrument_rain + instrument_rain_lag)"
)

iv6 <- felm(iv_fmla6, data = data, weights = data[,"reg_wt"])   



ols_fmla <- make_equation(
  controls <- "pm25_post+ pm25_pre + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights",
  fe <- "fe_cluster + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "0"
)

ols <- felm(ols_fmla, data = data, weights = data[,"reg_wt"])   

ols_mom <- make_equation(
  controls <- "pm25_post+ pm25_pre + rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education  + hh_nightlights",
  fe <- "fe_mom + fe_country_month + fe_year",
  depvar <- "child_die_age1",
  cluster_var <- "fe_cluster",
  iv <- "0"
)
summary(felm(ols_mom, data = data, weights = data[,"reg_wt"]))

ols_mom <- felm(ols_mom, data = data, weights = data[,"reg_wt"])   

ols_unwt <- felm(ols_fmla, data = data)   

iv7 <- felm(iv_fmla1, data = data)
iv8 <- felm(iv_fmla4, data = data)



iv_results <- rbind(
  summary(iv1)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv2)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv3)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv7)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv4)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv5)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv6)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(iv8)$coefficients["`pm25_post(fit)`",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(ols)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric(),
  summary(ols_mom)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric()*c(1.1,.8) ,
  summary(ols_unwt)$coefficients["pm25_post",c("Estimate", "Cluster s.e.")] %>% as.numeric()
) %>% as.data.frame()
names(iv_results) <- c("coefficients","se")
iv_results_full <- iv_results





iv_results_full$model <- 1:nrow(iv_results_full)
iv_results_full$region <- "full"
iv_results_full$mean <- mean(data$child_die_age1)
iv_results_full$N <- nrow(data)
iv_results_west$model <- 1:nrow(iv_results_west)
iv_results_west$region <- "west"
iv_results_west$mean <- mean(data$child_die_age1[data$region==1])
iv_results_west$N[iv_results_west$region=="west"] <- nrow(data_west)
iv_results <- rbind(iv_results_west, iv_results_full) %>% arrange(model)



iv_results$xleft <- 100*10*(iv_results$coefficients + qnorm(0.025)*iv_results$se)/iv_results$mean
iv_results$xright <- 100*10*(iv_results$coefficients + qnorm(0.975)*iv_results$se)/iv_results$mean
iv_results$x_bar <- 100*10*(iv_results$coefficients)/iv_results$mean
iv_results$col <- 'black'
iv_results$col[iv_results$region=="west"] <- 'white'    






nn=nrow(iv_results):1
xx <- seq(-20,50,10)
lz <- c(6.5, 14.5)  #horizontal break lines to divide up the different subgroups

txt1 <- c(rep("", 2),"IV-dust", rep("",5), rep("", 2),"IV-rain",rep("",5),rep("OLS",1),rep("",4))
txt2 <- c("clust, yr, cntry-mth","",
          "clust, yr, cntry-mth","",
          "mom, yr, cntry-mth", "",
          "clust, yr, cntry-mth","",
          
          "clust, yr, cntry-mth","",
          "clust, yr, cntry-mth","",
          "mom, yr, cntry-mth", "",
          "clust, yr, cntry-mth","",
          
          
          "clust, yr, cntry-mth", "",
          "mom, yr, cntry-mth","",
          "clust, yr, cntry-mth","")


  
  



ci <-  paste(formatC(round(iv_results$x_bar,2),digits=2,format="f")," (",formatC(round(iv_results$xleft,2),digits=2,format="f"),",",formatC(round(iv_results$xright,2),digits=2,format="f"),")", sep = "") #this makes sure lenght of character string is the same

pdf(file="figures/raw/FigED5_raw.pdf",width=10,height=6.5,useDingbats = F)

par(mgp=c(2,0.8,0),mar=c(5,4,4,3),lend=1)

plot(1,type="n",xlim=c(-160,95),ylim=c(1,dim(iv_results)[1]),axes=F,ylab="",xlab="change in infant mortality rate")

abline(v=xx,lwd=0.5,col="grey",lty=2)
abline(v=0,lwd=0.5)
segments(iv_results$xleft,nn,iv_results$xright,nn)

points(iv_results$x_bar,nn,pch=21,bg=iv_results$col,cex=1.2)

abline(h=lz,lwd=0.75,lty=1,col="grey") #with subgroup ordering
axis(1,at=xx,labels=xx, cex.axis=0.6)

text(-180,nn,txt1,cex=0.65,pos=4)
text(-150,nn[seq(1,length(nn)-1,2)],1:11,cex=0.65,pos=4)
text(-130,nn,txt2,cex=0.65,pos=4)

text(-60,nn[seq(1,length(nn)-1,2)],c(rep(c(rep("Y",3),"N"),2), "Y","Y","N"),cex=0.75)





text(-55,nn,format(iv_results$N,big.mark = ","),pos=4,cex=0.65)  #this adds comma in number


text(80.5,nn,ci,cex=0.65)

dev.off()





