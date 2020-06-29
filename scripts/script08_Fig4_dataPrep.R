source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

  data_west <- read_rds("data/inputs/analysis_data.rds") %>% filter(region==1)                                                      
  celldata <- read_rds("data/figure_data/fig4_cell_regression_data.rds")                                 
  # cell level data is used for for estimating the relationship between rainfall and pm2.5               #
  # we use cell level data for this regression because if we use the main data set                       #
  # then the results are implicitly weighted by the number of births in each location and we want to     #
  # estimate the impact of rainfall on pm2.5 in our locations as directly as possible.                   #


  
  #now run 1000 boostraps estimating both
      #(1) main regression for west africa (ie effect of dust pm on imr)
      #(2) local pm2.5 ~ bodele rainfall annual cell level regression

#(1) same regression as 3rd row of fig 3   
eqn_imr <- make_equation(
                      controls = "rain_post +rain_pre + tmp_pre + tmp_post + child_female + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T)  + hh_nightlights ",
                      fe = "fe_cluster + fe_country_month + fe_year",
                      depvar = "child_die_age1",
                      cluster_var = "", # no need to cluster while bootstrapping since we're not using estimated errors
                      iv = "(pm25_post ~   instrument_rain )"
                        )

#(2) rainfall regression. 
# annual so can't include monthly FE and many fewer obs if we include country x year FE so we include cell FE and flexible country specific time trends
eqn_rain <- make_equation(
                      depvar = "pm_loc",
                      controls = "rain_loc + rain_bod + poly(ctt1,4) + poly(ctt2,4) + poly(ctt3,4) + poly(ctt4,4) + poly(ctt5,4) + poly(ctt6,4) + poly(ctt7,4) + poly(ctt8,4) + poly(ctt9,4) + poly(ctt10,4)", 
                      fe ="cell",
                      cluster_var = "0",
                      iv ="0"
              )
  
 


B <- 1000
beta_pm <- rep(NA, B)
beta_imr  <- rep(NA, B)

#this takes awhile. stored results can also be loaded and this step can be skipped.
for(b in 1:B){    
  
  celldata_sub <- celldata[sample(x = 1:nrow(celldata),replace = T),]
  data_west_sub <- data_west[sample(x = 1:nrow(data_west), replace = T),]
  
  beta_pm[b]  <- as.numeric(summary(felm(eqn_rain, data = celldata_sub, weights = celldata_sub$pop_wt))$coefficients["rain_bod","Estimate"])
  
  beta_imr[b] <- as.numeric(summary(felm(eqn_imr, data = data_west_sub, weights = data_west_sub[,"reg_wt"]))$coefficients["`pm25_post(fit)`","Estimate"])
  
  print(b)
}

save(beta_pm, beta_imr, file = "data/figure_data/fig4_bootstrap_runs_new.RData")
    


