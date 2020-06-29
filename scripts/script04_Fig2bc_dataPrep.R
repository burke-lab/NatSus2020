source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")
data_west <- filter(data, region==1)

africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")
#############################################################################################
# steps:
#       (1) run B iterations randomly sampling (i) dust shares (ii) bodele dust months
#       (2) for each iteration re-calculate dust instrument
#       (3) estimate beta for simulated dust instrument and store
#       (4) plot
#       (5) repeat steps 1-4 with rainfall instrument
#############################################################################################

B <- 1000
beta_dust_share <- beta_dust_timing <- rep(NA, B)

for(i in 1:B){
  
  #randomize DHS cluster dust shares
  #randomly re-assign dust shares across locations in our data  for the spatial veriation in the instrument
  data$dust_share_perm  <- data$dust_share[order(runif(n = nrow(data)))]
  
  
  #   #re-construct the instrument with the new spatial and temporal components of our instrument
  data[,"instrument_dust_perm"] <-  data[,"dust_bod_post"]*data[,"dust_share_perm"]
  
  #   #estimate model with simulated instruments
  mod_fs <-felm(pm25_post ~ instrument_dust_perm + rain_post + rain_pre + tmp_pre + 
                  tmp_post + child_female + child_birth_order + child_multi_birth + 
                  poly(mother_age_at_birth, 2, raw = T) + hh_nightlights | 
                  fe_cluster + fe_country_month + fe_year, data = data, weights = data[,"reg_wt"])
  
  beta_dust_share[i] <-as.numeric(summary(mod_fs)$coefficients["instrument_dust_perm","Estimate"])
  
  #drop just to make 100% sure we're getting a new iteration every time
  data <- data %>%  dplyr::select(-instrument_dust_perm)
  
  
  
  #randomize bodele dust months   
  #randomly re-sort order of bodele dust observations
  data$dust_bod_perm <- data$dust_bod_post[order(runif(n = nrow(data)))]
  
  #re-construct the instrument with the new spatial and temporal components of our instrument
  data[,"instrument_dust_perm"] <-  data[,"dust_bod_perm"]*data[,"dust_share"]
  
  #estimate model with simulated instruments
  mod_fs <-felm(pm25_post ~ instrument_dust_perm + rain_post + rain_pre + tmp_pre + 
                  tmp_post + child_female + child_birth_order + child_multi_birth + 
                  poly(mother_age_at_birth, 2, raw = T) + hh_nightlights | 
                  fe_cluster + fe_country_month + fe_year, data = data, weights = data[,"reg_wt"])
  
  beta_dust_timing[i] <-as.numeric(summary(mod_fs)$coefficients["instrument_dust_perm","Estimate"])
  
  #drop just to make 100% sure we're getting a new iteration every time
  data <- data %>% dplyr::select(-dust_bod_perm)
  
  
  #monitor progress
  if(i/10 == round(i/10)){print(i)}
  
}

#save coefficients
save(beta_dust_share, beta_dust_timing, file = "data/intermediate_outputs/permutation_test_coefs_dust_new.RData")



#############################################################################################
### Now same thing for rainfall instrument
#############################################################################################

B <- 1000
beta_dust_share_rain <- beta_rain_timing <- rep(NA, B)

for(i in 1:B){
  
  #randomize DHS cluster dust shares
  #randomly re-assign dust shares across locations in our data  for the spatial veriation in the instrument
  data$dust_share_perm  <- data$dust_share[order(runif(n = nrow(data)))]
  
  
  #   #re-construct the instrument with the new spatial and temporal components of our instrument
  data[,"instrument_rain_perm"] <-  data[,"rain_bod_post"]*data[,"dust_share_perm"]
  
  #   #estimate model with simulated instruments
  mod_fs <-felm(pm25_post ~ instrument_rain_perm + rain_post + rain_pre + tmp_pre + 
                  tmp_post + child_female + child_birth_order + child_multi_birth + 
                  poly(mother_age_at_birth, 2, raw = T) + hh_nightlights | 
                  fe_cluster + fe_country_month + fe_year, data = data, weights = data[,"reg_wt"])
  
  beta_dust_share_rain[i] <-as.numeric(summary(mod_fs)$coefficients["instrument_rain_perm","Estimate"])
  
  #drop just to make 100% sure we're getting a new iteration every time
  data <- data %>%  dplyr::select(-instrument_rain_perm)
  
  
  
  #randomize bodele dust months   
  #randomly re-sort order of bodele dust observations
  data$dust_bod_perm <- data$rain_bod_post[order(runif(n = nrow(data)))]
  
  #re-construct the instrument with the new spatial and temporal components of our instrument
  data[,"instrument_rain_perm"] <-  data[,"rain_bod_perm"]*data[,"dust_share"]
  
  #estimate model with simulated instruments
  mod_fs <-felm(pm25_post ~ instrument_rain_perm + rain_post + rain_pre + tmp_pre + 
                  tmp_post + child_female + child_birth_order + child_multi_birth + 
                  poly(mother_age_at_birth, 2, raw = T) + hh_nightlights | 
                  fe_cluster + fe_country_month + fe_year, data = data, weights = data[,"reg_wt"])
  
  beta_rain_timing[i] <-as.numeric(summary(mod_fs)$coefficients["instrument_rain_perm","Estimate"])
  
  #drop just to make 100% sure we're getting a new iteration every time
  data <- data %>% dplyr::select(-dust_bod_perm)
  
  
  #monitor progress
  if(i/10 == round(i/10)){print(i)}
  
}

#save coefficients
save(beta_dust_share_rain, beta_rain_timing, file = "data/intermediate_outputs/permutation_test_coefs_rain_new.RData")

