source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

################################################################################
####### paper calculations ############
################################################################################



mean(unique(data$rain_bodele_harmatan)) #mean is 43mm
sd(unique(data$rain_bodele_harmatan)) #sd is 6


summary(felm(pm25_local ~ rain_bodele_rainyseason + rain_bodele_harmatan + rain_local_post   | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))

#effect on pm is -1.3099381

#so 1 sd increase in rainfall -> 7.8 unit decrease in pm



summary(felm(child_die_age1 ~ pm25_local + rain_local_pre + rain_local_post  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))


4.368e-04 #change in infant mortality


summary(felm(child_die_age1 ~ rain_bodele_harmatan+ rain_bodele_rainyseason+rain_local_pre + rain_local_post  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))


-7.8 #unit reduction in pm should lead to 

-5.618e-04*-7.8




################################################################################




#(1) #dy/dr_bod = dy/dPM_loc * dPM_loc/dPM_bod * dPM_bod/dr_bod

#dy/dr_bod
summary(felm(child_die_age1 ~ rain_bodele_harmatan+ rain_bodele_rainyseason+rain_local_pre + rain_local_post  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
-5.618e-04


#dy/dPM_loc 
summary(felm(child_die_age1 ~ pm25_local +rain_local_pre + rain_local_post  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
4.368e-04

#dPM_loc/dPM_bod
summary(felm(pm25_local ~ dust_dsa_t  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
0.8567666


#dPM_bod/dr_bod
summary(felm(dust_dsa_t ~ rain_bodele_harmatan + rain_bodele_rainyseason | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
-1.1740472

# dy/dPM_loc * dPM_loc/dPM_bod * dPM_bod/dr_bod = 
4.368e-04* 0.8567666*-1.1740472

(4.368e-04* 0.8567666*-1.1740472)/-5.618e-04


#(2) #dy/dr_bod = dy/dPM_loc * dPM_loc/dr_bod

#dy/dr_bod
summary(felm(child_die_age1 ~ rain_bodele_harmatan+ rain_bodele_rainyseason+rain_local_pre + rain_local_post  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
-5.618e-04


#dy/dPM_loc 
summary(felm(child_die_age1 ~ pm25_local +rain_local_pre + rain_local_post  | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
4.368e-04

#dPM_loc/dr_bod
summary(felm(pm25_local ~ rain_bodele_harmatan + rain_bodele_rainyseason + rain_local_pre + rain_local_post | fe_loc + fe_season + fe_time | 0 | fe_loc, data = filter(data, region_west == 1), weights = filter(data, region_west==1)[,"reg_wt"]))
-1.202851 

# dy/dPM_loc * dPM_loc/dr_bod = 
4.368e-04*0.79774

(4.368e-04*-1.202851)/-5.618e-04    



### For 1 sd increase in bodel rainfall: 


mean(data$rain_bodele_harmatan)*0.1

-5.618e-04*(mean(data$rain_bodele_harmatan)*0.1)/mean(data$child_die_age1)






