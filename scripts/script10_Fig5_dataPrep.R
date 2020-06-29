source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

dat <- read_csv("data/inputs/exotic_proposal_parameters/parameters.csv") %>% filter(parameter %in% c("Unit cost for Borewell", "Unit cost for PV pumping system","Unit cost for pivot distribution system")==F)

  #separate parameters based on whether they're fixed or not and write functions to pull out fixed or sampled parameters
    par_fixed <- dat %>% filter(is.na(lb) & is.na(se)) %>% dplyr::select(parameter, est, lb, ub, se)
    par_uncrt <- dat%>% filter(!is.na(lb) | !is.na(se)) %>% dplyr::select(parameter, est, lb, ub, se) 

#functions for pulling fixed parameters or sampling parameters with uncertainty ranges
  fixedPar <- function(x){filter(par_fixed, parameter==x) %>% dplyr::select(est) %>% as.numeric()}
    unifSamplePar <- function(x){runif(n = 1, min = par_uncrt$lb[par_uncrt$parameter==x], max = par_uncrt$ub[par_uncrt$parameter==x])}
    normSamplePar <- function(x){rnorm(n = 1,mean = as.numeric(par_uncrt$est[par_uncrt$parameter==x]), sd = par_uncrt$se[par_uncrt$parameter==x])}
    unifSampleParMin <- function(x){par_uncrt$lb[par_uncrt$parameter==x]}
    unifSampleParMax <- function(x){par_uncrt$ub[par_uncrt$parameter==x]}
    normSampleParMin <- function(x){ as.numeric(par_uncrt$est[par_uncrt$parameter==x])-2*par_uncrt$se[par_uncrt$parameter==x]}
    normSampleParMax <- function(x){ as.numeric(par_uncrt$est[par_uncrt$parameter==x])+2*par_uncrt$se[par_uncrt$parameter==x]}

########################################################
    #each list item is a different parameter  
    unct <- list() #store uncertainties for each parameter
    unct[[1]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSampleParMin("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[1]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSampleParMax("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[1]][i,2]<-dollar_per_averted_YLL
  
  
  
}
print("done with 1")
unct[[2]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSampleParMin("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[2]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSampleParMax("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[2]][i,2]<-dollar_per_averted_YLL
  
  
  
}

print("done with 2")

unct[[3]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSampleParMin("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[3]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSampleParMax("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[3]][i,2]<-dollar_per_averted_YLL
  
  
  
}

print("done with 3")


unct[[4]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSampleParMin("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[4]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSampleParMax("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[4]][i,2]<-dollar_per_averted_YLL
  
  
  
}

print("done with 4")

unct[[5]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSampleParMin("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[5]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSampleParMax("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[5]][i,2]<-dollar_per_averted_YLL
  
  
  
}
print("done with 5")

unct[[6]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSampleParMin("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[6]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSampleParMax("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[6]][i,2]<-dollar_per_averted_YLL
  
  
  
}


print("done with 6")

unct[[7]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSampleParMin("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[7]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSampleParMax("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[7]][i,2]<-dollar_per_averted_YLL
  
  
  
}


print("done with 7")

unct[[8]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSampleParMax("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[8]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSampleParMin("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[8]][i,2]<-dollar_per_averted_YLL
  
  
  
}


print("done with 8")
unct[[9]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSampleParMax("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[9]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSampleParMin("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSamplePar("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[9]][i,2]<-dollar_per_averted_YLL
  
  
  
}

print("done with 9")
unct[[10]] <- matrix(nrow = 1000, ncol = 2)

for(i in 1:1000){
  
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSampleParMax("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[10]][i,1]<-dollar_per_averted_YLL
  
  #get costs of system
  unit_cost_borewell <- unifSamplePar("Cost per m")*fixedPar("Average Depth to Groundwater")
  unit_cost_pv_pump <- unifSamplePar("Cost per Wp PV pump system and BOS")*unifSamplePar("PV power capacity needed for pump")*1000
  unit_cost_pivot_dist <- unifSamplePar("Distribution System Cost") +  ( 1000*unifSamplePar("Power needed to run pivot")*unifSamplePar("Cost per Wp PV system alone"))/fixedPar("Number of pivots sharing power source")
  ####
  tot_unit_cost_annual <-  unit_cost_borewell/fixedPar("Borewell lifetime")  +  (unit_cost_pv_pump + unit_cost_pivot_dist)/unifSamplePar("System Lifetime for PV Pump and Pivot Systems")            
  
  #get rainfall -> deaths
  dIMR_dPM <- normSamplePar("% Change IMR per ug/m3 change in PM2.5")
  dPMloc_dRainBod <- normSamplePar("PM change in W. Africa per mm in Bodele")
  imr_base <- fixedPar("Baseline Mortality Rate for West Africa")
  births <- fixedPar("Births per Year")  
  
  #valuing deaths
  LYlost_per_death <- unifSampleParMin("years of life lost per death")
  
  
  
  ########  OUTPUTS ##########
  frac_area_covered <- fixedPar("Fraction of area covered") 
  number_units_needed <- frac_area_covered*fixedPar("Active Bodele Area (Washington)")/fixedPar("Unit Area")
  tot_annualized_cost <- number_units_needed*tot_unit_cost_annual
  tot_water_over_dry_season <- min(c(2, 180/fixedPar("Days per mm water")))
  ave_pm_reduction_waf <- tot_water_over_dry_season*dPMloc_dRainBod
  averted_death_per_year <-ave_pm_reduction_waf*dIMR_dPM*imr_base*births/100000
  dollar_per_averted_death<- tot_annualized_cost/averted_death_per_year
  
  
  dollar_per_averted_YLL <- dollar_per_averted_death/LYlost_per_death
  
  
  unct[[10]][i,2]<-dollar_per_averted_YLL
  
  
  
}

print("done with 10")


write_rds(unct, path = "data/figure_data/fig5_sampled_uncertainty.rds")

