source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")


  #[panel a]

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

####### Calculations ##########   
y <- matrix(nrow = 1000, ncol = 17)
colnames(y)<-c("unit_cost_borewell","unit_cost_pv_pump","unit_cost_pivot_dist","tot_unit_cost_annual","dIMR_dPM","dPMloc_dRainBod","imr_base","births","LYlost_per_death","frac_area_covered","number_units_needed","tot_annualized_cost","tot_water_over_dry_season","ave_pm_reduction_waf","averted_death_per_year","dollar_per_averted_death","dollar_per_averted_YLL")  


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
  
  y[i,] <- as.numeric(c(unit_cost_borewell,unit_cost_pv_pump,unit_cost_pivot_dist,tot_unit_cost_annual,dIMR_dPM,dPMloc_dRainBod,imr_base,births,LYlost_per_death,frac_area_covered,number_units_needed,tot_annualized_cost,tot_water_over_dry_season,ave_pm_reduction_waf,averted_death_per_year,dollar_per_averted_death,dollar_per_averted_YLL))
  
} 
y <- as.data.frame(y)   
vals <- y$dollar_per_averted_YLL

    
      pdf("figures/raw/Fig5a_raw.pdf", width = 6, height = 5)
            par(mar = c(5,5,2,3))
            hist(vals,breaks=20, xlab = "", axes = F, ylab = "",main = "", col = 'gray80', xlim = c(0,100), ylim = c(0,250))
            axis(1, at = seq(0,100,20),cex.axis = 1.25)
            axis(2, tick = T, las = 2, at = seq(0,250,50), labels =  seq(0,250,50)/1000,cex.axis = 1.25)
            mtext(side = 1, text = "$ per averted year of life lost",cex=1.5, line=3)
            mtext(side = 2, text = "share of estimates",cex=1.5, line=3.5)
            segments(x0 = median(vals), y0 = 0, y1 = 250, col = add.alpha('red',0.5), lwd =2)
      dev.off()



  
  #[panel b]  load uncertainties calculated in script 10 (or else pre-processed data) for panel b
  
      if(file.exists("data/figure_data/fig5_sampled_uncertainty_new.rds")){
        uncertainties <- read_rds("data/figure_data/fig5_sampled_uncertainty_new.rds")
        }else{uncertainties <- read_rds("data/figure_data/fig5_sampled_uncertainty.rds")}

          
          res <- data.frame(min = 
                              unlist(lapply(uncertainties,function(x){median(x[,1])})),
                            max = unlist(lapply(uncertainties,function(x){median(x[,2])}))
                              )
          res$diff <- res$max-res$min
          
          
          res$c1 <- apply(res, 1, function(x){min(c(x["min"], x["max"]))})
          res$c2 <- apply(res, 1, function(x){max(c(x["min"], x["max"]))})
          
          names(uncertainties)<-par_uncrt$parameter
          res <- data.frame(par = names(uncertainties), min = res$c1, max = res$c2)
          res$diff <- res$max-res$min
          res <- arrange(res, diff)



pdf("figures/raw/Fig5b_raw.pdf", width = 12, height = 4)

plot(rep(33,10),10:1, xlim = c(-300, 100),axes=F, xlab = "",ylab = "", pch = 16, col = NA)
segments(x0 = seq(0,100,10), y0 = 0, y1 = 12, col = 'gray90', lwd = 0.75)
rect(xleft = res$min, xright = res$max,ybottom = 1:10-.15, ytop = 1:10+.15, col = 'gray90', border = 'black')


axis(1, tick = T, at = seq(0,100,20), cex.axis=.7)
text(x = -150, y = 1:10,labels = as.character(res$par), cex = 0.75)
segments(x0 = 23.5,y0 = 1, y1 = 11, col = add.alpha('red',0.5),lwd = 1)


dev.off()
