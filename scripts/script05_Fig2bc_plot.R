source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

data <- read_rds("data/inputs/analysis_data.rds")
data_west <- filter(data, region==1)

africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")

#############################################################################################


#load coefficients for plotting. If script 04 was run load that output. otherwise load pre-processed data. 
if(file.exists("data/figure_data/fig2_permutation_test_coefs_dust_new.RData")){
  load("data/figure_data/fig2_permutation_test_coefs_dust_new.RData")
  } else{load("data/figure_data/fig2_permutation_test_coefs_dust.RData")}



pdf("figures/raw/Fig3_b_raw.pdf", width = 6, height = 3)

hist(beta_dust_timing,axes=F, xlab = "",ylab = "", main = "", breaks=15, ylim = c(0,500), col = add.alpha('gray90', 0.5), 
     border = add.alpha('gray20', 0.5), xlim = c(-0.015, 0.015))


hist(beta_dust_share, add=T,  col =add.alpha('gray20', 0.5), border ='white', breaks =6)



axis(2, at = seq(0,500,100),labels = seq(0,0.5,0.1), las=2)

axis(1, at =seq(-0.015, 0.015, .005))

axis(1, at = 1.09) #from fig 2a
dev.off()



#load coefficients for plotting. If script 04 was run load that output. otherwise load pre-processed data. 
if(file.exists("data/figure_data/fig2_permutation_test_coefs_rain_new.RData")){
  load("data/figure_data/fig2_permutation_test_coefs_rain_new.RData")
} else{load("data/figure_data/fig2_permutation_test_coefs_rain.RData")}

pdf("figures/raw/Fig3_c_raw.pdf", width = 6, height = 3)

hist(beta_dust_timing,axes=F, xlab = "",ylab = "", main = "", breaks=15, ylim = c(0,600), col = add.alpha('gray90', 0.5), 
     border = add.alpha('gray20', 0.5), xlim = c(-0.05, 0.05))


hist(beta_dust_share, add=T,  col =add.alpha('gray20', 0.5), border ='white', breaks =3)



axis(2, at = seq(0,600,100),labels = seq(0,0.6,0.1), las=2)

axis(1, at =seq(-0.05, 0.05, .01))
abline(col = 'red', v= -1.62)
axis(1, at = -1.62)#see fig 2a
dev.off()


