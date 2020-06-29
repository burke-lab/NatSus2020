source("scripts/0_loadPackages.R")
source("scripts/0_loadFunctions.R")

#see QuasiExperimentEstimates.xlsx for more on source of estimates and details on unit conversion calculations
estimates <- read_csv("data/figure_data/fig3_data.csv")%>% arrange(-estimate) 
estimates <- rbind(data.frame(citation = "This Study",location = "Sub-Saharan Africa",estimate= 24,lowerbound=10,upperbound = 35 ), estimates)

	
		
	pdf("figures/raw/Fig3b_raw.pdf", width = 8, height = 4)	
		      par(mar = c(4,16,2,3))
	        barplot(estimates$estimate, horiz = T,col = c("gray90",rep('gray70',nrow(estimates)-1)),  names.arg = paste(estimates$citation, estimates$location, sep = "-"),las=2,axes=F, xlim = c(0,60))
		
		      axis(1, at = seq(0,60,5),las =1)
		      segments(x0 = estimates$lowerbound, x1 = estimates$upperbound, y0 = seq(0.7,11.5,1.2))
		      mtext(side = 1, text = "% increase in infant mortality per 10ug/m3 PM2.5",line=3,cex=1.25)
		
		dev.off()
	

