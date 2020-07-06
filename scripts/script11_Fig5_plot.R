source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")


  ### plot panel a ###

    if(file.exists("data/figure_data/fig5_sampled_costs_new.rds")){
      costs <- read_rds("data/figure_data/fig5_sampled_costs_new.rds")
    }else{costs <- read_rds("data/figure_data/fig5_sampled_costs.rds")}


    
      pdf("figures/raw/Fig5a_raw.pdf", width = 6, height = 5)
            par(mar = c(5,5,2,3))
            hist(costs,breaks=30, xlab = "", axes = F, ylab = "",main = "", col = 'gray80', xlim = c(0,100), ylim = c(0,250))
            axis(1, at = seq(0,100,20),cex.axis = 1.25)
            axis(2, tick = T, las = 2, at = seq(0,250,50), labels =  seq(0,250,50)/1000,cex.axis = 1.25)
            mtext(side = 1, text = "$ per averted year of life lost",cex=1.5, line=3)
            mtext(side = 2, text = "share of estimates",cex=1.5, line=3.5)
            segments(x0 = median(costs), y0 = 0, y1 = 250, col = add.alpha('red',0.5), lwd =2)
      dev.off()


  ### load data and do calculations for panel b ###  
    
       # load uncertainties calculated in script 10 (or else load pre-processed version included with replication materials)
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


          
  ### plot panel b ###
    
    pdf("figures/raw/Fig5b_raw.pdf", width = 12, height = 4)
        
        plot(rep(33,10),10:1, xlim = c(-300, 100),axes=F, xlab = "",ylab = "", pch = 16, col = NA)
        segments(x0 = seq(0,100,10), y0 = 0, y1 = 12, col = 'gray90', lwd = 0.75)
        rect(xleft = res$min, xright = res$max,ybottom = 1:10-.15, ytop = 1:10+.15, col = 'gray90', border = 'black')
        
        
        axis(1, tick = T, at = seq(0,100,20), cex.axis=.7)
        text(x = -150, y = 1:10,labels = as.character(res$par), cex = 0.75)
        segments(x0 = 23.5,y0 = 1, y1 = 11, col = add.alpha('red',0.5),lwd = 1)
    
    
    dev.off()
