source("scripts/0_loadPackages.R")
source("scripts/0_loadFunctions.R")

      ##### Load Data #####
      load("data/intermediate_outputs/fig1_data.RData") #pm average data for panels (a) and (b) [pm_ave and pm_dust_ave and pm_dust_share_ave and africa (shapefile) and dsa (plotting bodele)]
      data <- read_rds("data/inputs/analysis_data.rds")
      #cell by time period data frame with pm2.5 and night lights (for cells in DHS locations)
      nldf <- read_rds("data/inputs/nightLights/nl_pm25_cell_level_data.rds")
      

      pdf("figures/raw/Fig1_panels_a_b.pdf",width = 6, height = 3.5)    
      
      par(mfrow = c(1,2))
      par(mar = c(2,3,3,1))
      
      
      pal1 <- colorRampPalette(colors = add.alpha(c("#404096","#63AD99","#BEBC48","#E66B33","#D92120"), 0.5))
      plot(africa, border = NA, axes=F,xlab = "",ylab = "")
      plot(pm_ave, col = pal1(256),add=T, legend = F)
      plot(africa, add=T, lwd = 0.25)
      mtext(side =1,text ="PM2.5 Concentration", line=0.5)
      
      plotrix::gradient.rect(-10,-42.2,45,-36.7,col=plotrix::smoothColors(pal1(5000)),
                             gradient="x", border = NA)
      
      text(x = -10 + 13.75*0+ 0.9, y = -44, labels = "0", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*1, y = -44, labels = "30", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*2, y = -44, labels = "60", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*3, y = -44, labels = "90", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*4 -2.5, y = -44, labels = ">120", col = 'gray30',cex = 1)
      
      
      pal2 <-colorRampPalette(colors = add.alpha(c("white","#63AD99","#BEBC48","#E66B33","#D92120"), 0.3))
      plot(africa, border = NA, axes=F,xlab = "",ylab = "")
      plot(pm_dust_share_ave, col = pal2(256),add=T, legend = F)
      plot(africa, add=T, lwd = 0.25)
      mtext(side = 1,text ="Share of PM2.5 from Dust",line=0.5)
      plotrix::gradient.rect(-10,-42.2,45,-36.7,col=plotrix::smoothColors(pal2(5000)),
                             gradient="x", border = NA)
      text(x = -10 + 0.9 + 13.75*0, y = -44, labels = "0", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*1, y = -44, labels = "25", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*2, y = -44, labels = "50", col = 'gray30',cex = 1)
      text(x = -10 + 13.75*3, y = -44, labels = "75", col = 'gray30',cex = 1)
      text(x = -10 -2.5 + 13.75*4, y = -44, labels = "100", col = 'gray30',cex = 1)
      
      dev.off()
      
      
      
      
  
      #get averages for each location
      
     
      
      
          
      
      cb1 <- data %>% group_by(country, fe_cluster) %>% 
        summarise(co_bod = mean(dust_bod_post), co_rf =mean(rain_bod_post) ,pm =mean(pm25_post)) %>% 
        mutate(x = round(co_bod)) %>% 
        group_by(x) %>%summarise(y = mean(pm, na.rm=  T), ylow = quantile(pm, 0.25), yhigh = quantile(pm, 0.75), ymed = quantile(pm, 0.5), n = n()) %>% 
        filter(x > 66 & x < 81)
      
        cb1_hist <- data %>% group_by(country, fe_cluster) %>% 
        summarise(co_bod = mean(dust_bod_post), co_rf =mean(rain_bod_post) ,pm =mean(pm25_post)) %>% 
        mutate(x = round(co_bod,1)) %>% 
        group_by(x) %>%summarise(y = mean(pm, na.rm=  T), ylow = quantile(pm, 0.25), yhigh = quantile(pm, 0.75), ymed = quantile(pm, 0.5), n = n()) %>% 
        filter(x > 66 & x < 81) %>% mutate(n = replace(n, n<25, 25))
      
   
      cb2 <- data %>% group_by(country, fe_cluster) %>% 
        summarise(co_rf =mean(rain_bod_post) ,pm =mean(pm25_post)) %>% 
        mutate(x = round(co_rf/0.5)*0.5) %>% 
        group_by(x) %>%summarise(y = mean(pm, na.rm=  T), ylow = quantile(pm, 0.25), yhigh = quantile(pm, 0.75), ymed = quantile(pm, 0.5), n = n()) %>% 
        filter(x >= 41 & x <= 46)
      

      cb2_hist <- data %>% group_by(country, fe_cluster) %>% 
        summarise(co_rf =mean(rain_bod_post) ,pm =mean(pm25_post)) %>% 
        mutate(x = round(co_rf/0.05,1)*.05) %>% 
        group_by(x) %>%summarise(y = mean(pm, na.rm=  T), ylow = quantile(pm, 0.25), yhigh = quantile(pm, 0.75), ymed = quantile(pm, 0.5), n = n()) %>% 
        filter(x > 41 & x < 46) %>% mutate(n = replace(n, n<5, 5), n = replace(n, n>150,150 ))
      
      cb3 <- nldf %>% mutate(x = round(nl/5)*5) %>% 
        group_by(x) %>%  summarise(
          pmt= mean(pm), pma = mean(pm_anthro), pma_low = quantile(pm_anthro, .25), pma_high = quantile(pm_anthro, .75),
          pmd = mean(pm_dust), pmd_low = quantile(pm_dust, 0.25), pmd_high = quantile(pm_dust, 0.75)) %>% mutate(pmd=replace(pmd,pmd>14.5 , 14) ) 
      
      
      
       
       

  

      
      pdf("figures/raw/Fig1_panels_c_d_e_raw.pdf", width = 12, height = 4, useDingbats = F)
      
        par(mfrow = c(1,3))
        par(mar = c(3,3,1,1))
        
        
        ## panel (c)
             plot(cb1$x, cb1$ymed, axes = F, xlab = "",ylab = "", pch = 21,col = NA, ylim = c(-2.5, 45), cex=1.2, xlim = c(66,81))
             segments(x0 = cb1$x, x1 = cb1$x, y0 = cb1$ylow, y1 = cb1$yhigh, col = add.alpha('gray90',1), lwd = 2)
             lines(0:81,predict(lm(y~x, data = cb1), newdata = data.frame(x = 0:81)),lty=2)
             points(cb1$x, cb1$y,  pch = 21, bg = 'gray50', cex=2)
           
            axis(1, at = seq(66,80,2))
            axis(2, las =2, at = seq(0,40,10))
            mtext(side = 1, text = "Dust over Bodele (ug/m3)",line =3)
            mtext(side = 2, text = "Local PM2.5 Concentration",line =3)
            
            rect(xleft = cb1_hist$x-0.05, xright = cb1_hist$x + 0.05,ybottom = -2.5, ytop = -2.5 + (cb1_hist$n/max(cb1_hist$n))*10 , col = add.alpha('gray50', 0.25), border = 'gray90')
            
            
  
        ## panel (d)
            plot(cb2$x, cb2$ymed, axes = F, xlab = "",ylab = "", pch = 21,col = NA, ylim = c(-2.5, 45), cex=1.2, xlim = c(40.5,46))
            segments(x0 = cb2$x, x1 = cb2$x, y0 = cb2$ylow, y1 = cb2$yhigh, col = add.alpha('gray90',1), lwd = 2)
            lines(0:81,predict(lm(y~x, data = cb2), newdata = data.frame(x = 0:81)),lty=2)
            points(cb2$x, cb2$y,  pch = 21, bg = 'gray50', cex=2)

            axis(1, at = seq(41,46,1))
            axis(2, las =2, at = seq(0,40,10))
            mtext(side = 1, text = "Dust over Bodele (ug/m3)",line =3)
            
            rect(xleft = cb2_hist$x-0.005, xright = cb2_hist$x + 0.005,ybottom = -2.5, ytop = -2.5 + (cb2_hist$n/max(cb2_hist$n))*10 , col = add.alpha('gray50', 0.25), border = 'gray90')
            
            
            
            
            
        ## panel (e)
            plot(cb3$x, cb3$pma, axes = F,xlab = "", ylab = "", bg = add.alpha('black', 1),col = 'white', ylim = c(10,20), pch = 21, cex=2, xlim = c(0, 65))
            lines(predict(lm(pma ~ x, data = cb3), newdata= data.frame(x = 0:60)), col = 'black',lty=1, lwd=0.5)
            
            points(cb3$x, cb3$pmd,  pch = 21, bg = add.alpha('white', 1),col = 'gray30',cex=2, lwd=1.5)
            lines(predict(lm(pmd ~ x, data = cb3), newdata= data.frame(x = 0:60)),lty=3,col = 'gray30')
      
                    
            axis(2,las=2, at = seq(10,20,2.5))
            axis(1)
            mtext(side = 1, text = "Local Nightlights",line=3)

            

            
      dev.off()







