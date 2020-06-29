source("scripts/loadFunctions.R")
source("scripts/loadPackages.R")

#africa shapefile
africa <- read_rds("data/inputs/spatial_boundaries/africa_borders.rds")

#read in (partially processed) aerodata data
aerodata <- read_rds("data/inputs/aeronet/aeronet_calculation_2002_2016.rds") #volume aersol and dummy for whether "dust"

#read in estimated dust shares
dust_share <- brick("data/figure_data/figED3_dust_share.nc")

#very imperfect estimation of share of aerosols from dust by counting volume weighted days thought to be dust and days thought to be nondust (see SI for details)
shares <- aerodata %>% group_by(site, year, study_sample, lon, lat) %>% mutate(dust = dust*vol,nondust = nondust*vol) %>%  summarise(n_dust = sum(dust, na.rm = T), n_nondust = sum(nondust, na.rm = T)) %>% 
  mutate( share_dust_an = n_dust/(n_dust + n_nondust)) #estimate share from aeronet 



#add in van Donkelaar et al share estimates
shares$vd_cell <- cellFromXY(pm_dust, as.matrix(shares[,c("lon","lat")]))
annual_shares <- dust_share[shares$vd_cell]
colnames(annual_shares)<-paste("yr",1998:2016,sep="")
annual_shares <- data.frame(site = shares$site, annual_shares)
annual_shares<-annual_shares %>% gather(year,dust_share, -site)
annual_shares$year <- as.numeric(substr(annual_shares$year, 3,10))
annual_shares<-annual_shares %>% arrange(site,year)
names(annual_shares)[3]<-"share_dust_vd"
annual_shares<-annual_shares %>% group_by(site, year) %>% summarise(share_dust_vd = mean(share_dust_vd))

#merge in yearly share estimates with aeronet  
shares <- left_join(shares, annual_shares, by = c("site","year"))

# calculate average shares at each site
ave_shares <- shares %>% group_by(site, study_sample) %>% 
  summarise( share_dust_vd = mean(share_dust_vd, na.rm = T), share_dust_an = mean(share_dust_an, na.rm = T), lon = mean(lon, na.rm = T), lat = mean(lat, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(site = as.character(site),
         diff = share_dust_vd - share_dust_an)














pdf("figures/raw/FigED3_raw.pdf", width = 10, height = 10)

par(mfrow = c(2,2))
par(mar = c(5,5,3,1))


# panel a
r2 <- summary(lm(share_dust_vd ~ share_dust_an, data = shares))$adj.r.squared
plot(shares$share_dust_an, shares$share_dust_vd, pch = 16, xlim = c(0, 1), ylim = c(0,1 ), xlab = "", ylab = "", main = "", col =NA, axes=F)
points(shares$share_dust_an, shares$share_dust_vd, pch = 21, bg = 'gray50', col ='black')
segments(x0 = seq(0,1, .1), y0 = 0, y1 = 1, col = 'gray30',lwd = 0.25, lty=2)        
segments(y0 = seq(0,1, .1), x0 = 0, x1 = 1, col = 'gray30',lwd = 0.25, lty=2)        
segments(x0 = 0, y0 = 0, x1 = 1, y1=1, )
axis(1, tick = T, at = seq(0, 1, .1),line=-1)
axis(2, tick = T, at = seq(0, 1, .1),line=-1, las = 2)
mtext(side = 1, text = "Dust share - Aeronet ground stations",cex=1,line=3)
mtext(side = 2, text = "Dust (+ sea salt) share - satellite derived",cex=1,line=3)

mtext(side = 3, adj = 1, text = "annual",line=1,cex=1.5)
text(x = 0.15, y = 0.95, labels =TeX(paste("$R^2$=",round(r2,3), sep = "" )),cex=1.25)



# panel (b)
r2 <- summary(lm(share_dust_vd ~ share_dust_an, data = ave_shares))$adj.r.squared
plot(ave_shares$share_dust_an, ave_shares$share_dust_vd, pch = 16, xlim = c(0, 1), ylim = c(0,1 ), xlab = "", ylab = "", main = "", col =NA, axes=F)
points(ave_shares$share_dust_an, ave_shares$share_dust_vd, pch = 21, bg = 'gray50', col ='black')
segments(x0 = seq(0,1, .1), y0 = 0, y1 = 1, col = 'gray30',lwd = 0.25, lty=2)        
segments(y0 = seq(0,1, .1), x0 = 0, x1 = 1, col = 'gray30',lwd = 0.25, lty=2)        
segments(x0 = 0, y0 = 0, x1 = 1, y1=1, )
axis(1, tick = T, at = seq(0, 1, .1),line=-1)
axis(2, tick = T, at = seq(0, 1, .1),line=-1, las = 2)
mtext(side = 1, text = "Dust share - Aeronet ground stations",cex=1,line=3)
mtext(side = 2, text = "Dust (+ sea salt) share - satellite derived",cex=1,line=3)

text(x = 0.15, y = 0.95, labels =TeX(paste("$R^2$=",round(r2,3), sep = "" )),cex=1.25)
mtext(side = 3, adj = 1, text = "average",line=1,cex=1.5)



#panel (c)
ave_shares <- filter(ave_shares, study_sample == 1)
pal <- two.colors(middle = 'royalblue',start ='white',end = 'navy')
int <- classIntervals(ave_shares$share_dust_vd, style = "fixed",fixedBreaks = c(seq(0,1,.025)))
col <- findColours(int, pal)
plot(africa)
points(ave_shares$lon, ave_shares$lat, pch = 21, cex=2, bg = col)
legend(x = "bottomleft", fill = c(fields::two.colors(n = 11,middle = 'royalblue',start ='white',end = 'navy')), legend = seq(0,1,.1),bty = "n", horiz = F)
text(x = -20.5, y = 3, labels = "Dust Share")
mtext(side = 3, text = "Model derived",adj=0,cex=1.5,line=-1)


#panel (d)    
pal <- two.colors(middle = 'royalblue',start ='white',end = 'navy')
int <- classIntervals(ave_shares$share_dust_an, style = "fixed",fixedBreaks = c(seq(0,1,.025)))
col <- findColours(int, pal)
plot(africa)
points(ave_shares$lon, ave_shares$lat, pch = 21, cex=2, bg = col)
legend(x = "bottomleft", fill = c(fields::two.colors(n = 11,middle = 'royalblue',start ='white',end = 'navy')), legend = seq(0,1,.1),bty = "n", horiz = F)


text(x = -20.5, y = 3, labels = "Dust Share")
mtext(side = 3, text = "Aeronet ground station derived",adj = 0,cex=1.5,line=-1)

dev.off()

