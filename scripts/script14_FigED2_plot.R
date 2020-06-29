
####################### spatial panels


#  dts gives date info for layers
#   dust is raster brick   
grid[] <- 1:ncell(grid)  

africa_cells <- raster::extract(grid, africa, small = T) %>% unlist() %>% unique() %>% sort()
all_cells <- 1:ncell(grid)
non_africa_cells <- all_cells[all_cells %in% africa_cells == F]
dust_backup <- dust

dust[non_africa_cells] <- NA
dust[dust>2.5]<-2.5 #define upper bound
dust[dust<0]<-0 #define lower bound
dust[1]<-3  #set a max (for convenient raster color scale plotting)
dust[2]<-0  #set a min (for convenient raster color scale plotting)
            #now have range 0-3 for color scale





pal1 <- colorRampPalette(colors = add.alpha(c("#404096","#63AD99","#BEBC48","#E66B33","#D92120"), 0.5))

start_pt <- which(dts$year == 2009 & dts$month == 2 & dts$dom == 10)


pdf("figures/raw/FigED2a_raw.pdf", width = 3, height = 4)
par(mfrow = c(3,2))
for(tp in (start_pt):(start_pt+5)){
  par(mar = c(0,0,0,0))
  plot(africa, border = NA)
  plot(dust[[start_pt]], add = T, legend = F, col = pal1(256))
  plot(africa, add=T, lwd = 0.1)
  rect(xleft = -40, xright = -19, ybottom = -40, ytop = 40, border = NA, col ='white')
  rect(xleft = -40, xright = 0, ybottom = -40, ytop = 0, border = NA, col ='white')
  rect(xleft = 60, xright = 70, ybottom = -40, ytop = 40, border = NA, col ='white')
  rect(xleft = -40, xright = 60, ybottom = -60, ytop = -40, border = NA, col ='white')
  rect(xleft = 52, xright = 80, ybottom = -60, ytop = 40, border = NA, col ='white')
  
  mtext(side = 3, text = paste(simpleCap(as.character(dts[tp,"month_name"]))," ",as.character(dts[tp,"dom"]),", ", as.character(dts[tp,"year"]), sep = "" ) ,cex=0.25,line = -0.5) 
}


dev.off()





######### TEMPORAL FIGURE ############


pdf("figures/raw/FigED2b_insert_raw.pdf",width = 6, height = 3.5)       
par(mar = c(0,0,0,0))

plot(africa, lwd = 0.01, col = 'gray90', border = 'gray30')
points(xyFromCell(grid, bod_cells), pch = 15, col = 'red', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, nmy_cells)), pch = 15, col = 'forestgreen', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, kno_cells)), pch = 15, col = 'orange', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, nrb_cells)), pch = 15, col = 'navy', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, bfs_cells)), pch = 15, col = 'red3', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, bmk_cells)), pch = 15, col = 'royalblue', cex = 0.4)
points(xyFromCell(grid, cellFromXY(grid, dkr_cells)), pch = 15, col = 'purple', cex = 0.4)
dev.off()




pdf("figures/raw/FigED2b_raw.pdf", width = 7.5, height = 5)      
par(mar = c(5,5,4,4))    

xy <- data.frame(x = -2:6, y1 = cors[["nmy"]], y2 = cors[["kno"]], y3 = cors[["mtn"]], y4 = cors[["nrb"]], y5 = cors[["dkr"]], y6 = cors[["kms"]], y7 = cors[["lagos"]], y8 = cors[["bmk"]], y9 = cors[["djo"]], y10 = cors[["bfs"]]) %>% filter(x>=0)

plot(0:6, predict(loess(y1 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), type = "l", xlab = "", axes=F,ylab = "", ylim = c(-.025, 0.25), col = NA,lwd = 2)

abline(h = seq(0, 0.25, 0.05), col = 'gray75', lwd = 0.75, lty = 4)
abline(h = 0, lty = 4, col = 'black', lwd = 0.75)

lines(0:6, predict(loess(y1 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'forestgreen', lwd = 2)
lines(0:6, predict(loess(y2 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'orange', lwd = 2)
lines(0:6, predict(loess(y4 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'navy', lwd = 2)
lines(0:6, predict(loess(y10 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'red3', lwd = 2)
lines(0:6, predict(loess(y8 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'royalblue', lwd = 2)
lines(0:6, predict(loess(y5 ~ x, data = xy, span = 1), newvalues = data.frame(x = 0:6)), col = 'purple', lwd = 2)


axis(1, at = 0:6, labels = c("day t", paste("day t+", 1:6, sep = "")))
axis(2, tick = T, las =2)

mtext(side = 1, text = "Lagged day",line = 3.5, cex =1.25)
mtext(side = 2, text = "Daily correlation ",line = 3.5, cex = 1.25)

dev.off()





