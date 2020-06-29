
    #define a function for adding alphas to colors
        add.alpha <- function(col, alpha=1){
          if(missing(col))
            stop("Please provide a vector of colours.")
          apply(sapply(col, col2rgb)/255, 2,
                function(x)
                  rgb(x[1], x[2], x[3], alpha=alpha))
        }

    #formulate equation from inputs    
        make_equation <- function(controls, fe, depvar, cluster_var, iv){
            as.formula(paste(depvar, " ~ ", controls, "|", fe, "|", iv, "|",cluster_var, sep=""))
        }
        
    #paste function    
        "%&%"<-function(x,y)paste(x,y,sep="")         
        
        
        
        
    #function for dealing with capitalizations
        simpleCap <- function(x) {
            s <- strsplit(x, " ")[[1]]
            paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse=" ")
        }
        
        #function for pulling a box around cells (used for Fig ED2)
        get_cells <- function(x,y){rbind(c(x,y), c(x-1, y), c(x+1, y), c(x, y-1), c(x, y+1), c(x-1, y-1), c(x-1, y+1), c(x+1, y-1), c(x+1, y+1))}
        
        
        
        ## get dust for location  (used for Fig ED2)
        
        get_series <- function(cells_var){    
            
            cells <- cellFromXY(grid, cells_var )
            tmp <- dust[cells] %>% apply(2,function(x){max(x, na.rm =T)})
            tmp[is.infinite(tmp)]<-NA
            
            data2 <- data.frame(dts, var = tmp)
            rownames(data2) <- 1:nrow(data2)       
            
            data2$harm <- as.numeric(data2$month %in% c(10:12, 1:3))
            data2$season <- NA
            data2$season[data2$month %in% 10:12] <- data2$year[data2$month %in% 10:12] - 2000
            data2$season[data2$month %in% 1:3] <- data2$year[data2$month %in% 1:3] - 2001
            data2 <- data2 %>% filter(harm == 1 & season > 0)
            
            
            return(data2$var)
        }
        
        
        ##get corrrelation (used for Fig ED2)
        
        get_cor <- function(var){
            
            c(
                cor(data_full2$bod_z[3:nrow(data_full)], var[1:(nrow(data_full) - 2)], use = "complete.obs"),
                cor(data_full2$bod_z[2:nrow(data_full)], var[1:(nrow(data_full) - 1)], use = "complete.obs"),
                cor(data_full2$bod_z, var, use = "complete.obs"),
                cor(var[2:nrow(data_full)], data_full2$bod_z[1:(nrow(data_full) - 1)], use = "complete.obs"),
                cor(var[3:nrow(data_full)], data_full2$bod_z[1:(nrow(data_full) - 2)], use = "complete.obs"),
                cor(var[4:nrow(data_full)], data_full2$bod_z[1:(nrow(data_full) - 3)], use = "complete.obs"),
                cor(var[5:nrow(data_full)], data_full2$bod_z[1:(nrow(data_full) - 4)], use = "complete.obs"),
                cor(var[6:nrow(data_full)], data_full2$bod_z[1:(nrow(data_full) - 5)], use = "complete.obs"),
                cor(var[7:nrow(data_full)], data_full2$bod_z[1:(nrow(data_full) - 6)], use = "complete.obs")
            )
            
        }
        
        
        
        
        
     