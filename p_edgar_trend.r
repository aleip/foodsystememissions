plotEmTrend <- function(dt = foodglob.bysec, gas="GHG", suffx = ""){
  
  dto <- copy(dt)
  
  # Construct data frame to plot
  years <- unique(dto$variable)
  nyears <- length(years)
  secs <- setdiff(names(dto), "variable")
  secs <- intersect(c("sec4", "sec6", "sec1", "sec2", "sec3", "sec5", "other"), secs)
  sess <- intersect(c("Agriculture", "Waste", "Energy", "Industry", "Product Use", "LUCF", "Energy+Industry"), 
                    secdesc$dess)
  nsecs <- length(secs)
  
  # Reorder
  dto <- dto[, c("variable", secs), with=FALSE]
  
  # Calculate shares
  dtshare <- copy(dto[variable %% 5 == 0])
  dtshare[, (secs) := .SD/1000000, .SDcols=secs]
  dtshare[, tot := rowSums(.SD), .SDcols=secs]
  dtshare[, (paste0(secs, "sh")) := round(100*.SD/tot,0), .SDcols=secs][]
  
  for (x in secs){
    dtshare[, (paste0(x, "cum")) := rowSums(.SD), .SDcols=secs[1:which(secs==x)]]
  }
  
  # --> we need a data.frame with nyears x nsecs rows
  time <- rep(years, nsecs)
  Sectors <- as.vector(sapply(rep(secs, each=nyears), function(x)
    secdesc[which(secs==x)]$dess))
    
  value <- Reduce(c, lapply(as.character(secs), 
                            function(x) t(dto[, x, with=FALSE][[1]])))/1000000
  
  my.cols <- heat.colors(n = length(secs), alpha = 0.8)
  
  mydata <- data.frame(time, value, Sectors)
  mydata$Sectors <- factor(mydata$Sectors, levels=rev(sess))
  mydata
  
  
  p1 <- ggplot(mydata, aes(x=time, y=value, fill=Sectors)) + geom_area()
  #p1 <- p1 + scale_fill_manual(values = my.cols)
  #p1 + scale_fill_brewer(palette="Dark2") 
  #p1 + scale_fill_grey()
  #p1 + scale_fill_brewer(palette = "Blues")
  #p1 + scale_fill_brewer(palette = "Greys")
  #p1 + scale_fill_manual(values = my.cols)
  p1 <- p1 + scale_fill_brewer(palette = "Blues")
  maxrange <- layer_scales(p1)$y$range$range[2]
  p1 <- p1 + 
    #Add 10% to the scale to allow annotations
    scale_y_continuous(limits = c(layer_scales(p1)$y$range$range[1], 
                                  1.1 * maxrange))
  p2 <- p1
  for(x in secs){
    if(x=="sec3"){next()}
    
    shares <- dtshare[, paste0(x, "sh"), with=FALSE][[1]]
    if(max(shares < 2)) {next()}
    xshift <- 0.1
    yshift <- -0.02 * maxrange
    if(min(shares < 2)) yshift = +0.02 * maxrange
    p2 <- p2 + annotate(geom="text", 
                        x=dtshare$variable+xshift, 
                        y=dtshare[, paste0(x, "cum"), with=FALSE][[1]]+yshift, 
                        label=paste0(shares, "%"), 
                        angle=0, 
                        hjust=c(0, rep(0.5, nrow(dtshare)-2), 1), 
                        colour = "white", fontface =2, size=3)
  }
  p3 <- p2 + annotate(geom="text", 
                      x = dtshare$variable,
                      y = dtshare$tot + 0.025 * maxrange,
                      label = format(round(dtshare$tot, 1), nsmall=1),
                      # Attention hjust always in 'text' direction, therefore
                      # the meaning 'changes' for vertical plotting
                      # https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
                      angle=90, hjust=0, 
                      vjust=c(1, rep(0.5, nrow(dtshare)-2), 0), 
                      colour="blue", fontface = 2, size = 4
  )
  #annotate(geom="text", x=as.numeric(as.vector(trendshare$year)), y=trendshare$Nagri, 
  #         label=paste0(trendshare$Otshare, "%")) 
  yl <- bquote(paste("Food System ", .(gas), " [",CO["2e"]~yr^{-1},"]"))
  p3 <- p3 + xlab("Year") + ggtitle(paste0("Trend of global ", gas, " emissions by sector")) + ylab(yl)
  
  print(p3)
  
  png(filename = paste0(edgar_plots, "emissions_by_sector", gas, suffx, "~", format(Sys.time(), "%Y%m%d%H"), ".png"), width=10000, height=5000, units="px", res=1000)
  print(p3)
  dev.off()
  
  return(p3)
  
}
