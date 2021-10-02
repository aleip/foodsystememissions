plotEmTrend <- function(dt = foodglob.bysec, gas="GHG", suffx = "", singleplot=TRUE){
  
  dto <- copy(dt)
  
  # Construct data frame to plot
  years <- unique(dto$variable)
  nyears <- length(years)
  secs <- setdiff(names(dto), "variable")
  secs <- intersect(c("sec4", "sec6", "sec1", "sec2", "sec3", "sec5", "other"), secs)
  sess <- intersect(c("Agriculture", "Waste", "Energy", "Industry", "Product Use", "LULUCF", "Energy+Industry"), 
                    secdesc$dess)
  nsecs <- length(secs)
  
  # Reorder
  dto <- dto[, c("variable", secs), with=FALSE]
  
  # Convert N2O and CH4 to Mt / yr
  convunit <- 1e-6
  if(gas=="CH4") convunit <- 1e-3
  if(gas=="N2O") convunit <- 1e-3
  
  # Calculate shares
  #dtshare <- copy(dto[variable %% 5 == 0])
  dtshare <- copy(dto[variable %in% c(seq.int(1990, 2015, 5), 2018)])
  dtshare[, (secs) := .SD * convunit, .SDcols=secs]
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
                            function(x) t(dto[, x, with=FALSE][[1]])))*convunit
  my.cols <- heat.colors(n = length(secs), alpha = 0.8)
  
  # IPCC guidelines Single hue sequential schemes
  # 5 colors, blue (see Page 12)
  col1 <- rgb("239", "243", "255", maxColorValue = 255)
  col2 <- rgb("189", "215", "231", maxColorValue = 255)
  col3 <- rgb("107", "174", "214", maxColorValue = 255)
  #col3 <- rgb("0", "0", "214", maxColorValue = 255)
  col4 <- rgb("49", "130", "189", maxColorValue = 255)
  col5 <- rgb("8", "81", "156", maxColorValue = 255)
  col6 <- rgb("0", "0", "80", maxColorValue = 255)
  my.cols <- c(col1, col2, col3, col4, col5, col6)
  
  
  mydata <- data.frame(time, value, Sectors)
  mydata$Sectors <- factor(mydata$Sectors, levels=rev(sess))
  mydata
  
  fontcol <- c(rep(my.cols[1], 5), my.cols[3])
  if(length(unique(mydata$Sectors)) == 4) {
    my.cols <- my.cols[2:5]
    fontcol <- c(rep(col1, 2), rep(col5, 2))
  }
  
  #print(barplot(rep(1, 5), col = my.cols)) 
  
  p1 <- ggplot(mydata, aes(x=time, y=value, fill=Sectors)) + 
    #geom_area(show.legend = singleplot)
    geom_area(show.legend = TRUE)
  #p1 <- p1 + scale_fill_manual(values = my.cols)
  #p1 + scale_fill_brewer(palette="Dark2") 
  #p1 + scale_fill_grey()
  #p1 + scale_fill_brewer(palette = "Blues")
  #p1 + scale_fill_brewer(palette = "Greys")
  #p1 <- p1 + scale_fill_brewer(palette = "Blues")
  maxrange <- layer_scales(p1)$y$range$range[2]
  p1 <- p1 + 
    #Add 10% to the scale to allow annotations
    scale_y_continuous(limits = c(layer_scales(p1)$y$range$range[1], 
                                  1.1 * maxrange))
  p1 <- p1 + scale_fill_manual(values = my.cols)
  if(! singleplot) {p1 <- p1 + theme(legend.position = "none")}
  p1 <- p1 + theme(axis.text = element_text(size=16, face="bold"), )
  p1 <- p1 + scale_x_continuous(limits = c(1990, 2018), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2018))
  p2 <- p1
  
  for(i in 1:length(secs)){
    x <- secs[i]
    if(x=="sec3"){next()}
    
    shares <- dtshare[, paste0(x, "sh"), with=FALSE][[1]]
    if(max(shares < 2)) {next()}
    xshift <- 0.1
    yshift <- -0.02 * maxrange
    if(min(shares < 2)) yshift = +0.02 * maxrange
    p2 <- p2 + annotate(geom="text", 
                        x=dtshare$variable, 
                        y=dtshare[, paste0(x, "cum"), with=FALSE][[1]]+yshift, 
                        label=paste0(shares, "%"), 
                        angle=0, 
                        hjust=c(0, rep(0.5, nrow(dtshare)-2), 1), 
                        colour = fontcol[i], fontface =2, size=5)
  }
  
  # Add total emissions to graph
  gasround <- 1
  if(gas=="CH4"){gasround <- 0}
  p3 <- p2 + annotate(geom="text", 
                      x = dtshare$variable,
                      y = dtshare$tot + 0.025 * maxrange,
                      label = format(round(dtshare$tot, gasround), nsmall=gasround),
                      # Attention hjust always in 'text' direction, therefore
                      # the meaning 'changes' for vertical plotting
                      # https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
                      angle=90, hjust=0, 
                      vjust=c(1, rep(0.5, nrow(dtshare)-2), 0), 
                      colour="blue", fontface = 2, size = 5
  )
  
  #annotate(geom="text", x=as.numeric(as.vector(trendshare$year)), y=trendshare$Nagri, 
  #         label=paste0(trendshare$Otshare, "%")) 
  if(gas %in% c("GHG")){
    yg <- "GHG"
    yt <- bquote(paste("Trend of global ", GHG, " emissions by sector"))
    yl <- bquote(paste("Food System ", .(gas), " [Gt ",CO["2e"]~yr^{-1},"]"))
    if(! singleplot){yg <- bquote(paste(GHG, " [Gt ", CO["2e"]~yr^{-1},"]"))}
  }else if (gas=="CO2"){
    yg <- bquote(paste(CO[2]))
    yt <- bquote(paste("Trend of global ", CO[2], " emissions by sector"))
    yl <- bquote(paste("Food System ", CO[2], " [Gt ", CO[2] , ~yr^{-1},"]"))
    if(! singleplot){yg <- bquote(paste(CO[2], " [Gt ", CO[2] , ~yr^{-1},"]"))}
  }else if (gas=="CH4"){
    yg <- bquote(paste(CH[4]))
    yt <- bquote(paste("Trend of global ", CH[4], " emissions by sector"))
    yl <- bquote(paste("Food System ", CH[4], " [Mt ", CH[4] , ~yr^{-1},"]"))
    if(! singleplot){yg <- bquote(paste(CH[4], " [Mt ", CH[4] , ~yr^{-1},"]"))}
  }else if (gas=="N2O"){
    yg <- bquote(paste(N[2], "O"))
    yt <- bquote(paste("Trend of global ", N[2] , "O emissions by sector"))
    yl <- bquote(paste("Food System ", N[2], "O [Mt ", N[2], "O" , ~yr^{-1},"]"))
    if(! singleplot){yg <- bquote(paste(N[2], "O [Mt ", N[2], "O" , ~yr^{-1},"]"))}
  }
  p4 <- p3 + xlab("Year")
  # Add gas
  p4 <- p4 + annotate(geom="text", x=dtshare[1]$variable, y=1.05 *maxrange,
                      label = yg, angle=0, hjust=0, vjust=0, colour='blue',
                      fontface=2, size=8)
  p4 <- p4 + ylab(yl)
  
  if(singleplot) {p4 <- p4 + ggtitle(yt) + ylab(yl)}
  if(! singleplot) {p4 <- p4 + theme(axis.title = element_blank())}
  
  print(p4)
  
  if(singleplot) {singp <- "_single"} else {singp <- ""}
  pngfile <- paste0(edgar_plots, "emissions_by_sector", gas, suffx, "~", format(Sys.time(), "%Y%m%d%H"), singp, ".png")
  png(filename = pngfile, width=10000, height=5000, units="px", res=1000)
  print(p4)
  dev.off()
  
  return(p4)
  
}
