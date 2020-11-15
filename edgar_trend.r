### RUN edgar_v2 before to load data
### Make sure all logics are FALSE to not incidently recalculate
source("edgar_v2.r")
source("p_edgar_trend.r")
require(RColorBrewer)

edgar_plots <- paste0(google, "projects/edgar/plots/")


edgarfood[, cat := substr(ipcc, 1, 2)]
edgarfood[, sec := paste0("sec", substr(ipcc, 1, 1))]

# Assing indirect N2O emissions to main sectors
edgarfood[ipcc%in%c("7B1", "7C1"), sec:="sec1"]
edgarfood[ipcc%in%c("7B2", "7C2"), sec:="sec2"]

secdesc <- data.table(
  secs = c(paste0("sec", 1:6), "other"),
  desc = c("Energy", "Industrial Processes", "Solvent and Other Product Use",
           "Agriculture", "Land-Use Change & Forestry", "Waste", "Energy, Industry and Product Use"),
  dess = c("Energy", "Industry", "Product Use",
           "Agriculture", "LUCF", "Waste", "Energy+Industry")
)

foodglobal <- edgarfood[stage != "" , 
                        sum(value, na.rm = TRUE), 
                        by = .(Substance, sec, cat, stage, stagedet, compartment, part, variable)]

##
##
##  Plots of the Trend of global total GHG emissions by sector
##  - Total GHG (currently AR5 - 20200819)
##  - By gas: CO2, CH4, N2O
##
##

if(TRUE){
  
  runp <- function(fooddt, suffx=""){
    
    plghg <- plotEmTrend(dt = fooddt, gas = "GHG", suffx = suffx)
    
    gases <- unique(foodglobal$Substance)[1:3]
    for (g in gases){
      foodglob <-dcast.data.table(foodglobal[stage!="" & Substance==g], 
                                  variable ~ sec, value.var = "V1", sum)
      g <- gsub("GWP_100_", "", g)
      pl <- plotEmTrend(dt = fooddt, gas = g, suffx = suffx)
    }
    
  }
  
  foodglob.bysec <-dcast.data.table(foodglobal[stage!=""], variable ~ sec, value.var = "V1", sum)
  runp(foodglob.bysec)
  
  foodglob.bysecagg <- foodglob.bysec[, other := sec1 + sec2 + sec3][, .(variable, other, sec4, sec5, sec6)]
  runp(foodglob.bysecagg, suffx = "agg")
}

##
##
##  Plots of Global total GHG emissions by gas and sector
##
##
foodglob <- dcast.data.table(foodglobal[stage!="" & variable==2015], 
                             sec ~ Substance, value.var = "V1", sum)
foodglob <- foodglobal[stage!="" & variable==2015, sum(V1), by=.(sec, Substance)]
foodglob[, Substance := gsub("GWP_100_", "", Substance)]
foodglob[! Substance %in% c("CO2", "CH4", "N2O", "Fgases"), Substance := "Fgases"]
foodglob <- foodglob[, sum(V1), by=.(sec, Substance)]

if(FALSE){
  filename <- "globalemissions_gas_sectors"
  yl <- bquote(paste("Food System ", .(gas), " [",CO["2e"]~yr^{-1},"]"))
  p4 <- ggplot(foodglob, aes_string(x="sec", fill = "Substance")) + 
    geom_bar(aes_string(y="V1", fill = "Substance"), position = "stack",
             stat='identity', width = 0.8, alpha = 0.8, show.legend = TRUE) +
    scale_fill_manual(values = rev(my.cols[1:4])) +
    xlab("Sectors") + 
    ggtitle(paste0("Global GHG emissions by gas and sector")) +
    ylab(yl)
  png(filename = paste0(edgar_plots, filename, "~", format(Sys.time(), "%Y%m%d%H"), ".png"), width=10000, height=5000, units="px", res=1000)
  print(p4)
  dev.off()
}  

if(FALSE){
  filename <- "globalemissions_share_gas_sectors"
  foodp5 <- copy(foodglob)
  foodp5[, sharegas := V1 / sum(V1), by=(sec)]
  setkey(foodp5, sec)
  foodp5[, sumsec := sum(V1), by=(sec)]
  foodp5[, sumgas := sum(V1), by=(Substance)]
  foodp5[, width := sumsec * 2 / max(sumsec)]
  
  # See https://stackoverflow.com/questions/20688376/how-to-make-variable-bar-widths-in-ggplot2-not-overlap-or-gap
  
  w <- unique(foodp5[, .(sec, sumsec)])
  w[, sharex := sumsec/sum(sumsec)]
  w[, x1 := cumsum(sharex)-sharex]
  w[, x2 := cumsum(sharex)]
  w[, posx := 0.5 * (x1 + x2)]
  p <- w$pos
  
  h <- unique(foodp5[, .(sec, Substance, emission=V1)])
  h[, sharey := emission/sum(emission), by=(sec)]
  h[, sharet := emission/sum(emission)]
  h[, y1 := cumsum(sharey)-sharey, by=sec]
  h[, y2 := cumsum(sharey), by=sec]
  h[, posy := 0.5 * (y1 + y2)]
  h[, id := .I]
  
  h1 <- merge.data.table(h, w, by="sec")
  
  mypals <- c("Blues", "Reds", "Greys", "Greens")
  mygas <- unique(h$Substance)
  mysec <- sort(unique(h$sec))
  
  
  h4plot <- merge(h1[!sec=="sec3"], secdesc[,.(sec=secs, dess)], by="sec")
  xx <- Reduce(c, lapply(1:nrow(h4plot), function(x) as.numeric(unlist(h4plot[x, .(x2, x1, x1, x2)]))))
  yy <- Reduce(c, lapply(1:nrow(h4plot), function(x) as.numeric(unlist(h4plot[x, .(y1, y1, y2, y2)]))))
  
  gcol <- sapply(1:nrow(h4plot), function(x) mypals[which(mygas==h4plot$Substance[x])])
  gsec <- sapply(1:nrow(h4plot), function(x) which(mysec==h4plot$sec[x]))
  cc <- sapply(1:nrow(h4plot), function(x) 
    brewer.pal(n = 8, name = gcol[x])[9-gsec[x]])
  cc4 <- rep(cc, each=4)

  data2plot <- data.table(merge(1:4, h4plot))
  data2plot <- data2plot[, .(id, sec, Substance, emission, dess)]
  data2plot[, `:=` (x = xx, y = yy, col=cc4, id2 = paste0(Substance, " - ", dess))]

  ## Check polygon which could work:
  ## https://ggplot2.tidyverse.org/reference/geom_polygon.html
  
  p6 <- ggplot(data2plot, aes_string(x="x", y="y")) +
    geom_polygon(aes(group=id, fill=id2), 
                 fill=cc4, stat = "identity", 
                 show.legend = NA) +
#    scale_fill_identity() + 
    xlab("Share of food system GHG emissions by sector") + 
    ylab("Share of food system GHG emissions by gas") +
    # Remove Sector 3 as almost no emissions
    geom_text(data=h4plot, mapping=aes(label=dess, x=posx, y=-0.05), size=3.0, fontface='bold') +
    geom_text(data=h4plot[sharet>0.003], mapping=aes(label=paste0(round(sharet*100,1), "%"), 
                                       x=posx, y=posy), size=3, colour="white")
  
  
  p6
  
  leg <- data.table(expand.grid(secs, unique(h4plot$Substance)))
  leg <- merge(leg, secdesc[, .(sec=secs, dess)], by="sec")
  setnames(leg, c("Var1", "Var2"), c("sec", "gas"))
  mygas <- c("CO2", "CH4", "N2O", "Fgases")
  mysec <- paste0("sec", 1:6); mysec <- mysec[!mysec=="sec3"]
  leg$gas <- factor(leg$gas, levels = mygas)
  leg$sec <- factor(leg$sec, levels = mysec)
  setkey(leg, sec, gas)
  leg <- leg[sec!="sec3"]
  
  gcol <- sapply(1:nrow(leg), function(x) mypals[which(mygas==leg$gas[x])])
  gsec <- sapply(1:nrow(leg), function(x) which(mysec==leg$sec[x]))
  cc <- sapply(1:nrow(leg), function(x) 
    brewer.pal(n = 8, name = gcol[x])[9-gsec[x]])
  cc <- sapply(1:nrow(leg), function(x) 
    brewer.pal(n = 8, name = gcol[x])[9-gsec[x]])
  leg$cc <- cc
  leg$xpos <- sapply(1:nrow(leg), function(x) which(mygas == leg$gas[x]))
  leg$ypos <- sapply(1:nrow(leg), function(x) which(mysec == leg$sec[x]))
  png("colors.png"); barplot(rep(1, length(cc)), col = cc, axes = FALSE); dev.off()
  
  pleg <- ggplot(leg,  aes(x=gas, y=v)) + 
    geom_bar(fill=cc, stat="identity", position = position_stack(reverse = TRUE), colour="black", alpha=1) +
    geom_text(data=leg, mapping=aes(label=dess, x=xpos,  y=ypos-0.4), size=3, colour="white") +
    geom_text(data=leg, mapping=aes(label=gas, x=xpos,  y=ypos-0.6), size=3, colour="white") +
    ylab("Sectors") + xlab("Gas") 
  
  
  barplot(rep(1, 2), col = h4leg[1:2]$cc)
  
  png(filename = paste0(edgar_plots, "emissionsByGasSector_legend", "~", format(Sys.time(), "%Y%m%d%H"), ".png"), width=10000, height=5000, units="px", res=1000)
  print(pleg)
  dev.off()
  
}

# install.packages("treemapify")
require(treemapify)
#ptreemap <- 
ggplot(data2plot, aes(area=emission, 
                      group=sec, 
                      label=id2,
                      fill=id2)) +
  geom_treemap(fill=data2plot$col, layout='srow') + #squarified, scol, srow, fixed
  geom_treemap_text(size=10)

                    