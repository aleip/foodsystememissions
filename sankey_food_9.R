
# E. Solazzo, JRC February 2020

setwd("D:/work/GHG_move/EDGAR/IPCC_food/code")
source('D:\\work\\GHG_move/EDGAR/IPCC_food/code/sankey_auxiliary.R')

d.dir <- 'D:\\work\\GhG_move\\EDGAR\\IPCC_food\\'
#d.dir <- 'E:\\D_extra\\work\\GhG_move\\EDGAR\\IPCC_food\\'
data  <- read.csv(file=paste0(d.dir,'data\\by_ipcc_detailed_and_food_sectors_GAS_detailed_07MAy2020_with_LULUC_1.csv'), stringsAsFactors = F,
                  blank.lines.skip=T)

# run withount China
 # data <- data[-which(data$Country_code_A3 == 'CHN'),]

#all_NA <- apply(data,1,function(z) all(is.na(z)))
#data <- data[!all_NA,]

# where to classify China ? 
# data[data$Country_code_A3=='CHN','dev_country'] <- 'I'
# 

country_group <- c('World','Developing','Industrialised','EU28')

# CONTROLLING INPUTS 
# ---- set these
country <- country_group[2]
bSankey <- T # WHEN true RUNS FOR 2015 ONLY
#*********

if (bSankey) {
  sYear0 <-  '2015' #1990' #c'1990'  #
} else {
  sYear0 <- c('2015', '2010',  '2005', '2000',  '1995', '1990')
}

hold_df <- NULL
for (y in 1:length(sYear0)){
  sYear <- paste0('Y_',sYear0[y])
  
  data.y        <- data[,1:13]; data.y <- cbind(data.y, data[,sYear ])
  names(data.y) <- c(names(data[1:13]),'yearN')
  
  data.y$yearN <- as.numeric(as.character(data.y$yearN) )
  
  
  if (country=='Developing'){
    data.y <- data.y[data.y$dev_country  =='D',]
  } else if (country=='Industrialised'){
    data.y <- data.y[data.y$dev_country  =='I',]
    
  } else if (country=='EU28'){
    data.y <- data.y[data.y$Name %in% EU28,]
  } else if (country=='World'){
    data.y <- data.y
  }
  # 
  
  bAgg <- T
  if (bAgg){
    idx <- NULL; hold <- NULL
    for (i in 1:6){ 
      
      emi.agg <- paste(paste0(c('^RE','^TR','^PR'), as.character(i)), collapse='|')
      
      data.p <- data.y %>% group_by(Country_code_A3, GHGs) %>% 
        subset (grepl(emi.agg,EDGAR_Short)) %>% 
        summarise(yearN = sum(yearN,na.rm=T))
      
      
      
      if (i==1) { 
        idx <- c(idx, which(grepl(emi.agg, data.y$EDGAR_Short)))
        data.p$EDGAR_Short                <- rep('Agg1', dim(data.p)[1])
        data.p$EDGAR_Sector               <- rep('Ener for households', dim(data.p)[1])
        data.p$FOOD_system_stage           <- rep('Consumption', dim(data.p)[1])
        data.p$FOOD_system_stage_detailed <- rep('Consumption', dim(data.p)[1])
        data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
        
        data.p <- data.frame(data.p)
        # browser()
        
      }
      if (i==2) { 
        idx <- c(idx, which(grepl(emi.agg, data.y$EDGAR_Short)))
        data.p$EDGAR_Short                <- rep('Agg2', dim(data.p)[1])
        data.p$EDGAR_Sector               <- rep('Ener for packaging', dim(data.p)[1])
        data.p$FOOD_system_stage           <- rep('Distribution', dim(data.p)[1])
        data.p$FOOD_system_stage_detailed <- rep('Packaging', dim(data.p)[1])
        data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
        
        data.p <- data.frame(data.p)
      }
      
      if (i==3) { 
        idx <- c(idx, which(grepl(emi.agg, data.y$EDGAR_Short)))
        data.p$EDGAR_Short                <- rep('Agg3', dim(data.p)[1])
        data.p$EDGAR_Sector               <- rep('Ener for production', dim(data.p)[1])
        data.p$FOOD_system_stage           <- rep('Processing', dim(data.p)[1])
        data.p$FOOD_system_stage_detailed <- rep('Processing', dim(data.p)[1])
        data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
        
        data.p <- data.frame(data.p)
      }  
      if (i==4) { 
        idx <- c(idx, which(grepl(emi.agg, data.y$EDGAR_Short)))
        data.p$EDGAR_Short                <- rep('Agg4', dim(data.p)[1])
        data.p$EDGAR_Sector               <- rep('Ener for agr. fishing', dim(data.p)[1])
        data.p$FOOD_system_stage           <- rep('Production', dim(data.p)[1])
        data.p$FOOD_system_stage_detailed <- rep('Production', dim(data.p)[1])
        data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
        
        data.p <- data.frame(data.p)
      }  
      if (i==5) { 
        idx <- c(idx, which(grepl(emi.agg, data.y$EDGAR_Short)))
        data.p$EDGAR_Short                <- rep('Agg5', dim(data.p)[1])
        data.p$EDGAR_Sector               <- rep('Ener for retail', dim(data.p)[1])
        data.p$FOOD_system_stage           <- rep('Distribution', dim(data.p)[1])
        data.p$FOOD_system_stage_detailed <- rep('Retail', dim(data.p)[1])
        data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
        
        data.p <- data.frame(data.p)
      }  
      if (i==6) { 
        idx <- c(idx, which(grepl(emi.agg, data.y$EDGAR_Short)))
        data.p$EDGAR_Short                <- rep('Agg6', dim(data.p)[1])
        data.p$EDGAR_Sector               <- rep('Ener for transport', dim(data.p)[1])
        data.p$FOOD_system_stage           <- rep('Distribution', dim(data.p)[1])
        data.p$FOOD_system_stage_detailed <- rep('Transport', dim(data.p)[1])
        data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
        
        data.p <- data.frame(data.p)
      }  
      hold <- rbind(hold, data.p)
    }
    
    data.n <- data.y[-idx,]
    
    data.n1 <- merge(data.n,hold, by=intersect(names(data.n),names(hold)), all = T)
    rm(data.p); idx <- NULL; emi.agg <- NULL
    
    # Now aggregate EN0, EN6, EN8,EN9 ----
    emi.agg <- paste(paste0(c('EN'), as.character(c(0,6,8,9))), collapse='|')
    
    data.p <- data.n1 %>% group_by(Country_code_A3, GHGs) %>% 
      subset (grepl(emi.agg,EDGAR_Short)) %>% 
      summarise(yearN = sum(yearN,na.rm=T))
    
    idx <- which(grepl(emi.agg, data.n1$EDGAR_Short))
    
    data.p$EDGAR_Short                <- rep('AggM', dim(data.p)[1])
    data.p$EDGAR_Sector               <- rep('Electricity', dim(data.p)[1])
    data.p$FOOD_system_stage          <- rep('Distribution', dim(data.p)[1])
    data.p$FOOD_system_stage_detailed <- rep('Packaging', dim(data.p)[1])
    data.p$FOOD_system_compartment    <- rep('Energy', dim(data.p)[1])
    
    data.p <- data.frame(data.p)
    data.n1 <- data.n1[-idx,]
    
  } else {
    data.n1 <- data.y # imp: uncomment this one if bAgg is FALSE
  }
  
  # aggregate F-gases
  strFg   <- paste(c('^HF','^SF'), collapse='|')
  data.p1 <- data.n1 %>% group_by(Country_code_A3, EDGAR_Sector,
                                  FOOD_system_stage,	FOOD_system_stage_detailed,	
  ) %>% 
    subset(grepl(strFg,GHGs)) %>% summarise(yearN =sum(yearN,na.rm=T))
  
  data.p1$FOOD_system_compartment <- rep('Industry', dim(data.p1)[1])
  data.p1$GHGs       <- rep('F-gases', dim(data.p1)[1])
  
  data.n1 <- data.n1[-grep(strFg,data.n1$GHGs),]
  
  data.n2 <- merge(data.n1, data.p1, by=intersect(names(data.n1),names(data.p1)), all = T )
  
  dataf <- data.n2 %>% group_by(GHGs, EDGAR_Sector, 
                                # IPCC_for_std_report_detailed, 
                                FOOD_system_stage,	FOOD_system_stage_detailed,	FOOD_system_compartment)  %>% 
    summarise(flux= sum(yearN, na.rm=T))
  
  dataf <- as.data.frame(dataf, stringAsFactors=F)
  # remove negative
  # dataf <- dataf[-which(dataf$flux<0),]
  if(country == 'Developing'){dataf <- dataf[-which(dataf$GHGs=='F-gases'),]} # manually remove F-gases from developing countries
  
  dataf$freq <- dataf$flux/sum(dataf$flux, na.rm=T)
  
  dataf$GHG2 <- dataf$GHGs
  dataf <- data.frame(dataf)
  
  bPlot <-  T
  
  if(bPlot){
    
    write.csv(dataf, file= paste0(d.dir,'out\\prep_file_',country, '_LULUC_',sYear,'.csv') )
    
    
    # dataf$EDGAR <- which(dataf$EDGAR_SECTOR %in% long_names)
    
    #https://towardsdatascience.com/recreating-more-data-visualizations-from-the-book-knowledge-is-beautiful-part-iv-686938a84c9e
    # library(devtools)
    # devtools::install_github("Displayr/flipPlots", dependencies = F)
    
    #library(flipPlots)# reorder the df
    #crash$total_crashes <- rep("YES", 25)# plot
    #SankeyDiagram(dataf[,-c(2,6)], link.color = "Source", label.show.varname = F, max.categories = 28)#, weights = crash$freq)
    
    #   idx <- which(dataf$freq < 0.005)
    #   dataf0 <- dataf %>% group_by(EDGAR_Sector) %>% filter(sum(freq)>0.01)
    #    dataf0 <- dataf[-idx,]
    dataf0 <- dataf
    #  dataf0 <- dataf[dataf$freq>=0.005,]
    dataf0 <- dataf0[, c(1,5,3,4,2,6,7,8)]
    
    if ( country=='World' |  country== 'Industrialised'){
      
      lev0 <- c('LULUC','Agriculture','Enteric Ferment','Manure','Indirect N2O',
                'Ener for agric. fishing','Ener for household','Ener for production','Ener for transport',
                'Ener for retail',"Chemicals","Industrial" , 'Residential',"Products" , "Road Transport",
                "Solid waste" ,"Waste water" )
      
    } else if (country == 'Developing') {
      lev0 <- c('LULUC','Agriculture','Enteric Ferment','Manure','Indirect N2O',
                'Ener for agric. fishing','Ener for household','Ener for production','Ener for packaging',
                "Chemicals","Industrial" , 'Residential', "Road Transport",
                "Solid waste" ,"Waste water" )
    }
    #    dataf0$EDGAR_Sector[is.na(dataf0$EDGAR_Sector)] <- 'Other'
    dataf0$EDGAR_Sector[!(as.character(dataf0$EDGAR_Sector)) %in% lev0] <- 'Other'
    
    
    dataf0$FOOD_system_compartment <- factor(dataf0$FOOD_system_compartment, 
                                             levels=c('Landbased','Energy','Industry','Waste'))
    
    dataf0$FOOD_system_stage_detailed <- factor(dataf0$FOOD_system_stage_detailed, 
                                                levels=c('LULUC','Production','Transport','Processing','Packaging',
                                                         'Retail','Consumption','End of Life') )
    if ( country=='World' |  country== 'Industrialised'){
      dataf0$EDGAR_Sector <- factor(dataf0$EDGAR_Sector, 
                                    levels= c(lev0, 'Other' ))
    } else if (country == 'Developing') {
      dataf0$EDGAR_Sector <- factor(dataf0$EDGAR_Sector, 
                                    levels= c(lev0, 'Other' ))
    }
    #    dataf0 <- dataf0[, c(1,5,3,4,2,6,7,8)]
    #    dataf0 <- na.omit(dataf0)
    
    # dataf2 <- dataf0
    # other_fluxes <- sum(dataf2[is.na(dataf2$EDGAR_Sector),'flux'])
    # 
    # a <- dataf2 %>% group_by(FOOD_system_compartment, FOOD_system_stage, FOOD_system_stage_detailed) %>% filter(is.na(EDGAR_Sector))
    # for (i in 1: nrow(a)){
    #   
    #   idx <- which(dataf2$GHGs==a$GHGs[i] & dataf2$FOOD_system_compartment ==a$FOOD_system_compartment[i] &
    #                             dataf2$FOOD_system_stage_detailed == a$FOOD_system_stage_detailed[i] & dataf2$GHG2 == a$GHG2[i])
    #   tot <- sum(dataf2[idx,'flux'])
    #   dataf2[idx[1], 'flux'] <- dataf2[idx[1], 'flux'] + tot
    #   
    # }
    
    
    
    SD     <- SankeyDiagram(dataf0[,-c(3, 6,7)], link.color = "Source", output.data.only = T,
                            label.show.varname = F, max.categories = 100,
                            hovertext.show.percentages=T,variables.share.values = F,
                            label.show.percentages = T, node.width = 13, 
                            node.padding = 10, colors=NULL,
                            font.size = 12, font.family = "Verdana", 
                            weights = dataf0$freq, sinks.right = F)
    
    if (sYear == 'Y_1990') {save(SD, file=paste0(d.dir,'out\\prep_file_',country, '_LULUC_',sYear,'.Rdata'))}
    
    
    
    tmp <- f.Get_trends(SD, country)
    SD$nodes$name <- tmp
    
    
    # d3_category20 to hex
    # check the number of nodes and make sure that the last four are the same as the first four 
    
    # for 41 nodes you can define 18 colors that repeats twice, 
    # add one more to get to 37. the function will use the first four to cover for the missing ones 
    # i.e. from 37 to 41. in this way the first and last four will repeat and the GHG gases have
    # same color
    
    # for 38 nodes you can define 16 colors that repeats twice, 
    # add two more to get to 34. the function will use the first four to cover for the missing ones 
    # i.e. from 37 to 41. in this way the first and last four will repeat and the GHG gases have
    # same color
    
    my.colors <- c(
      '#1f77b4', '#aec7e8',
      '#ff7f0e', '#ffbb78',
      #      '#2ca02c', '#98df8a',
      #     '#d62728', '#ff9896',
      
      '#008800','#ff9900','#aaaaaa','#ffcc33' ,#landbased,energy,industry,waste
      '#548235','#00CC00','#203864','#315597','#5A82CA','#3399ff','#33ccff','#999999', #luluc,prod,transp,proc,pack,retail,consump,endoflife
      #      '#548235','#85bd5', '#203864', '#315597','#5A82CA','#97B0D','#C9D6E','#999999',
      '#9467bd', '#c5b0d5',
      '#8c564b', '#c49c94',
      '#e377c2', '#f7b6d2',
      '#7f7f7f', '#c7c7c7',
      '#bcbd22' , '#dbdb8d', 
      '#17becf' , '#9edae5') # 
    
    # get number of nodes
    if(country != 'Developing'){ 
      nNodes <- dim(SD$nodes)[1] - 4 # remove four nodes (initial 4 GH gases)
    } else {
      nNodes <- dim(SD$nodes)[1] - 3 # F-gases have share less than one in developing countries
      my.colors <- my.colors[-3]
    }
    ratio <-  nNodes/length(my.colors)
    my.palette <- paste(c(rep(my.colors,floor(ratio)),my.colors[1:(nNodes-length(my.colors))]) ,collapse = '", "')
    
    
    colorJS <- paste('d3.scaleOrdinal(["', my.palette, '"])')
    
    if(country=='World')         {mar0 <- list(top=-150, right=-400, left=-250,  bottom=150)}
    if(country=='Industrialised'){mar0 <- list(top=-110, right=-350, left= -200, bottom=150)}
    if(country=='Developing')    {mar0 <- list(top=-250, right=-550, left=-450,  bottom=150)}
    
    sankey <- sankeyNetwork(SD$links,SD$nodes,Source = 'source',
                            Target = 'target', Value = 'value', NodeID = 'name', 
                            sinksRight = FALSE,
                            fontSize = 24, fontFamily = "Verdana", 
                            LinkGroup = 'group', NodeGroup = 'group',
                            colourScale =colorJS,
                            nodeWidth = 20, nodePadding = 20 ,
                            iterations=0 , margin = mar0) #list(top=-250, right=-700, left=-400, bottom=150))
    
    # https://stackoverflow.com/questions/50132459/how-to-add-title-to-a-networkd3-visualisation-when-saving-as-a-web-page 
    # add title and text
    
    sankey <- htmlwidgets::prependContent(sankey, htmltools::tags$h1(paste0('Country Group: ', country,';  Year: ',sYear0[y], '(excluding China)' )))
    #sankey <- htmlwidgets::appendContent(sankey, htmltools::tags$p("Including China"))
    sankey <- htmlwidgets::prependContent(sankey, htmltools::includeMarkdown("header.txt"))
    sankey
    
  }
  
  dataf$year <- sYear
  hold_df <- rbind(hold_df,dataf) 
  rm(dataf)
}
sankey

f.create_time_series (hold_df, country)



##

write.csv(hold_df, file=paste0(d.dir,'out\\hold_TimeSeries_',country,'.csv') )

hold_df %>% filter(GHGs=='CO2' & !EDGAR_Sector == 'LULUC' & FOOD_system_compartment == 'Landbased') %>% 
  group_by(year) %>% summarise(landCO2Tot= sum(flux))

