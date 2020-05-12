
# library(ggalluvial)
library(dplyr)
# library(ggfittext)
library(networkD3)
library(flipPlots)
library(pagedown)
library(manipulateWidget)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(webshot) # https://github.com/wch/webshot
library(reshape2)
library(directlabels)

EU28 <-c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic',
         'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary',
         'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg','Malta', 'Netherlands',
         'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'United Kingdom')


d.dir <- 'D:\\work\\GhG_move\\EDGAR\\IPCC_food\\'

#----
loadRData <- function(fileName){
  # copied from http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file 
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
} # end function


#----

f.Get_trends <- function(SD_2015, country)
{
  
  # debug
  # SD_2015 <- SD
  
  SD_1990 <- loadRData(file=paste0(d.dir,'out\\prep_file_',country, '_LULUC_Y_1990.Rdata'))
  
  idx_eol <- grep('End of Life',SD_2015$nodes$name)
  
  tmp.now <- SD_2015$nodes$name[1:idx_eol] # 
  tmp.now <- as.character(tmp.now)
  
  tmp.ref  <- SD_1990$nodes$name[1:idx_eol] 
  tmp.ref <- as.character(tmp.ref)
  
  # get text
  # text between start and position of first parenthesis
  
  text.now <- as.character(sapply (tmp.now, function(x){substr(x, 1, (gregexpr(pattern ='\\(',x , perl=T)[[1]][1])-1)}))
  text.ref <- as.character(sapply (tmp.ref, function(x){substr(x, 1, (gregexpr(pattern ='\\(',x , perl=T)[[1]][1])-1)}))
  
  idx_odd <- which(!text.now %in% text.ref) # entry in recent year that is not present in previous years
  
  # get percentage
  # text between '(' and ')'
  
  perc.now <- as.numeric(as.character(sapply (tmp.now, function(x){substr(x, (gregexpr(pattern ='\\(',x, perl=T )[[1]][1])+1, 
                                                                          (gregexpr(pattern ='\\)',x , perl=T)[[1]][1])-2)})))
  perc.ref <- as.numeric(as.character(sapply (tmp.ref, function(x){substr(x, (gregexpr(pattern ='\\(',x, perl=T )[[1]][1])+1, 
                                                                          (gregexpr(pattern ='\\)',x , perl=T)[[1]][1])-2)})))
  
  if (length(idx_odd) >=1){
    for (i in 1:length(idx_odd)){
      perc.ref <- append(perc.ref,NA, idx_odd[i]-1) # add NA to the position of missing element
      text.ref <- append(text.ref,text.now[idx_odd[i]],idx_odd[i]-1) # add missing element to text
    }
    perc.ref <- perc.ref[-c((idx_eol+1):(idx_eol+length(idx_odd)))]
    text.ref <- text.ref[-c((idx_eol+1):(idx_eol+length(idx_odd)))]
  }
  
  
  hold <- data.frame('cat.now'=text.now, 'cat.ref'=text.ref, 'perc.now'=perc.now, 'perc.ref'=perc.ref)
  hold$var <- 100*(hold$perc.now-hold$perc.ref)/hold$perc.ref
  hold$arrow <- rep('', length(hold$var))
  
  idx <- which ( hold$var >0 )
  hold$arrow[idx] <-  paste0('\u2191', '+', round(hold$var[idx],0),'%') 
  idx <- which ( hold$var <0 )
  hold$arrow[idx] <-  paste0('\u2193',  round(hold$var[idx],0),'%')
  
  hold$arrow[which( hold$var ==0)]  <- paste0('\u2194', '0%')
  hold$arrow[which((is.infinite(hold$var) & (hold$perc.now > hold$perc.ref)) | is.na(hold$var))] <- paste0('\u2191', '>100%')
  hold$arrow[which((is.infinite(hold$var) & (hold$perc.now < hold$perc.ref)))] <- paste0('\u2193', '>100%')
  
  tmp <- as.character(SD_2015$nodes$name)
  
  tmp[1:idx_eol] <- paste0(tmp.now,' [',hold$arrow, ']')
  #  tmp <- paste0(tmp.now,' [',hold$arrow, ']')
  tmp <- as.factor(tmp)
  
  #   a <- substr(tmp.ref, nchar(tmp.ref)-3, nchar(tmp.ref)-1 )
  #   tmp[2] <- ('xxxx \n +0.66% \u2191')
  #   tmp[2] <-paste('abc', '<br>', 'abcd')
  #   tmp <- as.factor(tmp)
  #   SD$nodes$name <- tmp
  return(tmp)
}


#----
f.create_time_series <- function(hold_df,country) {
  
  a <- dcast(hold_df, FOOD_system_stage_detailed ~ year, value.var = 'flux', fun.aggregate = sum)
  a<- melt(a)
  a$year <- as.numeric(substr(a$variable,3,7))
  figure1 <-  ggplot(a, aes(x=year, y=value, color=FOOD_system_stage_detailed))+geom_line(size=1.5)+
    ggtitle(paste0(country)) + scale_x_continuous(breaks=seq(1990,2020,5), limits=c(1990,2020)) +
    theme(text=element_text(color="grey50")) +theme_bw() +
    theme(axis.title=element_text(size=15)) +
    theme(plot.title=element_text(size=20, color="steelblue")) +  ylab( "GHG emissions") +  
    theme(    axis.title.x = element_blank())
  figure2 <- print(direct.label(figure1, method=list("last.points", hjust=c( -0.1,-0.2, -0.1, -0.2), 
                                                     vjust = c(0, # consumption
                                                               -1.5, # end of life
                                                               -0, # LULUC
                                                               0.1, # packaging
                                                               0.85, # processing
                                                               0,   # production
                                                               -0.8, # retail
                                                               -0.8  # transport
                                                     )) ))
  
  ggsave(figure2, file=paste0(d.dir, 'figs\\TS_stage_detailed_', country,'.png'))
}
# 
# write.csv(hold_df, file=paste0(d.dir, 'proc\\TS_sankey_test.csv' ))
# idx <- which(grepl('FOOD_system_stage_detailed',names(hold_df)))
# hold_df <- data.frame(hold_df)
# SankeyDiagram(hold_df[,idx], 
#               link.color = "Source", output.data.only = F,
#               label.show.varname = F, max.categories = 50,
#               hovertext.show.percentages=F,variables.share.values = F,
#               label.show.percentages = TRUE, node.width = 13, 
#               node.padding = 10, colors=NULL,
#               font.size = 12, font.family = "Verdana", 
#               sinks.right = F)
# 
# }



#   onRender(sankey,
#            '
# function(el,x) {
#   d3.selectAll(".node text").remove()
#   d3.selectAll(".node")
#     .append("foreignObject")
#     .attr("width", 100)
#     .attr("height", 50)
#     .html(function(d) { return d.name; })
# }
# '
#   )