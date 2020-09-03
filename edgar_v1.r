require(data.table)
require(openxlsx)
require(ggplot2)

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/projects/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "edgar")

edgar_folder <- paste0("C:/Users/", myuser, "/google/projects/edgar/Table2/")

## FUNCIONS #

xround <- function(x){
  xround <- round(x * 10^(-round(log10(x))), 1) * 10^(round(log10(x)))
  return(xround)
}

# Data for sectorial contribution
loadedgardata201911 <- function(){
  edgar_folder <- paste0("C:/Users/", myuser, "/google/projects/edgar/emi_by_IPCC/")
  edgar_files <- list.files(path=edgar_folder)
  
  edgarsector <- data.table()
  
  for (edgar_file in edgar_files){
    cat("\n", edgar_file)
    newdata <- fread(paste0(edgar_folder, edgar_file), skip=1, header=TRUE)
    
    newdataname <- gsub("_SECTORS", "", edgar_file)
    newdataname <- gsub("_AR5", "", newdataname)
    newdataname <- gsub(".csv", "", newdataname)
    newdataname <- strsplit(gsub("IPCC_based_emissions_", "", newdataname), "_")[[1]]
    gas <- newdataname[3]
    newdata$gas <- gas
    system <- newdataname[2]
    if(system=="ALL"){
      food <- "all"
      newdata$FOOD_chain <- 'Nonfood'
      newdata$FOOD_system <- 'Nonfood'
    }else{
      food <- "food"
    }
    newdata$food <- food
    
    setnames(newdata, 
             c("Country_code_A3", "Substance", "IPCC_for_std_report", "FOOD_chain", "FOOD_system"), 
             c("country", "unit", "category", "stage", "system"))
    idcols <- c("country", "gas", "category", "food", "stage", "system")
    years <- gsub("Y_", "", names(newdata)[grepl("Y_", names(newdata))])
    keepcols <- c(idcols, paste0("Y_",years))
    
    numcols <- paste0("Y_",years)
    newdata <- newdata[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]
    
    
    if(food == 0 & gas == "GHG"){
      #retrieve mappings
      countrycodes <- unique(newdata[, .(country, Name)])
      edgarcodes <- unique(newdata[, .(category, EDGAR_SECTOR, IPCC_for_std_report_desc)])
      
    }
    
    
    newdata <- melt.data.table(newdata[, keepcols, with=FALSE], id.vars=idcols, variable.name="year", na.rm=TRUE)
    edgarsector <- rbind(edgarsector, newdata)
  }
  edgarsector <- edgarsector[, sector := substr(category, start=1, stop=1)]
  edgarsector <- edgarsector[! is.na(value)]
  edgarsector <- edgarsector[, year := gsub("Y_", "", year)]
  #Convert from kt to Mt
  edgarsector <- edgarsector[, value := value/1000]
  save(edgarsector, file=paste0("C:/Users/", myuser, "/google/projects/edgar/edgardata.rdata"))
  return(edgarsector)
}
loadedgardata <- function(){
  
  crippa_folder <- paste0(manuscripts, "/crippa_ghgfoodsystems")
  edgar <- fread(paste0(crippa_folder, "/by_ipcc_detailed_and_food_sectors_GAS_detailed_feb2020.csv"))
  
  regions <- unique(edgar[, .(Country_code_A3, Name, C_group_IM24_sh, dev_country)])
  ipcc <- unique(edgar[, .(IPCC_for_std_report_detailed, IPCC_for_std_report_detailed_desc)])
  years <- names(edgar)[grepl("Y_", names(edgar))]
  edgar <- edgar[, -c("Name", "C_group_IM24_sh", "dev_country"), with=FALSE]
  
  setnames(edgar, names(edgar), gsub("FOOD_system_", "", names(edgar)))
  setnames(edgar, c("Country_code_A3", "Substance", "EDGAR_SECTOR", "IPCC_for_std_report_detailed", "IPCC_for_std_report_detailed_desc"), 
           c("country", "gas", "edgar", "category", "categorydesc"))
  
  edgar <- melt.data.table(edgar, measure.vars = years, variable.name = "year", value.name = "value")
  edgar <- edgar[, gas := gsub("GWP_100_", "", gas)]
  edgar <- edgar[, year := as.numeric(gsub("Y_", "", year))]
  global <- edgar[, sum(value, na.rm = TRUE)]

  classification <- unique(edgar[, .(category, categorydesc, edgar, stage, stage_detailed, compartment)])
  inputs <- unique(classification[stage=="Inputs", .(category, stage, stage_detailed)])
  stagedet <- unique(inputs$stage_detailed)
  inputs <- unique(classification[stage=="Inputs", .(category, stage)])
  inputs <- paste(inputs$category, collapse = ",")
  write.xlsx(classification, file = paste0(crippa_folder, "/202002001_classification.xlsx"))
  write.csv(inputs, file = paste0(crippa_folder, "/202002001_inputs.csv"))

  lulucfile <- paste0(crippa_folder, "/composed_food_emissions_from_lulucf_co2eq_country_sector_wide.csv")
  luluc <- fread(lulucfile, header = TRUE)
  lulucglobal <- luluc[, lapply(.SD, sum, na.rm=TRUE), .SDcols = 3:ncol(luluc)]
}


processedgar <- function(edx){

  edx <- dcast.data.table(edx, year + sector + gas ~ food, value.var="V1", fill=0)
  
  edx <- edx[all != 0, share := food/all]
  edx <- merge(edx, codemap[, .(code, description)], by.x="sector", by.y="code", all.x=TRUE)
  edx <- edx[sector=="Total", description := ""]
  edx <- edx[, sector := paste0(sector, " ", description)]
  edx <- edx[, sector := gsub(" \\(please specify\\)", "", sector)]
  edx <- edx[, sector := gsub(",", "", sector)]
  edx <- edx[, (roundcols) := xround(.SD), .SDcols = roundcols]
  edx <- edx[is.nan(share), share := 0]
  edx <- edx[is.nan(food), food := 0]
  #  edx <- edx[, Unit:="[Mt gas yr-1]"]
  #  edx <- edx[grepl("Total", sector), Unit:="[Mt CO2e yr-1]"]
}
writeoutedgar <- function(edx, bywhat){
  gascols <- c("CO2", "CH4", "N2O", "F-gas", "GHG")
  datacols <- c(paste0("food_", gascols), paste0("share_", gascols))
  sortcols <- c("year", "sector",datacols)
  edgarTable <- dcast.data.table(edx, year + sector ~ gas, value.var=c("food", "share"), fill=0)
  edgarTable <- edgarTable[, c(sortcols), with=FALSE]
  
  
  conFil <- file(description=paste0("C:/Users/", myuser, "/google/projects/edgar/foodsystem_emissions_", bywhat, "_", 
                                    format(Sys.time(), "%Y%m%d%H"),".csv"), open="w")
  write.table(paste(gsub("food_|share_", "", setdiff(sortcols, "year")), collapse=","), 
              quote=FALSE, na="", row.names=FALSE, sep=",", col.names=FALSE, conFil)
  write.table(paste(c("", rep("Emissions [Mt gas yr-1]", 5),rep("Share of total [%]", 5)), collapse=","), 
              quote=FALSE, na="", row.names=FALSE, sep=",", col.names=FALSE, conFil)
  
  write.table(",1990", quote=FALSE, na="", row.names=FALSE, col.names=FALSE,conFil)
  write.table(edgarTable[order(sector)][year==1990, setdiff(sortcols, "year"), with=FALSE], 
              quote=FALSE, na="", row.names=FALSE, sep=",", col.names=FALSE, conFil)
  
  write.table(",2015", quote=FALSE, na="", row.names=FALSE, col.names=FALSE,conFil)
  write.table(edgarTable[order(sector)][year==2015, setdiff(sortcols, "year"), with=FALSE], 
              quote=FALSE, na="", row.names=FALSE, sep=",", col.names=FALSE, conFil)
  close(conFil)
  
  return(edgarTable)
}


codemap <- fread(paste0("C:/Users/", myuser, "/google/projects/edgar/ipcc_ar6_edgar_data_gwp100_classifications_codes.csv"))
regionmap <- fread(paste0("C:/Users/", myuser, "/google/projects/edgar/ipcc_ar6_edgar_data_gwp100_classifications_regions.csv"))

edgarsector <- loadedgardata()
countries <- unique(edgarsector$country)
units <- unique(edgarsector$unit)
gases <- unique(edgarsector$gas)
categories <- sort(unique(edgarsector$category))
stages <- unique(edgarsector$stage)
systems <- unique(edgarsector$system)
years <- unique(edgarsector$year)
sectors <- unique(edgarsector$sector)

roundcols <- c( "all", "food", "share")
edgarall1 <- edgarsector[year%in%c(1990, 2015), sum(value, na.rm=TRUE), by=c("year", "gas", "sector", "food")]
edgarall2 <- edgarsector[year%in%c(1990, 2015), sum(value, na.rm=TRUE), by=c("year", "gas", "category", "food")]
edgarTotal <- edgarsector[year%in%c(1990, 2015), sum(value, na.rm=TRUE), by=c("year", "gas", "food")]
edgarTotal <- edgarTotal[, sector:="Total"]
edgarall1 <- rbind(edgarall1, edgarTotal[, .(year, gas, sector, food, V1)])
edgarall2 <- rbind(edgarall2[, .(year, gas, sector=category, food, V1)], edgarTotal[, .(year, gas, sector, food, V1)])

edgarall1 <- processedgar(edgarall1)
edgarall2 <- processedgar(edgarall2)

sectortable <- writeoutedgar(edgarall1, "sector")
categorytable <- writeoutedgar(edgarall2, "category")


absTotalFood <- sectortable[year==2015 & grepl("T", sector), food_GHG]
shareTotal <- sectortable[year==2015 & grepl("T", sector), share_GHG]
contrSectors <- sectortable[year==2015, food_GHG/absTotalFood]
embycat <- categorytable[, category:=substr(sector, 1, 4) ]
embycat <- embycat[category:=gsub(" .*", "", category)]
embycat <- embycat[year==2015]
embycat[grepl("^1", sector), .(sector, food_GHG, category)]



#Detailed food system data all sectors - available for all countries and years
#All values in GWP_100
#edgar_folder <- paste0("C:/Users/", myuser, "/google/projects/edgar/emi_by_IPCC/IPCC_based_emissions_GHG_AR5_FOOD.csv")
edgar_folder <- paste0("C:/Users/", myuser, "/google/projects/edgar/emi_by_IPCC/by_ipcc_detailed_and_food_sectors.csv")

details <- fread(paste0(edgar_folder), skip=1, header=TRUE)
details <- details[, c(names(details)[!grepl("^Y", names(details))], "Y_2015"), with=FALSE]
details <- details[, .(country=Country_code_A3, cgroup = C_group_IM24_sh, dev = dev_country, category=IPCC_for_std_report_detailed, chain=FOOD_chain, system=FOOD_system, value=Y_2015)]
globdetails <- details[, .(value=sum(value, na.rm=TRUE)), by=.(category, chain, system)]
globdetails <- globdetails[, value:=value/1000]
globdetails <- globdetails[, value:=xround(value)]
globdetails <- globdetails[is.nan(value), value:=0]

manufact <- globdetails[grepl("^1A2", category), .(category, value, subcat=substr(category, 1, 4))]
manufact <- manufact[, .(value=sum(value)), by=.(subcat)]
manufact <- manufact[, share := value / sum(value)]

othersectors <- globdetails[grepl("^1A4", category), .(category, value, subcat=substr(category, 1, 4))]
othersectors <- othersectors[, sum(value, na.rm=TRUE), by="subcat"]
othersectors <- othersectors[, share := V1 / sum(V1)]
othersectors <- othersectors[, value := xround(V1)]

transport <- globdetails[grepl("1A3|1C", category), .(category, value, subcat=substr(category, 1, 3))]
transport <- transport[, share := value/sum(value)]
transport <- rbind(transport, transport[, .(category = "total", value=sum(value), subcat = "total", share=sum(share))])
transport <- transport[, share:= round(share*100, 3)]
transport[order(share, decreasing=TRUE)]
round(transport[category=="total", value]/sectortable[year==2015 & grepl("Tot", sector), food_GHG], 3)
write.csv(transport, paste0("C:/Users/", myuser, "/google/projects/edgar/transportglobal_", format(Sys.time(), "%Y%m%d%H"),".csv"))

industry <- globdetails[grepl("^2", category), .(category, value, subcat=substr(category, 1, 3))]
industry <- industry[!grepl("^2B", category), subcat := substr(category, 1, 2)]
industry <- industry[, .(value=sum(value)), by=.(subcat)]

waste <- globdetails[grepl("^6", category), .(category, value, subcat=substr(category, 1, 3))]
waste <- waste[grepl("^6C|6D", subcat), subcat := "6CD"]
waste <- waste[, .(value=sum(value)), by=.(subcat)]
waste <- waste[, share := value/sum(value)]
waste <- rbind(waste, waste[, .(subcat = "total", value=sum(value), share=sum(share))])


# Applying AR5-GWP100: CH4 28, N2O 265
#edgarlevel1 <- edgarlevel1[, CO2eq := 28*CH4 + 265*N2O + CO2]
#edgarlevel1 <- edgarlevel1[, Fgases := GHG - CO2eq]


resdata <- function(){
  resCon <- file(description=paste0("C:/Users/", myuser, "/google/projects/edgar/foodsystem_data4report_", 
                                    format(Sys.time(), "%Y%m%d%H"),".csv"), open="w")
  gases <- c("CO2",H4", "N2O", "F-gases")
  
  
  writeLines(paste0("Share food system Total in 2015: ", shareTotal), resCon)
  writeLines(paste0("Share sectors to food system Total in 2015: ", paste(unique(sectortable$sector), contrSectors)), resCon)
  
  writeLines(paste0("Share supply chain in 2015: ", (1-contrSectors[4])))
  writeLines(paste0("Share supply chain in 2015: ", round((1-contrSectors[4]-contrSectors[5])/(1-contrSectors[5]),2)), resCon)
  
  writeLines(paste0("Emissions from manufacturing sector in 2015: ", embycat[year==2015 & grepl("^1A2", sector), food_GHG]), resCon)
  writeLines(paste0("Emissions from 'Other sectors' in 2015: ", embycat[year==2015 & grepl("^1A4", sector), food_GHG]), resCon)
  writeLines(paste0("Emissions from Agri-forestry-fishery in 2015: ", othersectors[subcat=="1A4c", value]), resCon)
  writeLines(paste0("Emissions from Residential in 2015: ", othersectors[subcat=="1A4b", value]), resCon)
  writeLines(paste0("Emissions from Commercial in 2015: ", othersectors[subcat=="1A4a", value]), resCon)
  
  
  writeLines(paste0("Emissions from transport in 2015: ", transport[category=="total", value]), resCon)
  writeLines(paste0("Emissions from energy sector in 2015: ", embycat[grepl("1A1", category), sum(food_GHG)]), resCon)
  writeLines(paste0("Emissions from other sources in 2015: ", embycat[grepl("1A5|1B", category), sum(food_GHG)]), resCon)
  
  writeLines(paste0("Manufacturing 1A2e - Food&beverages and Tobacco in 2015: ", manufact[subcat=="1A2e", value]), resCon)
  writeLines(paste0("Manufacturing 1A2d - Paper in 2015: ", manufact[subcat=="1A2d", value]), resCon)
  writeLines(paste0("Manufacturing 1A2b - Non-ferrous in metals 2015: ", manufact[subcat=="1A2b", value]), resCon)
  writeLines(paste0("Manufacturing 1A2a - Ferrous metals in 2015: ", manufact[subcat=="1A2a", value]), resCon)
  writeLines(paste0("Manufacturing 1A2f - Other in 2015 (what is it??): ", manufact[subcat=="1A2f", value]), resCon)
  
  writeLines(paste0("Transport - road: ", transport[category=="1A3b", share]+transport[category=="1A3bx", share]), resCon)
  writeLines(paste0("Transport - railway: ", transport[category=="1A3c", share]+transport[category=="1A3cx", share]), resCon)
  writeLines(paste0("Transport - navigation: ", transport[category=="1A3d", share]+transport[category=="1A3dx", share]+transport[category=="1C2", share]), resCon)
  writeLines(paste0("Transport - aviation: ", transport[category=="1A3a", share]+transport[category=="1C1", share]), resCon)
  
  writeLines(paste0("Industry - ammonia: ", industry[subcat=="2B1", value]), resCon)
  writeLines(paste0("Industry - halocarbons and sulphur hexafluoride: ", industry[subcat=="2B5", value]), resCon)
  
  
  
  close(resCon)
}
resdata()


trend <- edgarsector[gas=="GHG", .(value=sum(value)), by=.(sector, food, year)]
trend2015 <- trend[year==2015]
trend <- trend[sector != "4", sector:= "0"]
trend <- trend[, .(value=sum(value)), by=.(sector, food, year)]
trend <- dcast.data.table(trend, food + sector ~ year, value.var="value")
years

trendshare <- melt.data.table(trend[food=="food"], id.vars=1:2)
trendshare <- dcast.data.table(trendshare, variable~sector, value.var="value")
trendshare <- trendshare[, .(year=variable, 
                             Agri = `4`/2,
                             Nagri = `4`+`0`/2,
                             
                             Agshare=round(100*`4`/(`0`+`4`), 0), 
                             Otshare=round(100*`0`/(`0`+`4`), 0))]
trendshare <- trendshare[year %in% seq(1970, 2015, 5)]


plot(years, trend[food=="food" & sector==4, years, with=FALSE])
plot(years, as.matrix(t(trend[food=="food", years, with=FALSE])))


time <- rep(as.numeric(years), 2)
value <- c(t(as.vector(trend[food=="food" & sector=="4", years, with=FALSE])),
           t(as.vector(trend[food=="food" & sector=="0", years, with=FALSE])))
Sector <- c(rep("Agriculture", length(years)), rep("Other", length(years)))
data <- data.frame(time, value, Sector)
data$Sector <- factor(data$Sector, levels=rev(levels(data$Sector)))
data
p <- ggplot(data, aes(x=time, y=value, fill=Sector)) + 
  geom_area() + 
  annotate(geom="text", x=as.numeric(as.vector(trendshare$year)), y=trendshare$Agri, 
           label=paste0(trendshare$Agshare, "%")) +
  annotate(geom="text", x=as.numeric(as.vector(trendshare$year)), y=trendshare$Nagri, 
           label=paste0(trendshare$Otshare, "%")) +
  xlab("Year") + 
  ylab(expression(Food~System~GHG~emissions~"["~CO["2e"]~yr^{"-1"}~"]"))
  #ylab(bquote('Food system GHG emissions ['*, CO[2e] ~ yr^-1*']'))

p

png(filename = paste0("C:/Users/", myuser, "/google/projects/edgar/Shareagri-nonagri", format(Sys.time(), "%Y%m%d%H"), ".png"), width=10000, height=5000, units="px", res=1000)
print(p)
dev.off()



