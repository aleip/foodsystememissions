require(data.table)
require(openxlsx)
require(ggplot2)

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "edgar")
edgar_folder <- paste0(google, "/projects/edgar/data/202005/")
fao_folder <- paste0(google, "/projects/faostat_landuse/")

openEmissionFiles <- function(){
  
  ##################################################################################################### 
  # Food system elements
  # 
  # A: Food system emissions from all sectors except Land Use/Land Use Cange/Forestry. 
  #    Source: EDGAR
  #    Version: 07/05/2020 (sent by MC)
  #    File: GHG food system including LULUC (final dataset FAO).xlsx
  # B: Non-food system emissions from all sectors except Land Use/Land Use Cange/Forestry.
  #    Source: EDGAR
  #    Version: 03/03/2020 uploaded to JRCBox by ES
  #    File: item_B_nonfoodEDGAR_aggregated.csv
  # C: Food system emissions from Land Use / Land Use Change
  #    Source: FAO
  #            - http://www.fao.org/faostat/en/#data/GF net forest conversion, within the forest land sub-domain  ;
  #            - http://www.fao.org/faostat/en/#data/GC peatland drainage, croplands
  #            - http://www.fao.org/faostat/en/#data/GG peatland drainage, grassland
  #            - http://www.fao.org/faostat/en/#data/GI Peatland fires, in the biomass burning subdomain
  #    Version: Dowloaded 06/05/2020 by ES - file composed_food_emissions_from_lulucf_co2eq_country_sector_6MAy2020 (1).csv
  #            - integrated into complete food system emissions file
  #    File: GHG food system including LULUC (final dataset FAO).xlsx
  # D: Non-food system emissions from Land Use / Land Use Change
  #    Source: FAO
  #            - http://www.fao.org/faostat/en/#data/GL
  #    Version: 03/03/2020 uploaded to JRCBox by ES
  #    File: item_D_Total_LULUCF.csv
  #    Note: this file includes both Food system and non-food system emissions!
  # 
  ##################################################################################################### 
  
  fileAC <- paste0(edgar_folder, "GHG food system including LULUC (final dataset FAO).xlsx")
  acdef <- "# A = EDGAR and LULUC Food system emissions"
  AC <- as.data.table(read.xlsx(fileAC, startRow = 4))
  A <- AC[FOOD_system_stage_detailed != "LULUC"]
  
  fileB <- paste0(edgar_folder, "item_B_nonfoodEDGAR_aggregated.csv")
  bdef <- paste0("# B = EDGAR Non-food system emissions")
  B <- fread(fileB)
  
  C <- AC[FOOD_system_stage_detailed == "LULUC"]
  
  fileCD <- paste0(edgar_folder, "item_D_Total_LULUCF.csv")
  addef <- paste0("# D = Food and non-Food LULUCF emissions")
  CD <- fread(fileCD, header = TRUE)
  CD <- CD[Area != "China"]
  
  
  years <- paste0("Y_", seq(1990, 2015, 1))
  Atot <- A[, .SD, .SDcols = c("Country_code_A3", years)]
  Atot <- A[, lapply(.SD, sum, na.rm=TRUE), .SDcols=years, by=.(Country_code_A3)]
  Atot <- melt.data.table(Atot, id.vars = "Country_code_A3")
  Atot$part <- "A"
  Btot <- B[, lapply(.SD, sum, na.rm=TRUE), .SDcols=years, by=.(Country_code_A3)]
  Btot <- melt.data.table(Btot, id.vars = "Country_code_A3")
  Btot$part <- "B"
  Ctot <- C[, .SD, .SDcols = c("Country_code_A3", years)]
  Ctot <- C[, lapply(.SD, sum, na.rm=TRUE), .SDcols=years, by=.(Country_code_A3)]
  Ctot <- melt.data.table(Ctot, id.vars = "Country_code_A3")
  Ctot$part <- "C"
  
  CDtot <- CD[, .SD, .SDcols = paste0(c("Country_code_A3", seq(1990, 2015, 1)))]
  CDtot <- CDtot[Country_code_A3!=""]
  CDtot <- CDtot[Country_code_A3!="#N/A"]
  setnames(CDtot, as.character(seq(1990, 2015, 1)), years)
  CDtot <- melt.data.table(CDtot, id.vars = "Country_code_A3")
  CDtot$part <- "CD"
  
  
  ## Join all elements to get the total emissions
  abcd <- rbind(Atot, Btot, Ctot, CDtot)
  abcd <- abcd[! is.na(value)]
  
}

calculateShares <- function(curregions){
  
  curregions <- curregions[, shareExcLULUCF := EDGAR_Food/EDGAR_total]
  curregions <- curregions[, shareIncLULUCF := Foodtotal/Total]
  curregions <- curregions[, shareExcFAONFood := Foodtotal/Total_exc_FAO_nFood]
  return(curregions)  
  
}

  

emissions <- openEmissionFiles()

alldata <- dcast.data.table(emissions, Country_code_A3 + variable ~ part, value.var = 'value', fill=0)
alldata <- alldata[, .(country=Country_code_A3, year=as.numeric(gsub("Y_", "", variable)), 
                       EDGAR_Food = A, 
                       EDGAR_nFood = B, 
                       FAO_Food = C, 
                       # Non-food system LULUCF emissions (might be negative !)
                       FAO_nFood= CD - C, 
                       # Total non-LULUCF emissions
                       EDGAR_total = A + B,
                       # Total food system emissions
                       Foodtotal = A + C,
                       FAO_total = CD,
                       # Total emissions except non-food system LULUCF
                       Total_exc_FAO_nFood = A + B + C,
                       # Total emissions 
                       Total = A + B + CD
                       )]
emCols <- setdiff(names(alldata), c("country", "year"))
alldata[year==2015, lapply(.SD, sum), .SDcols=emCols]

devtype <- unique(A[, .(Country_code_A3, dev_country)])
cgroup <- unique(A[, .(Country_code_A3, C_group_IM24_sh)])
global <- melt.data.table(alldata, id.vars = c("country", "year"), variable.name = "type")
devind <- merge(global, unique(A[, .(Country_code_A3, dev_country)]), by.x="country", by.y="Country_code_A3")
cgroups <- merge(global, unique(A[, .(Country_code_A3, C_group_IM24_sh)]), by.x="country", by.y="Country_code_A3")

global <- global[, sum(value, na.rm=TRUE), by=.(year, type)]
global <- dcast.data.table(global, year ~ type, value.var = "V1")

dev <- dcast.data.table(devind[dev_country=="D", sum(value, na.rm=TRUE), by=.(year, type)], year ~ type, value.var = "V1")
ind <- dcast.data.table(devind[dev_country=="I", sum(value, na.rm=TRUE), by=.(year, type)], year ~ type, value.var = "V1")

alldata <- calculateShares(alldata)
global <- calculateShares(global)
dev <- calculateShares(dev)
ind <- calculateShares(ind)


save(emissions, global, dev, ind, oth, devtype, file=paste0(edgar_folder, "/emissions_", format(Sys.time(), "%Y%m%d")))


fx <- createWorkbook()
addWorksheet(fx, sheetName = "shareExcLULUCF")
writeData(fx, sheet = "shareExcLULUCF", foodshareA)
addWorksheet(fx, sheetName = "shareIncLULUCF")
writeData(fx, sheet = "shareIncLULUCF", foodshareB)
addWorksheet(fx, sheetName = "shareExcFAO_nFood")
writeData(fx, sheet = "shareExcFAO_nFood", foodshareD)
addWorksheet(fx, sheet = "globaldata")
writeData(fx, sheet = "globaldata", global)
addWorksheet(fx, sheet = "developing")
writeData(fx, sheet = "developing", dev)
addWorksheet(fx, sheet = "industrialized")
writeData(fx, sheet = "industrialized", ind)

resfile <- paste0(edgar_folder, "foodsystemshares", format(Sys.time(), "%Y%m%d"), ".xlsx")
saveWorkbook(fx, file=resfile, overwrite = TRUE)

