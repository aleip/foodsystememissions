require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "edgar")
edgar_folder <- paste0(google, "/projects/edgar/data/202005/")
fao_folder <- paste0(google, "/projects/faostat_landuse/")

addandwrite <- function(fx, shName, data){
  addWorksheet(fx, sheetName = shName)
  writeData(fx, sheet = shName, data)
}
xround <- function(x){
  xround <- round(x * 10^(-round(log10(x))), 1) * 10^(round(log10(x)))
  return(xround)
}


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
  AC <- AC[FOOD_system_stage_detailed != "LULUC", part := "EDGAR_FOOD"]
  AC <- AC[FOOD_system_stage_detailed == "LULUC", part := "FAO_FOOD"]
 
  AC <- AC[! (Name=="China" & IPCC_for_std_report_detailed=="5")]
   
  fileB <- paste0(edgar_folder, "item_B_nonfoodEDGAR_aggregated.csv")
  bdef <- paste0("# B = EDGAR Non-food system emissions")
  B <- fread(fileB)
  B <- B[, part:="EDGAR_nFOOD"]
  
  fileCD <- paste0(edgar_folder, "item_D_Total_LULUCF.csv")
  addef <- paste0("# D = Food and non-Food LULUCF emissions")
  CD <- fread(fileCD, header = TRUE)
  CD <- CD[Area != "China"]
  CD <- CD[Area == "Pitcairn Islands", Country_code_A3 := "PCN"]
  CD <- CD[Area == "Palestine", Country_code_A3 := "PSE"]
  CD <- CD[, part:="FAO_total"] 
  years <- paste0("Y_", seq(1990, 2015, 1))
  setnames(CD, as.character(seq(1990, 2015, 1)), years)
  
  # Extract tables
  countrytable <- unique(AC[, .(Country_code_A3, Name, C_group_IM24_sh, dev_country)])
  categorytable <- unique(AC[, .(EDGAR_SECTOR, IPCC_for_std_report_detailed, IPCC_for_std_report_detailed_desc)])
  AC <- AC[, .SD, .SDcols = setdiff(names(AC), c("IPCC_for_std_report_detailed_desc",
                                                 "Name", "C_group_IM24_sh", "dev_country"))]
  
  # Melt all parts' years and 
  ACm <- melt.data.table(AC, measure.vars = years)
  Bm <- melt.data.table(B, measure.vars = years)
  CDm <- melt.data.table(CD, measure.vars = years)
  CDm <- CDm[, .(Country_code_A3, part, variable, value)]
  CDm <- CDm[Country_code_A3 != ""]
  CDm[, IPCC_for_std_report_detailed := "5"]
  CDm[, EDGAR_SECTOR := "FOLU"]# Attention here 'C' still included, needs to be subtracted later
  
  Bm[, IPCC_for_std_report_detailed := "Total"]
  Bm[, EDGAR_SECTOR := "Total"]
  BCDm <- rbind(Bm, CDm)
  
  
  # Fill missing information
  BCDm <- BCDm[, `:=` (Substance = "CO2",
                       FOOD_system_stage = "",
                       FOOD_system_stage_detailed = "",
                       FOOD_system_compartment = "")] 
  
  # bind all emission elements
  edgarfood <- rbind(ACm, BCDm)
  edgarfood <- edgarfood[, variable := as.numeric(gsub("Y_", "", variable))]
  edgarfood <- edgarfood[!is.na(value)]
  setnames(edgarfood, 
           c("FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment", "IPCC_for_std_report_detailed"),
           c("stage", "stagedet", "compartment", "ipcc"))
  
  totemissions <- edgarfood[, sum(value, na.rm=TRUE), by=.(Country_code_A3, variable, part)]
  totemissions <- dcast.data.table(totemissions[Country_code_A3 != ""], Country_code_A3 + variable ~ part, value.var = 'V1', fill=0)
  
  
  ## ADD TOTAL EDGAR V.5 EMISSIONS ###
  edgarv5_file <- paste0(edgar_folder, "data_from_V5.0_EM_GHG_AR5_by_gas_CO2noBIO.xlsx")
  edgarv5 <- as.data.table(read.xlsx(edgarv5_file, startRow = 1))
  setnames(edgarv5, 
           c("IPCC_for_std_report_detailed", "FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment"),
           c("ipcc", "stage", "stagedet", "compartment"))
  edgar5 <- melt.data.table(edgarv5, id.vars = c("Country_code_A3", "Substance", "ipcc", "stage", "compartment"), 
                            measure.vars = years, na.rm = TRUE)
  edgar5 <- edgar5[, .(Country_code_A3, year=variable, sector=substr(ipcc, 1, 1), 
                       stage, compartment, gas=gsub("GWP_100_", "", Substance), value=as.numeric(value))]
  edgar5 <- edgar5[!is.na(value)]
  edgar5[, year := as.numeric(gsub("Y_", "", year))]
  edgar5 <- edgar5[, sum(value, na.rm=TRUE), by=.(Country_code_A3, year, sector, stage, compartment, gas)]
  
  B <- melt.data.table(B, id.vars = "Country_code_A3", measure.vars = years)
  B[, year := as.numeric(gsub("Y_", "", variable))]
  
  edgar5tot <- edgar5[, sum(V1, na.rm=TRUE), by=year]
  edgar5gassec <- edgar5[, sum(V1, na.rm=TRUE), by=.(year, sector, gas)]
  edgar5gassec <- dcast.data.table(edgar5gassec, year + sector ~ gas, value.var = "V1", fill = 0)
  edgar5gassectot <- edgar5gassec[, lapply(.SD, sum, na.rm=TRUE), .SDcols = c("CH4", "CO2", "F-gases", "N2O"), by=year]
  edgar5gassec <- rbind(edgar5gassec, edgar5gassectot[, sector:="total"])
  edgar5gassec <- edgar5gassec[, GHG := CH4 + CO2 + N2O + `F-gases`]
  setkey(edgar5gassec, year, sector)
  write.xlsx(edgar5gassec, file=paste0(edgar_folder, "edgar.v5_total_by_sector_gas.xlsx"))
  
  fname <- paste0(edgar_folder, "edgar_food_", format(Sys.time(), "%Y%m%d"), ".rdata")
  save(edgarfood, totemissions, countrytable, categorytable, edgar5gassec, file=fname)
  return(fname) 
  
}

calculateGlobalShares <- function(totemissions){
  calculateShares <- function(curregions){
    
    curregions <- curregions[, shareExcLULUCF := EDGAR_FOOD/EDGAR_total]
    curregions <- curregions[, shareIncLULUCF := Foodtotal/Total]
    curregions <- curregions[, shareExcFAONFood := Foodtotal/Total_exc_FAO_nFood]
    
    
    return(curregions)  
    
  }
  
  
  alldata <- totemissions[, .(country=Country_code_A3, year=variable, 
                              EDGAR_FOOD, 
                              EDGAR_nFOOD, 
                              FAO_FOOD, 
                              # Non-food system LULUCF emissions (might be negative !)
                              FAO_nFOOD= FAO_total - FAO_FOOD, 
                              # Total non-LULUCF emissions
                              EDGAR_total = EDGAR_FOOD + EDGAR_nFOOD,
                              # Total food system emissions
                              Foodtotal = EDGAR_FOOD + FAO_FOOD,
                              FAO_total,
                              # Total emissions except non-food system LULUCF
                              Total_exc_FAO_nFood = EDGAR_FOOD + EDGAR_nFOOD + FAO_FOOD,
                              # Total emissions 
                              Total = EDGAR_FOOD + EDGAR_nFOOD + FAO_total
  )]
  emCols <- setdiff(names(alldata), c("country", "year"))
  alldata[year==2015, lapply(.SD, sum), .SDcols=emCols]
  
  devtype <- unique(countrytable[, .(Country_code_A3, dev_country)])
  cgroup <- unique(countrytable[, .(Country_code_A3, C_group_IM24_sh)])
  stages <- unique(edgarfood$FOOD_system_stage)
  stagesdet <- unique(edgarfood$FOOD_system_stage_detailed)
  
  
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
  save(alldata, global, dev, ind, file=paste0(edgar_folder, "/globalshares_", format(Sys.time(), "%Y%m%d"), ".rdata"))
  
  return(alldata)
}

calculateStages <- function(curstages=emissions, grouping='stagedet'){
  
  edgarfood <- edgarfood[value != 0]
  devtable <- unique(countrytable[, .(Country_code_A3, dev_country, C_group_IM24_sh)])
  global <- merge(edgarfood, devtable, by="Country_code_A3")
  setnames(global, grouping, "grouping")
  global <- global[part=="EDGAR_nFOOD", grouping := "EDGAR_nFOOD"]
  global <- global[part=="FAO_total", grouping := "FAO_total"]
  setnames(global, c("variable", "dev_country", "C_group_IM24_sh"), c("year", "region", "cgroup"))
  
  globals <- global[, sum(value, na.rm=TRUE), by=.(year, part, region, grouping)]
  globalr <- global[, sum(value, na.rm=TRUE), by=.(year, part, cgroup, grouping)]
  setnames(globalr, "cgroup", "region")
  globalt <- globals[, sum(V1, na.rm=TRUE), by=.(year, part, grouping)]
  globalt$region <- "Global"
  
  stages <- rbind(globals, rbind(globalr, globalt))
  setnames(stages, "V1", "emissions")
  
  if(grouping == "compartment"){
    stages <- stages[grouping == "Landbased", grouping := paste0("Landbased", substr(part, 1, 3))]
  }
  
  stagesd <- dcast.data.table(stages, year + region ~ grouping, value.var = "emissions", fill=0)
  
  if(grouping == "stagedet"){
    stagesd <- stagesd[, FAO_nFOOD := FAO_total - LULUC]
    
    stagesd <- stagesd[, .(region, year, LULUC, Production, Transport, Processing, Packaging, 
                           Retail, Consumption, EoL=`End of Life`, EDGAR_nFOOD, FAO_nFOOD)]
    
  } else if (grouping == "compartment"){
    stagesd <- stagesd[, FAO_nFOOD := FAO_total - LandbasedFAO]
    stagesd <- stagesd[, .(region, year, LandbasedEDG, LandbasedFAO, Energy, Industry, Waste,
                           EDGAR_nFOOD, FAO_nFOOD)]
  }
  stagenames <- setdiff(names(stagesd), c("year", "region"))
  stagesfood <- setdiff(stagenames, c("EDGAR_nFOOD", "FAO_nFOOD"))
  stagesexcl <- setdiff(stagenames, c("LULUC", "Landbased_FAO", "FAO_nFOOD"))
  emByStage <- stagesd[, TOT_incl := sum(.SD), by=1:nrow(stagesd), .SDcols=stagenames]
  emByStage <- stagesd[, TOT_excl := sum(.SD), by=1:nrow(stagesd), .SDcols=stagesexcl]
  emByStage <- stagesd[, TOT_FOOD := sum(.SD), by=1:nrow(stagesd), .SDcols=stagesfood]
  setkey(emByStage, region, year)
  
  sharByStage <- copy(emByStage)
  sharByStage <- sharByStage[, (stagenames) := .SD/TOT_FOOD, .SDcols = stagenames, by=1:nrow(sharByStage)]
  if(grouping == "compartment"){
    sharByStage <- sharByStage[, EnerOverLandEne := (Energy+Industry) / (LandbasedFAO + LandbasedEDG + Energy + Industry)]  
    sharByStage <- sharByStage[, LandOverLandEne := (LandbasedFAO + LandbasedEDG) / (LandbasedFAO + LandbasedEDG + Energy + Industry)]  
  }
  
  groupname <- grouping
  if(grouping == 'stagedet') {groupname <- "Stage"}
  if(grouping == 'compartment') {
    groupname <- "Comp"
  }
 
  fx <- createWorkbook()
  addandwrite(fx, paste0("emBy", groupname, "_global"), emByStage[region=="Global"])
  addandwrite(fx, paste0("emBy", groupname, "_dev"), emByStage[region%in%c("D", "I", "0")])
  addandwrite(fx, paste0("emBy", groupname, "_groups"), emByStage[grepl("^[0-9][0-9]", region)])
  addandwrite(fx, paste0("sharBy", groupname, "_global"), sharByStage[region=="Global"])
  addandwrite(fx, paste0("sharBy", groupname, "_dev"), sharByStage[region%in%c("D", "I", "0")])
  addandwrite(fx, paste0("sharBy", groupname, "_groups"), sharByStage[grepl("^[0-9][0-9]", region)])
  
  assign(paste0("emBy", groupname), emByStage)
  assign(paste0("sharBy", groupname), sharByStage)
  resfile <- paste0(edgar_folder, "EDGAR-FOOD_", groupname, format(Sys.time(), "%Y%m%d"), ".rdata")
  saveWorkbook(fx, file=gsub("rdata", "xlsx", resfile), overwrite = TRUE)
  save(list = c(paste0("emBy", groupname), paste0("sharBy", groupname)), dev, ind, file=resfile)
  
  return(resfile)
  
}

Global_byGasPart <- function(curdt = edgarfood, totedgar = edgar5gassec){
  
    edt <- copy(curdt)
  
  # Get sector names
  ipcccat <- as.data.table(read.xlsx(xlsxFile = paste0(edgar_folder, "../../classifications/IPCC_master_categories.xlsx")))
  ipcccat <- unique(ipcccat[, .(code, IPCC_AR2_description)])
  edt[, sec := substr(ipcc, 1, 1)]
  edt <- merge(edt, ipcccat, by.x="sec", by.y = "code")
  edt[, sector:=IPCC_AR2_description] 
  
  # Assing indirect N2O emissions to main sectors
  edt[ipcc%in%c("7B1", "7C1"), sector:="Energy"]
  edt[ipcc%in%c("7B2", "7C2"), sector:="Industrial Processes"]
  edt[ipcc%in%c("7B1", "7C1"), sec:=1]
  edt[ipcc%in%c("7B2", "7C2"), sec:=2]
  
  edt <- edt[, .(sec, sector, part, Country_code_A3, Substance, year=variable, value)]
  
  # Change names
  em3sector <- edt[, .(year, sec, sector, part, Country_code_A3, gas=gsub("GWP_100_", "", Substance), value)]
  em3sector <- em3sector[, sector := gsub(" \\(please specify\\)", "", sector)]
  em3sector <- em3sector[, sector := gsub(",", "", sector)]
  em3sector[grepl("F", gas), gas:="F-gases"]
  
  # Aggregate by year, sector, substance
  em3sector <- em3sector[, sum(value), by=.(year, sec, sector, part, gas)]
  
  # Convert from kt CO2eq to Mt CO2eq per year
  em3sector <- em3sector[, .(year, sec, sector, part, gas, value=V1/1000)]
  em3sector <- em3sector[, sector := paste0(sec, " ", sector)]
  
  
  # Add total EDGAR emissions
  totedgar <- melt.data.table(totedgar[sector != 'total'], id.vars = c("year", "sector"), variable.name = 'gas', value.name = 'EDGAR_total')
  totedgar <- totedgar[gas != 'GHG']
  totedgar[, EDGAR_total := EDGAR_total/1000]
  setnames(totedgar, "sector", "sec")
  
  # Calculate total food emissions
  #em3total <- em3sector[, sum(value), by=.(year, sec, part, sector)]
  #em3total$gas <- "GHG"
  #setnames(em3total, "V1", "value")
  #edgarfood <- rbind(em3sector, em3total)
  em3sector[, part := gsub("EDGAR_|FAO_", "", part)]
  edgarfood <- dcast.data.table(em3sector, year + sec + sector + gas ~ part, value.var = "value", fill = 0)
  
  alledgar <- merge.data.table(edgarfood, totedgar, by = c("year", "sec", "gas"), all = TRUE)
  alledgar[is.na(alledgar)] <- 0
  alledgar[sec==3, sector:= '3 Solvent and Other Product Use']
  alledgar[sec==6, sector:= '6 Waste']
  alledgar[sec==7, sector:= '7 Indirect']
  alledgar[sec=="total", sector:= 'total']
  alledgar[, total := EDGAR_total + total]
  
  alledgar <- alledgar[total == 0, total := FOOD]
  alledgar <- alledgar[total != 0]
  
  alledgar[, FOOD_total := sum(FOOD), by=.(year, sec)]
  alledgar[, ALL_total := sum(total), by=.(year)]
  alledgar[, ALLFOOD_total := sum(FOOD), by=.(year)]
  alledgar[, nFOOD := total - FOOD]
  
  alledgar <- alledgar[, .(year, sec, gas, sector, FOOD, nFOOD, total, FOOD_total, ALLFOOD_total, ALL_total)]
  
  fn <- paste0(edgar_folder, "allglobal_", format(Sys.time(), "%Y%m%d"), ".rdata")
  write.xlsx(alledgar, file=gsub("rdata", "xlsx", fn))
  save(alledgar, file=fn)
  return(fn)
  
}


IPCC_table <- function(curdt = edgarfood, totedgar = edgar5gassec){
  
  load(Global_byGasPart(curdt, totedgar)) 
   
  foodemissions <- alledgar[, .(year, sector, gas, FOOD, total)]
  foodtot <- foodemissions[, lapply(.SD, sum, na.rm=TRUE), by=.(year, gas), .SDcols=c("FOOD", "total")]
  foodtot$sector <- "Total"
  foodemissions <- rbind(foodemissions, foodtot)
  foodsec <- foodemissions[, lapply(.SD, sum, na.rm=TRUE), by=.(year, sector), .SDcols=c("FOOD", "total")]
  foodsec$gas <- "GHG"
  foodemissions <- rbind(foodemissions, foodsec)
  foodemissions[, share := FOOD/total]
  
  foodshares <- dcast.data.table(foodemissions, year + sector ~ gas, value.var = c("FOOD", "total", "share"), fill = 0)
  # Applying AR5-GWP100: CH4 28, N2O 265
  # GWPs - AR5
  foodemissions[, gwp := ifelse(gas=="CH4", 28, ifelse(gas=="N2O", 265, 1))]
  foodemissions[, (c("FOOD", "total")) := .SD/gwp, .SDcols=c("FOOD", "total")]
  
  
  # Select data for table
  ipcctable <- foodemissions[year %in% c(1990, 2015) & !grepl("^7", sector), .(year, sector, gas, FOOD, share)]
  
  # Round values
  ipcctable[ipcctable == 0] <- NA
  ipcctable <- ipcctable[, FOOD := round(FOOD, 0)]
  ipcctable <- ipcctable[, share := signif(share*100,3)]
  ipcctable <- ipcctable[, share := round(share, 1)]
  
  # Dcast table
  ipcctable <- dcast.data.table(ipcctable, year  + sector ~ gas, value.var = c("FOOD", "share"))
  
  # Sort columns
  ghgs <- c("CO2", "CH4", "N2O", "F-gases", "GHG")
  cols <- c(paste0("FOOD_", ghgs), paste0("share_", ghgs))
 
  ipcctable <- ipcctable[, .SD, .SDcols = c("year", "sector", cols)]
  setnames(ipcctable, paste0("FOOD_", ghgs), ghgs)
  
  fn <- paste0(edgar_folder, "IPCC_table_", format(Sys.time(), "%Y%m%d"), ".rdata")
  save(ipcctable, file=fn)
  
  ipcctable[, (paste0("share_", ghgs)) := lapply(.SD, paste0, "%"), .SDcols = paste0("share_", ghgs)]
  ipcctable[ipcctable == "NA%"] <- " - "
  
  wb <- createWorkbook()
  ws <- addWorksheet(wb, sheetName = "Table 12.7")
  header <- c("Sector", ghgs, ghgs)
  writeData(wb, startRow = 1, t(header), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 2, t(c("", "Emissions (Mt gas yr-1)", rep("", 4), "Share of total sectoial emissions (%)", rep("", 4))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 3, t(c("", "1990", rep("", 4+5))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 4, ipcctable[year==1990, -"year", with=FALSE], sheet = 'Table 12.7', startCol = 1, colNames = FALSE, keepNA = TRUE, na.string = " - ")
  writeData(wb, startRow = 11, t(c("", "2015", rep("", 4+5))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 12, ipcctable[year==2015, -"year", with=FALSE], sheet = 'Table 12.7', startCol = 1, colNames = FALSE, keepNA = TRUE, na.string = " - ")
  saveWorkbook(wb, file = gsub("rdata", "xlsx", fn), overwrite = TRUE)
  return(fn)

}


recalcemissions <- FALSE
doglobshares <- FALSE
docalcstages <- FALSE
docalcsector <- FALSE

if(recalcemissions){
  load(openEmissionFiles())
}else{
  toload <- paste0(edgar_folder, scan(paste0(edgar_folder, "/edgar_food_last.txt"), what = "character"))
  load(toload)
}


if(doglobshares) {shares <- calculateGlobalShares(totemissions)}
if(docalcstages) {load(calculateStages(edgarfood, "stagedet"))}
if(docalcsector) {load(calculateStages(edgarfood, "compartment"))}
