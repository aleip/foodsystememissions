require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "edgar")
edgar_folder <- paste0(google, "/projects/edgar/data/202011/")
fao_folder <- paste0(google, "/projects/faostat_landuse/")

source("../capriextract/f_tools.r")
source("../foodsystememissions/gwps.r")

addandwrite <- function(fx, shName, data){
  addWorksheet(fx, sheetName = shName)
  writeData(fx, sheet = shName, data)
}
xround <- function(x){
  xround <- round(x * 10^(-round(log10(x))), 1) * 10^(round(log10(x)))
  return(xround)
}


openEmissionFiles <- function(gwp='ar6'){
  
  ##################################################################################################### 
  # Food system elements
  # 
  # A: Food system emissions from all sectors except Land Use/Land Use Cange/Forestry. 
  #    Source: EDGAR-FOOD
  #    Version: 12/11/2020 (sent by DG, extracted by MC)
  #    File: EDGAR-FOOD_202011.xlsx
  # B: Non-food system emissions from all sectors except Land Use/Land Use Cange/Forestry.
  #    Source: EDGAR-FOOD
  #    Version: 12/11/2020 (sent by DG, extracted by MC)
  #    File: EDGAR-FOOD_202011.xlsx
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
  years <- paste0("Y_", seq(1990, 2015, 1))
  
  fileAB <- paste0(edgar_folder, "EDGAR-FOOD_202011.xlsx")
  A <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_GHG_food_EMI_AR5", startRow = 2, na.strings = "NULL"))
  AB <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_TOT_EMI_AR5", startRow = 4, na.strings = "NULL"))
  
  # Extract tables
  countrytable <- unique(A[, .(Country_code_A3, Name, C_group_IM24_sh, dev_country)])
  categorytable <- unique(A[, .(EDGAR_SECTOR, IPCC_for_std_report_detailed, IPCC_for_std_report_detailed_desc)])
  
  # Calculate non-food system emissions
  A[, part := "EDGAR_FOOD"]
  AB[, part := "EDGAR"]
  
  # Need to subtract 'food' from 'total' EDGAR values for each IPCC category, gas, country.
  tmp1 <- rbind(A, AB)
  tmp1 <- tmp1[, lapply(.SD, sum, na.rm=TRUE), 
                 by=.(Country_code_A3, Substance, IPCC_for_std_report_detailed, part), 
                 .SDcols=years]
  tmp1 <- melt.data.table(tmp1, measure.vars = years)
  tmp1 <- dcast.data.table(tmp1,Country_code_A3 + Substance + IPCC_for_std_report_detailed + variable ~ part, value.var = "value", fill = 0)
  tmp1[, EDGAR_nFOOD := EDGAR - EDGAR_FOOD]

  # Keep non-Food emissions 
  nfood <- dcast.data.table(tmp1[, .(Country_code_A3, Substance, IPCC_for_std_report_detailed, variable, EDGAR_nFOOD)],
                            Country_code_A3 + Substance + IPCC_for_std_report_detailed ~ variable, 
                            value.var = "EDGAR_nFOOD")
  nfood[, part := "EDGAR_nFOOD"] 
  
  # Recombine
  edgar <- rbind(A, nfood, fill=TRUE)
  edgar <- edgar[, -c("Name", "C_group_IM24_sh", "dev_country", "EDGAR_SECTOR", "IPCC_for_std_report_detailed_desc"), with=FALSE]
  
  
  fileC <- paste0(edgar_folder, "FAO_food.xlsx")
  C <- as.data.table(read.xlsx(fileC, startRow = 5, na.strings = "NULL"))
  C <- C[! (Name=="China" & IPCC_for_std_report_detailed=="5")]
  C <- C[, -c("Name", "C_group_IM24_sh", "dev_country", "EDGAR_SECTOR", "IPCC_for_std_report_detailed_desc"), with=FALSE]
  C[, part := "FAO_FOOD"]
  
  fileCD <- paste0(edgar_folder, "item_D_Total_LULUCF.csv")
  addef <- paste0("# D = Food and non-Food LULUCF emissions")
  CD <- fread(fileCD, header = TRUE)
  CD <- CD[Area != "China"]
  CD <- CD[Area == "Pitcairn Islands", Country_code_A3 := "PCN"]
  CD <- CD[Area == "Palestine", Country_code_A3 := "PSE"]
  CD <- CD[, part:="FAO_total"] 
  setnames(CD, as.character(seq(1990, 2015, 1)), years)
  
  # Calculate FAO-nonfood
  Ctotal <- melt.data.table(C, measure.vars = years)
  Ctotal <- Ctotal[, sum(value, na.rm = TRUE), by = .(Country_code_A3, part, variable)]
  CDtotal <- melt.data.table(CD, measure.vars = years)
  CDtotal <- CDtotal[, sum(value, na.rm = TRUE), by = .(Country_code_A3, part, variable)]
  faoall <- dcast.data.table(rbind(Ctotal, CDtotal), Country_code_A3 + variable ~ part, value.var = 'V1', fill = 0)
  faoall <- faoall[! Country_code_A3==""]
  faoall[, FAO_nFOOD := FAO_total - FAO_FOOD]
  
  # Keep non-Food emissions 
  faonfood <- dcast.data.table(faoall[, .(Country_code_A3, variable, FAO_nFOOD)],
                            Country_code_A3 ~ variable, 
                            value.var = "FAO_nFOOD")
  faonfood[, IPCC_for_std_report_detailed := "5"]
  faonfood[, Substance := "GWP_100_CO2"]
  faonfood[, part := "FAO_nFOOD"] 
  
  # Recombine
  fao <- rbind(C, faonfood, fill=TRUE)
  
  # Melt all parts' years and 
  edgarm <- melt.data.table(edgar, measure.vars = years)
  faom <- melt.data.table(fao, measure.vars = years)
  
  
  # bind all emission elements
  edgarfood <- rbind(edgarm, faom)
  edgarfood <- edgarfood[, variable := as.numeric(gsub("Y_", "", variable))]
  edgarfood <- edgarfood[!is.na(value)]
  edgarfood <- edgarfood[value != 0]
  setnames(edgarfood, 
           c("FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment", "IPCC_for_std_report_detailed"),
           c("stage", "stagedet", "compartment", "ipcc"))
 
  ar5 <- getgwp(ar = 5)
  ar6 <- getgwp(ar = 6)
  gwps <- merge(ar5, ar6, by="gases") 
   
  ## Convert between AR5 and AR6 Global Warming potentials
  edgarfood[, `:=` (ar5=1, ar6=1)]
  edgarfood[Substance == "GWP_100_N2O", `:=` (ar5 = gwps[gases == "N2O"]$ar5, ar6 = gwps[gases == "N2O"]$ar6)]
  edgarfood[Substance == "GWP_100_CH4", `:=` (ar5 = gwps[gases == "CH4fos"]$ar5, ar6 = gwps[gases == "CH4fos"]$ar6)]
  # Agriculure and LULUCF emit biogenic methane
  # Waste except burning
  edgarfood[Substance == "GWP_100_CH4" 
            & grepl("^[456]", ipcc), `:=` (ar5 = gwps[gases == "CH4bio"]$ar5, ar6 = gwps[gases == "CH4bio"]$ar6)]
  # 
  edgarfood[Substance == "GWP_100_CH4" 
            & grepl("^6C", ipcc), `:=` (ar5 = gwps[gases == "CH4fos"]$ar5, ar6 = gwps[gases == "CH4fos"]$ar6)]
  edgarfood[, ar6 := value / ar5 * ar6]
  edgarfood[, ar5 := value]
  edgarfood <- edgarfood[, -"value", with=FALSE]
   
  ## ADD TOTAL EDGAR V.4 EMISSIONS ###
  # edgarv5 <- AB
  # setnames(edgarv5, 
  #          c("IPCC_for_std_report_detailed", "FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment"),
  #          c("ipcc", "stage", "stagedet", "compartment"))
  # edgar5 <- melt.data.table(edgarv5, id.vars = c("Country_code_A3", "Substance", "ipcc", "stage", "compartment"), 
  #                           measure.vars = years, na.rm = TRUE)
  # edgar5 <- edgar5[, .(Country_code_A3, year=variable, sector=substr(ipcc, 1, 1), 
  #                      stage, compartment, gas=gsub("GWP_100_", "", Substance), value=as.numeric(value))]
  # edgar5 <- edgar5[!is.na(value)]
  # edgar5[, year := as.numeric(gsub("Y_", "", year))]
  # edgar5 <- edgar5[, sum(value, na.rm=TRUE), by=.(Country_code_A3, year, sector, stage, compartment, gas)]
  # 
  # edgar5tot <- edgar5[, sum(V1, na.rm=TRUE), by=year]
  # edgar5gassec <- edgar5[, sum(V1, na.rm=TRUE), by=.(year, sector, gas)]
  # edgar5gassec[gas %in% c("HFC-125", "HFC-134a", "HFC-143a", "HFC-32", "SF6"), gas := "F-gases"]
  # edgar5gassec <- edgar5gassec[, sum(V1), by=.(year, sector, gas)]
  # edgar5gassec <- dcast.data.table(edgar5gassec, year + sector ~ gas, value.var = "V1", fill = 0)
  # edgar5gassectot <- edgar5gassec[, lapply(.SD, sum, na.rm=TRUE), .SDcols = c("CH4", "CO2", "F-gases", "N2O"), by=year]
  # edgar5gassec <- rbind(edgar5gassec, edgar5gassectot[, sector:="total"])
  # edgar5gassec <- edgar5gassec[, GHG := CH4 + CO2 + N2O + `F-gases`]
  # setkey(edgar5gassec, year, sector)
  # write.xlsx(edgar5gassec, file=paste0(edgar_folder, "edgar.v5_total_by_sector_gas.xlsx"))
  
  fname <- paste0(edgar_folder, "edgar_food_", format(Sys.time(), "%Y%m%d"), ".rdata")
  save(edgarfood, countrytable, categorytable, gwps, file=fname)
  return(fname) 
  
}

calculateGlobalShares <- function(dt = edgarfood, gwp){
  
  curdt <- copy(dt)
  setnames(curdt, gwp, "value")
  
  calculateShares <- function(curregions){
    
    curregions <- curregions[, shareExcLULUCF := EDGAR_FOOD/EDGAR_total]
    curregions <- curregions[, shareIncLULUCF := Foodtotal/Total]
    curregions <- curregions[, shareExcFAONFood := Foodtotal/Total_exc_FAO_nFood]
    
    
    return(curregions)  
    
  }
  
  foodsystemelements <- c("EDGAR_FOOD", "EDGAR_nFOOD", "FAO_FOOD", "FAO_nFOOD")
  calctot <- function(dt = edgarfood){
    dtcalctot <- copy(dt)
    totemissions <- dtcalctot[, sum(value, na.rm=TRUE), by=.(Country_code_A3, variable, part)]
    totemissions <- dcast.data.table(totemissions[Country_code_A3 != ""], Country_code_A3 + variable ~ part, value.var = 'V1', fill=0)
    totemissions[, sum(.SD, na.rm = TRUE), by=.(variable), .SDcols=foodsystemelements]
    return(totemissions)  
  }
  totemissions <- calctot(edgarfood)
  
  alldata <- totemissions[, .(
    country=Country_code_A3, year=variable, 
    EDGAR_FOOD, 
    EDGAR_nFOOD, 
    FAO_FOOD, 
    # Non-food system LULUCF emissions (might be negative !)
    FAO_nFOOD, 
    # Total non-LULUCF emissions
    EDGAR_total = EDGAR_FOOD + EDGAR_nFOOD,
    FAO_total = FAO_FOOD + FAO_nFOOD)]
  alldata[, `:=` (  
    # Total food system emissions
    Foodtotal = EDGAR_FOOD + FAO_FOOD,
    nFoodtotal = EDGAR_nFOOD + FAO_nFOOD,
    # Total emissions except non-food system LULUCF
    Total_exc_FAO_nFood = EDGAR_FOOD + EDGAR_nFOOD + FAO_FOOD,
    # Total emissions 
    Total = EDGAR_total + FAO_total
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
  
  foodshareA <- dcast.data.table(alldata[, .(country, year, shareExcLULUCF)], country ~ year, value.var = "shareExcLULUCF")
  foodshareB <- dcast.data.table(alldata[, .(country, year, shareIncLULUCF)], country ~ year, value.var = "shareIncLULUCF")
  foodshareD <- dcast.data.table(alldata[, .(country, year, shareExcFAONFood)], country ~ year, value.var = "shareExcFAONFood")
  
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
  
  fn <- paste0(edgar_folder, "EDGAR-FOOD_by_globalshares", "_", format(Sys.time(), "%Y%m%d"), ".rdata")
  flx <- gsub(".rdata", ".xlsx", fn)
  saveWorkbook(wb = fx, file=flx, overwrite = TRUE)
  save(alldata, totemissions, global, dev, ind, foodsystemelements, file=fn)
  
  return(fn)
}

calculateStages <- function(curdt=edgarfood, grouping='stagedet', gwp='ar6'){
 
  
  curstages <- copy(curdt)
  setnames(curstages, gwp, "value")
  
  curstages <- curstages[value != 0]
  devtable <- unique(countrytable[, .(Country_code_A3, dev_country, C_group_IM24_sh)])
  global <- merge(curstages, devtable, by="Country_code_A3")
  setnames(global, grouping, "grouping")
  global <- global[part=="EDGAR_nFOOD", grouping := "EDGAR_nFOOD"]
  global <- global[part=="FAO_nFOOD", grouping := "FAO_nFOOD"]
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
    stagesd <- stagesd[, .(region, year, LULUC, Production, Transport, Processing, Packaging, 
                           Retail, Consumption, EoL=End_of_Life, EDGAR_nFOOD, FAO_nFOOD)]
    
  } else if (grouping == "compartment"){
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
  resfile <- paste0(edgar_folder, "EDGAR-FOOD_by_", groupname, "_", format(Sys.time(), "%Y%m%d"), ".rdata")
  saveWorkbook(fx, file=gsub("rdata", "xlsx", resfile), overwrite = TRUE)
  save(list = c(paste0("emBy", groupname), paste0("sharBy", groupname)), dev, ind, file=resfile)
  
  return(resfile)
  
}

Global_byGasPart <- function(curdt = edgarfood, gwp=ar6){
  
  edt <- copy(curdt)
  setnames(edt, gwp, "value")
  
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
  
  # Calculate totals
  em3sectort <- dcast.data.table(em3sector, year+sec+sector+gas ~ part, value.var = "value", fill = 0)
  em3sectort[, `:=` (EDGAR_total = EDGAR_FOOD + EDGAR_nFOOD,
                     FAO_total = FAO_FOOD + FAO_nFOOD,
                     FOOD_total = EDGAR_FOOD + FAO_FOOD,
                     total = EDGAR_FOOD + EDGAR_nFOOD + FAO_FOOD + FAO_nFOOD)]
  em3sectort[sec==7, sector:= '7 Indirect']
  
  em3sectort[, FOOD_bysec := sum(FOOD_total), by=.(year, sec)]
  em3sectort[, ALL_total := sum(total), by=.(year)]
  em3sectort[, ALLFOOD_total := sum(FOOD_total), by=.(year)]
  
  fn <- paste0(edgar_folder, "EDGAR-FOOD_by_Gas_", format(Sys.time(), "%Y%m%d"), ".rdata")
  write.xlsx(em3sectort, file=gsub("rdata", "xlsx", fn))
  save(em3sectort, file=fn)
  return(fn)
  
}


IPCC_table <- function(curdt = edgarfood, gwp='ar6'){
  
  load(Global_byGasPart(curdt, gwp))
   
  foodemissions <- em3sectort[, .(year, sector, gas, FOOD_total, total)]
  setnames(foodemissions, "FOOD_total", "FOOD")
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


recalcemissions <- TRUE
doglobshares <- TRUE
docalcstages <- TRUE
docalcsector <- TRUE
writeIPCCtable <- TRUE

if(recalcemissions){
  load(openEmissionFiles())
  write(paste0("edgar_food_", today(), ".rdata"), paste0(edgar_folder, "/edgar_food_last.txt"))
}else{
  toload <- paste0(edgar_folder, scan(paste0(edgar_folder, "/edgar_food_last.txt"), what = "character"))
  load(toload)
}

gwp <- "ar6"
if(doglobshares) {load(calculateGlobalShares(edgarfood, gwp))}
if(docalcstages) {load(calculateStages(edgarfood, "stagedet", gwp))}
if(docalcsector) {load(calculateStages(edgarfood, "compartment", gwp))}
if(writeIPCCtable) {load(IPCC_table(edgarfood, gwp))}
