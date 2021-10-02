require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

rm(list=ls())

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
jrcbox <- paste0("C:/Users/", myuser, "/ownCloud/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "edgar")
edgar_folder2020 <- paste0(jrcbox, "EDGAR-FOOD/202011/")
edgar_folder <- paste0(jrcbox, "EDGAR-FOOD/202108/")
fao_folder <- paste0(google, "/projects/faostat_landuse/")
fao_folder <- edgar_folder
ipcc_folder <- paste0(google, "/projects/ipccwg3/xcutting/dms05report_data_energy_emissions/")

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


openEDGAREmission <- function(){
  
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
  
  # C: Total and Food system emissions from Forestry and Land Use and Land Use Change
  #    Source: FAO (2021) (version 25/06/2021)
  #    Emissions from agriculture and forest land. Global, regional and country trends 1990-2019
  #    
  #    see http://www.fao.org/food-agriculture-statistics/data-release/data-release-detail/en/c/1413420/
  #    see also methodological README NOTES http://fenixservices.fao.org/faostat/static/documents/GT/GT_e.pdf
  #                                         http://fenixservices.fao.org/faostat/static/documents/EM/EM_e.pdf
  #         
  #         Agriculture-related land use emissions include emissions from cropland, grassland, net forest conversion, 
  #         and fires from burning of organic soils and humid tropical forests. 
  #       
  #   
  #    Version: Dowloaded 25/08/2021 by ES
  #     - files FAOSTAT_FOOD_LULUCF_detailed_forAdrian_202108.csv
  #             FAOSTAT_total_LULUCF_detailed_forAdrian_202108.csv
  # 
  ##################################################################################################### 
  years <- paste0("Y_", seq(1990, 2018, 1))
  
  version <- "202108"
  if(version=="202011"){
    fileAB <- paste0(edgar_folder, "EDGAR-FOOD_202011.xlsx")
    A <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_GHG_food_EMI_AR5", startRow = 2, na.strings = "NULL"))
    AB <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_TOT_EMI_AR5", startRow = 4, na.strings = "NULL"))
  }
  if(version=="202108"){
    fileAB <- paste0(edgar_folder, "IPCC_input_for_adrian_06082021.xlsx")
    A <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_GHG_food_EMI", startRow = 2, na.strings = "NULL"))
    nfood <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_DIFF_EMI", startRow = 4, na.strings = "NULL"))
    #totcounsector <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_GHG_food_by_country"))
    #totsubssector <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_GHG_food_by_subst"))
    
  }
  
  
  
  # Extract tables
  countrytable <- unique(A[, .(Country_code_A3, Name, C_group_IM24_sh, dev_country)])
  categorytable <- unique(A[, .(EDGAR_SECTOR, IPCC_for_std_report_detailed, IPCC_for_std_report_detailed_desc)])
  
  # Calculate non-food system emissions
  A[, part := "EDGAR_FOOD"]
  nfood[, part := "EDGAR_nFOOD"]
  nfood[, FOOD_system_stage := NA]
  nfood[, FOOD_system_stage_detailed := NA]
  #totcounsector[, part := "EDGAR"]
  
  #tota <- AB[, sum(Y_2018, na.rm=TRUE), by = .(Country_code_A3, Substance)]
  #totb <- totcounsector[, sum(as.numeric(Y_2018), na.rm=TRUE), by = .(Country_code_A3, Substance)]
  #tota <- dcast.data.table(tota, Country_code_A3~Substance, value.var = 'V1')
  #totb <- dcast.data.table(totb, Country_code_A3~Substance, value.var = 'V1')
  
  # Need to subtract 'food' from 'total' EDGAR values for each IPCC category, gas, country.
  #tmp1 <- rbind(A, AB)
  #tmp1 <- tmp1[, lapply(.SD, sum, na.rm=TRUE), 
  #             by=.(Country_code_A3, Substance, IPCC_for_std_report_detailed, part), 
  #             .SDcols=years]
  #tmp1 <- melt.data.table(tmp1, measure.vars = years)
  #tmp1 <- dcast.data.table(tmp1,Country_code_A3 + Substance + IPCC_for_std_report_detailed + variable ~ part, value.var = "value", fill = 0)
  #tmp1[, EDGAR_nFOOD := EDGAR - EDGAR_FOOD]
  
  # Keep non-Food emissions 
  #nfood <- dcast.data.table(tmp1[, .(Country_code_A3, Substance, IPCC_for_std_report_detailed, variable, EDGAR_nFOOD)],
  #                         Country_code_A3 + Substance + IPCC_for_std_report_detailed ~ variable, 
  #                          value.var = "EDGAR_nFOOD")
  #nfood[, part := "EDGAR_nFOOD"] 
  
  # Recombine
  edgar <- rbind(A, nfood, fill=TRUE)
  edgar <- edgar[, -c("Name", "C_group_IM24_sh", "dev_country", "EDGAR_SECTOR", "IPCC_for_std_report_detailed_desc"), with=FALSE]
  edgar <- melt.data.table(edgar, id.vars = c("Country_code_A3", "Substance", "IPCC_for_std_report_detailed", 
                                              "FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment", "part"), variable.name = "Year")
  edgar[, Year := as.numeric(gsub("Y_", "", Year))]
  
  edgar <- edgar[, .(value=sum(value)), by=.(Country_code_A3, Substance, IPCC_for_std_report_detailed, 
                                            FOOD_system_stage, FOOD_system_stage_detailed, FOOD_system_compartment, part, Year)]
  save(edgar, file=paste0(edgar_folder, "EDGAR_", format(Sys.time(), "%Y%m%d"), ".rdata"))
  save(countrytable, categorytable, file=paste0(edgar_folder, "tables_", format(Sys.time(), "%Y%m%d"), ".rdata"))
  
  return(edgar)
  
}

openFAOemissions <- function(){
  
  #faototal <- fread(paste0(edgar_folder, "input_fao/FAOSTAT_total_LULUCF_detailed_forAdrian_202108.csv"))
  #faofood <- fread(paste0(edgar_folder, "input_fao/FAOSTAT_FOOD_LULUCF_detailed_forAdrian_202108.csv"))
  
  faofires <- fread(paste0(edgar_folder, "input_fao_efisio/fires_FAOSTAT_data_202108.csv"))
  #Fires in other forests not food system related
  faofires_nonfood <- faofires[Item=="Other forest"]
  faofires <- faofires[Item!="Other forest"]
  #Fires on organic soils assumed to be food related only for South East Asian Countries
  sea <- fread(paste0(edgar_folder, "input_fao_efisio/countries_south_east_asia.csv"))$ISO3
  faofires_organic_nonfood <- faofires[Item=="Fires in organic soils" & ! `Area Code (ISO3)` %in% sea]  
  faofires <- faofires[Item!="Fires in organic soils" | `Area Code (ISO3)` %in% sea]
  
  faonet_forest <- fread(paste0(edgar_folder, "input_fao_efisio/net_forest_FAOSTAT_202108.csv"))
  faoorganic_soils <- fread(paste0(edgar_folder, "input_fao_efisio/organic_soils_FAOSTAT_202108.csv"))
  
  faofood <- Reduce(rbind, list(faofires, faonet_forest, faoorganic_soils))
  
  
  faototal <- fread(paste0(edgar_folder, "input_fao_efisio/total_LULUCF_FAOSTAT_20210824_bygas_byprocess.csv"))
  
  faofood$part <- "FAO_FOOD"
  faototal$part <- "FAO_total"
  
  faoall <- rbind(faofood, faototal)
  
  faoall <- faoall[grepl("Net", Element), Element := "CO2"]
  faoall[, Element := gsub("Emissions \\(", "", gsub("\\)", "", Element))]
  faoall[, Domain := gsub("Emissions ", "", Domain)]
  
  # Adapt to EDGAR terminology
  # All units in kilotonnes
  faoall <- faoall[, .(Country_code_A3=`Area Code (ISO3)`, 
                       Substance=Element, 
                       IPCC_for_std_report_detailed = paste0("5.", `Domain Code`, "_", Domain, "_", Item),
                       FOOD_system_stage = "Production", 
                       FOOD_system_stage_detailed = "LULUCF", 
                       FOOD_system_compartment = "Landbased",
                       Year, part, Value
  )]
  faoall <- faoall[! is.na(Value)]
  
  # Calculate FAO_nFOOD
  faoall <- dcast.data.table(faoall, Country_code_A3 + Substance + IPCC_for_std_report_detailed + FOOD_system_stage + FOOD_system_stage_detailed + FOOD_system_compartment + Year ~ part, 
                             value.var = "Value", fill = 0) 
  faoall[, FAO_nFOOD := FAO_total-FAO_FOOD]
  
  # Melt back for combining
  faoall <- melt.data.table(faoall, measure.vars = c("FAO_FOOD", "FAO_total", "FAO_nFOOD"), variable.name = "part", value.name = "Value")
  faoall[part=="FAO_nFOOD", FOOD_system_stage := ""]
  faoall[part=="FAO_nFOOD", FOOD_system_stage_detailed := ""]
  
  # Clean countries
  faocountries <- unique(faofood[, .(Country_code_A3=`Area Code (ISO3)`, Area)])
  load(paste0(edgar_folder, "EDGAR_", format(Sys.time(), "%Y%m%d"), ".rdata"))
  allcountries <- merge(countrytable, faocountries, by="Country_code_A3", all = TRUE)
  write.xlsx(allcountries, file=paste0(edgar_folder, "countrymerge.xlsx"))
  
  # Country groups in FAO to eliminate
  #Country_code_A3	Name	Area
  #15		Belgium-Luxembourg
  #164		Pacific Islands Trust Territory
  #206		Sudan (former)
  #228		USSR
  #248		Yugoslav SFR
  #41		China, mainland
  #51		Czechoslovakia
  #62		Ethiopia PDR
  #SRB		Serbia ===> ignore as there is also Serbia and Montenegro
  #MNE		Montenegro ===> ignore as there is also Serbia and Montenegro
  cgroups <- c("15", "164", "206", "228", "248", "41", "51", "62", "SRB", "MNE")
  faoall <- faoall[! Country_code_A3 %in% cgroups]
  
  
  # Countries in FAO which are not in EDGAR
  #AND		Andorra  ===> merge to Spain
  #LIE		Liechtenstein ===> merge to Austria
  #MCO		Monaco ===> merge to France
  #VAT		Holy See ===> merge to Italy
  #SMR		San Marino ===> mwerge to Italy
  faoall[Country_code_A3=="AND", Country_code_A3 := "ESP"]
  faoall[Country_code_A3=="LIE", Country_code_A3 := "AUT"]
  faoall[Country_code_A3=="MCO", Country_code_A3 := "FRA"]
  faoall[Country_code_A3=="VAT", Country_code_A3 := "ITA"]
  faoall[Country_code_A3=="SMR", Country_code_A3 := "ITA"]
  
  
  #"Colony"-islands
  #CUW		Curaçao ===> merge to Netherlands
  #SXM		Sint Maarten (Dutch part) ===> merge to Netherlands
  #MAF		Saint-Martin (French part) ==> merge to France
  #IMN		Isle of Man ===> merge to UK
  #PCN		Pitcairn ===> merge to UK
  #SJM		Svalbard and Jan Mayen Islands ===> merge to Norway
  faoall[Country_code_A3=="CUW", Country_code_A3 := "NLD"]
  faoall[Country_code_A3=="SXM", Country_code_A3 := "NLD"]
  faoall[Country_code_A3=="MAF", Country_code_A3 := "FRA"]
  faoall[Country_code_A3=="IMN", Country_code_A3 := "GBR"]
  faoall[Country_code_A3=="PCN", Country_code_A3 := "GBR"]
  faoall[Country_code_A3=="SJM", Country_code_A3 := "NOR"]
  
  #PSE		Palestine ===> 
  #SSD		South Sudan ===> merge to Sudan
  faoall[Country_code_A3=="SSD", Country_code_A3 := "SDN"]
  faoall[Country_code_A3=="PSE", Country_code_A3 := "ISR"]
  
  fao <- faoall[, .(value=sum(Value)), by=.(Country_code_A3, Substance, IPCC_for_std_report_detailed, 
                                            FOOD_system_stage, FOOD_system_stage_detailed, FOOD_system_compartment, part, Year)]
  
  save(fao, file=paste0(edgar_folder, "FAO_", format(Sys.time(), "%Y%m%d"), ".rdata"))  
  
  # For comparison
  ipcclulucf <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "data (CO2-LULUCF)"))
  ipcclulucfglobal <- ipcclulucf[, sum(.SD), by=c("year"), .SDcols=c("blue", "houghton", "oscar", "mean")]
  
  return(fao)
}

mergeedgarfood <- function(){
  
  edgar <- openEDGAREmission()
  fao <- openFAOemissions()  
  edgarfao <- rbind(edgar, fao)
  setnames(edgarfao, 
           c("Country_code_A3", "Substance", "FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment", "IPCC_for_std_report_detailed", "value"),
           c("Country_code_A3", "gas", "stage", "stagedet", "compartment", "ipcc", "ktgas"))
  
  
  # Load IPCC GWPs - EDGAR provided the data using AR5-GWP
  # They need to be converted for AR6
  ipccgwps <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "100_yr_gwps"))
  ipccgwpch4 <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "CH4_gwps"))
  ipccgwpch4 <- unique(ipccgwpch4[, .(ipcc=sector_code, gwp_ar6)])
  
  edgarfaogas <- merge(edgarfao[gas!="CH4"],ipccgwps, by="gas")
  edgarfaoch4 <- merge(edgarfao[gas=="CH4"], unique(ipccgwpch4[, .(ipcc, gwp_ar6)]), by = "ipcc")
  edgarfood <- rbind(edgarfaogas, edgarfaoch4)
  edgarfood <- edgarfood[! is.na(ktgas)]
  setkey(edgarfood, Country_code_A3, Year, ipcc, stage, gas)
  edgarfood[, ktCO2eq := ktgas * gwp_ar6]

  fname <- paste0(edgar_folder, "edgar_food_", format(Sys.time(), "%Y%m%d"), ".rdata")
  save(ipccgwps, ipccgwpch4, file=paste0(edgar_folder, "gwps_", format(Sys.time(), "%Y%m%d"), ".rdata"))
  save(edgarfood, file=fname)
  return(fname) 
  
}

recalcGWP <- function(edgarfood=edgarfood, gwp='ar4'){
  
  load(paste0(edgar_folder, "gwps_", format(Sys.time(), "%Y%m%d"), ".rdata"))
  if(gwp=='ar4'){
    #From code Efisio - but F-gases missing
    ipccgwps[, gwp_ar4 := ifelse(gas=="CH4", 25, ifelse(gas=="N2O", 298, gwp_ar6))]
    edgarfood <- merge(edgarfood, ipccgwps, by="gas")
    edgarfood[, ktCO2eq := ktgas * gwp_ar4]
  }
  fname <- paste0(edgar_folder, "edgar_food_", format(Sys.time(), "%Y%m%d"), "_", gwp, ".rdata")
  save(edgarfood, file=fname)
  return(fname) 
  
}

calculateGlobalShares <- function(dt = edgarfood, gwp){
  
  curdt <- copy(dt)
  #setnames(curdt, gwp, "value")
  
  calculateShares <- function(curregions){
    
    curregions <- curregions[, shareExcLULUCF := EDGAR_FOOD/EDGAR_total]
    curregions <- curregions[, shareIncLULUCF := Foodtotal/Total]
    curregions <- curregions[, shareExcFAONFood := Foodtotal/Total_exc_FAO_nFood]
    
    
    return(curregions)  
    
  }
  
  foodsystemelements <- c("EDGAR_FOOD", "EDGAR_nFOOD", "FAO_FOOD", "FAO_nFOOD")
  calctot <- function(dt = edgarfood){
    dtcalctot <- copy(dt)
    totemissions <- dtcalctot[, sum(ktCO2eq, na.rm=TRUE), by=.(Country_code_A3, Year, part)]
    totemissions <- dcast.data.table(totemissions[Country_code_A3 != ""], Country_code_A3 + Year ~ part, value.var = 'V1', fill=0)
    totemissions[, sum(.SD, na.rm = TRUE), by=.(Year), .SDcols=foodsystemelements]
    return(totemissions)  
  }
  totemissions <- calctot(dt = curdt)
  
  alldata <- totemissions[, .(
    country=Country_code_A3, year=Year, 
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
  
  fn <- paste0(edgar_folder, "EDGAR-FOOD_by_globalshares", "_", gwp, "_", format(Sys.time(), "%Y%m%d"), ".rdata")
  flx <- gsub(".rdata", ".xlsx", fn)
  saveWorkbook(wb = fx, file=flx, overwrite = TRUE)
  save(alldata, totemissions, global, dev, ind, foodsystemelements, file=fn)
  
  return(fn)
}

calculateStages <- function(curdt=edgarfood, grouping='stagedet', gwp='ar6'){
  
  
  curstages <- copy(curdt)
  #setnames(curstages, gwp, "value")
  
  curstages <- curstages[ktCO2eq != 0]
  devtable <- unique(countrytable[, .(Country_code_A3, dev_country, C_group_IM24_sh)])
  global <- merge(curstages, devtable, by="Country_code_A3")
  setnames(global, grouping, "grouping")
  global <- global[part=="EDGAR_nFOOD", grouping := "EDGAR_nFOOD"]
  global <- global[part=="FAO_nFOOD", grouping := "FAO_nFOOD"]
  setnames(global, c("Year", "dev_country", "C_group_IM24_sh"), c("year", "region", "cgroup"))
  global <- global[part != "FAO_total"]
  
  globals <- global[, sum(ktCO2eq, na.rm=TRUE), by=.(year, part, region, grouping)]
  globalr <- global[, sum(ktCO2eq, na.rm=TRUE), by=.(year, part, cgroup, grouping)]
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
    stagesd <- stagesd[, .(region, year, LULUCF, Production, Transport, Processing, Packaging, 
                           Retail, Consumption, EoL=End_of_Life, EDGAR_nFOOD, FAO_nFOOD)]
    
  } else if (grouping == "compartment"){
    stagesd <- stagesd[, .(region, year, LandbasedEDG, LandbasedFAO, Energy, Industry, Waste,
                           EDGAR_nFOOD, FAO_nFOOD)]
  }
  stagenames <- setdiff(names(stagesd), c("year", "region"))
  stagesfood <- setdiff(stagenames, c("EDGAR_nFOOD", "FAO_nFOOD"))
  stagesexcl <- setdiff(stagenames, c("LULUCF", "Landbased_FAO", "FAO_nFOOD"))
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
  resfile <- paste0(edgar_folder, "EDGAR-FOOD_by_", groupname, "_", gwp, "_", format(Sys.time(), "%Y%m%d"), ".rdata")
  saveWorkbook(fx, file=gsub("rdata", "xlsx", resfile), overwrite = TRUE)
  save(list = c(paste0("emBy", groupname), paste0("sharBy", groupname)), dev, ind, file=resfile)
  
  return(resfile)
  
}

Global_byGasPart <- function(curdt = edgarfood, gwp=ar6){
  
  edt <- copy(curdt)
  #setnames(edt, gwp, "value")
  
  # Get sector names
  ipcccat <- as.data.table(read.xlsx(xlsxFile = paste0(edgar_folder, "../classifications/IPCC_master_categories.xlsx")))
  ipcccat <- unique(ipcccat[, .(code, IPCC_AR2_description)])
  edt[, sec := substr(ipcc, 1, 1)]
  edt <- merge(edt, ipcccat, by.x="sec", by.y = "code")
  edt[, sector:=IPCC_AR2_description] 
  
  # Assing indirect N2O emissions to main sectors
  edt[ipcc%in%c("7B1", "7C1"), sector:="Energy"]
  edt[ipcc%in%c("7B2", "7C2"), sector:="Industrial Processes"]
  edt[ipcc%in%c("7B1", "7C1"), sec:=1]
  edt[ipcc%in%c("7B2", "7C2"), sec:=2]
  
  edt <- edt[, .(sec, sector, part, Country_code_A3, gas, year=Year, ktCO2eq)]
  
  # Change names
  em3sector <- edt[, .(year, sec, sector, part, Country_code_A3, gas, ktCO2eq)]
  em3sector <- em3sector[, sector := gsub(" \\(please specify\\)", "", sector)]
  em3sector <- em3sector[, sector := gsub(",", "", sector)]
  em3sector[grepl("F", gas), gas:="F-gases"]
  
  # Aggregate by year, sector, substance
  em3sector <- em3sector[, sum(ktCO2eq), by=.(year, sec, sector, part, gas)]
  
  # Convert from kt CO2eq to Mt CO2eq per year
  em3sector <- em3sector[, .(year, sec, sector, part, gas, ktCO2eq=V1/1000)]
  em3sector <- em3sector[, sector := paste0(sec, " ", sector)]
  
  # Calculate totals
  em3sectort <- dcast.data.table(em3sector, year+sec+sector+gas ~ part, value.var = "ktCO2eq", fill = 0)
  em3sectort[, `:=` (EDGAR_total = EDGAR_FOOD + EDGAR_nFOOD,
                     FAO_total = FAO_FOOD + FAO_nFOOD,
                     FOOD_total = EDGAR_FOOD + FAO_FOOD,
                     total = EDGAR_FOOD + EDGAR_nFOOD + FAO_FOOD + FAO_nFOOD)]
  em3sectort[sec==7, sector:= '7 Indirect']
  
  em3sectort[, FOOD_bysec := sum(FOOD_total), by=.(year, sec)]
  em3sectort[, ALL_total := sum(total), by=.(year)]
  em3sectort[, ALLFOOD_total := sum(FOOD_total), by=.(year)]
  
  fn <- paste0(edgar_folder, "EDGAR-FOOD_by_Gas_", gwp, "_", format(Sys.time(), "%Y%m%d"), ".rdata")
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
  ipcctable <- foodemissions[year %in% c(1990, 2015, 2018) & !grepl("^7", sector), .(year, sector, gas, FOOD, share)]
  
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
  
  fn <- paste0(edgar_folder, "IPCC_table_", gwp, "_", format(Sys.time(), "%Y%m%d"), ".rdata")
  save(ipcctable, file=fn)
  
  #ipcctable[, (paste0("share_", ghgs)) := lapply(.SD, paste0, "%"), .SDcols = paste0("share_", ghgs)]
  #ipcctable[ipcctable == "NA%"] <- " - "
  ipcctable[ipcctable == "NA"] <- " - "
  
  wb <- createWorkbook()
  ws <- addWorksheet(wb, sheetName = "Table 12.7")
  header <- c("Sector", ghgs, ghgs)
  writeData(wb, startRow = 1, t(header), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 2, t(c("", "Emissions (Mt gas yr-1)", rep("", 4), "Share of total sectoial emissions (%)", rep("", 4))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 3, t(c("", "1990", rep("", 4+5))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 4, ipcctable[year==1990, -"year", with=FALSE], sheet = 'Table 12.7', startCol = 1, colNames = FALSE, keepNA = TRUE, na.string = " - ")
  writeData(wb, startRow = 11, t(c("", "2015", rep("", 4+5))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 12, ipcctable[year==2015, -"year", with=FALSE], sheet = 'Table 12.7', startCol = 1, colNames = FALSE, keepNA = TRUE, na.string = " - ")
  writeData(wb, startRow = 20, t(c("", "2018", rep("", 4+5))), sheet = 'Table 12.7', startCol = 1, colNames = FALSE)
  writeData(wb, startRow = 21, ipcctable[year==2018, -"year", with=FALSE], sheet = 'Table 12.7', startCol = 1, colNames = FALSE, keepNA = TRUE, na.string = " - ")
  saveWorkbook(wb, file = gsub("rdata", "xlsx", fn), overwrite = TRUE)
  return(fn)
  
}


recalcemissions <- FALSE
doglobshares <- FALSE
docalcstages <- FALSE
docalcsector <- FALSE
writeIPCCtable <- TRUE

if(recalcemissions){
  load(mergeedgarfood())
  write(paste0("edgar_food_", today(), ".rdata"), paste0(edgar_folder, "/edgar_food_last.txt"))
}else{
  toload <- paste0(edgar_folder, scan(paste0(edgar_folder, "/edgar_food_last.txt"), what = "character"))
  load(toload)
}
#gwp <- "ar4"
#if(doglobshares) {load(calculateGlobalShares(dt = edgarfood, gwp = gwp))}
#if(docalcstages) {load(calculateStages(edgarfood, "stagedet", gwp))}
#if(docalcsector) {load(calculateStages(edgarfood, "compartment", gwp))}
#if(writeIPCCtable) {load(IPCC_table(edgarfood, gwp))}
gwp <- "ar6"
if(doglobshares) {load(calculateGlobalShares(dt = edgarfood, gwp = gwp))}
if(docalcstages) {load(calculateStages(edgarfood, "stagedet", gwp))}
if(docalcsector) {load(calculateStages(edgarfood, "compartment", gwp))}
if(writeIPCCtable) {load(IPCC_table(edgarfood, gwp))}

stop()

#20210920
load(recalcGWP(edgarfood, gwp = 'ar4'))
load(calculateGlobalShares(dt = edgarfood, gwp = 'ar4'))




#20210918
#--> Test for AR4 (as far as known)

# from function mergeedgarfood <- function(){
  
  edgar <- openEDGAREmission()
  fao <- openFAOemissions()  
  edgarfao <- rbind(edgar, fao)
  setnames(edgarfao, 
           c("Country_code_A3", "Substance", "FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment", "IPCC_for_std_report_detailed", "value"),
           c("Country_code_A3", "gas", "stage", "stagedet", "compartment", "ipcc", "ktgas"))
  
  
  # Load IPCC GWPs - EDGAR provided the data using AR5-GWP
  # They need to be converted for AR6
  ipccgwps <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "100_yr_gwps"))
  ipccgwpch4 <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "CH4_gwps"))
  ipccgwpch4 <- unique(ipccgwpch4[, .(ipcc=sector_code, gwp_ar6)])
  
  #From code Efisio - but F-gases missing
  ipccgwps[, gwp_ar4 := ifelse(gas=="CH4", 25, ifelse(gas=="N2O", 298, gwp_ar6))]
  
  #edgarfaogas <- merge(edgarfao[gas!="CH4"],ipccgwps, by="gas")
  #edgarfaoch4 <- merge(edgarfao[gas=="CH4"], unique(ipccgwpch4[, .(ipcc, gwp_ar6)]), by = "ipcc")
  #edgarfood <- rbind(edgarfaogas, edgarfaoch4)
  edgarfood <- merge(edgarfao, ipccgwps, by="gas")
  
  
  edgarfood <- edgarfood[! is.na(ktgas)]
  setkey(edgarfood, Country_code_A3, Year, ipcc, stage, gas)
  edgarfood[, ktCO2eq := ktgas * gwp_ar4]
  
load(calculateGlobalShares(dt = edgarfood, gwp = gwp))

