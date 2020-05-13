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

addandwrite <- function(fx, shName, data){
  addWorksheet(fx, sheetName = shName)
  writeData(fx, sheet = shName, data)
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
  
  BCDm <- rbind(Bm, CDm)
  
  
  # Fill missing information
  BCDm <- BCDm[, `:=` (Substance = "CO2eq",
                       EDGAR_SECTOR = "FOLU", # Attention here 'C' still included, needs to be subtracted later
                       FOOD_system_stage = "",
                       FOOD_system_stage_detailed = "",
                       FOOD_system_compartment = "",
                       IPCC_for_std_report_detailed = "4")] 
  
  # bind all emission elements
  edgarfood <- rbind(ACm, BCDm)
  edgarfood <- edgarfood[, variable := as.numeric(gsub("Y_", "", variable))]
  edgarfood <- edgarfood[!is.na(value)]
  setnames(edgarfood, 
           c("FOOD_system_stage", "FOOD_system_stage_detailed", "FOOD_system_compartment", "IPCC_for_std_report_detailed"),
           c("stage", "stagedet", "compartment", "ipcc"))
  
  years <- gsub("Y_", "", years)
  totemissions <- edgarfood[, sum(value, na.rm=TRUE), by=.(Country_code_A3, variable, part)]
  totemissions <- dcast.data.table(totemissions[Country_code_A3 != ""], Country_code_A3 + variable ~ part, value.var = 'V1', fill=0)
  
  fname <- paste0(edgar_folder, "edgar_food_", format(Sys.time(), "%Y%m%d"), ".rdata")
  save(edgarfood, totemissions, countrytable, categorytable, file=fname)
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


load(openEmissionFiles())
shares <- calculateGlobalShares(totemissions)
load(calculateStages(edgarfood, "stagedet"))
load(calculateStages(edgarfood, "compartment"))
