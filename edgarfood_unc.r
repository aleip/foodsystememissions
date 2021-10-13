require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

rm(list=ls())

####################################################################################################
## 
## Options: 
## TRUE  : do step 
## FALSE : load previous results
  do.importfood <- FALSE
  do.importtotal <- FALSE
  do.aggregate <- TRUE
##
##
####################################################################################################

  
  # Questions to clarify:
  # - what sector belongs LULUCF? According to IPCC2006, it is Sector 4 and Waste Sector 5.
  #   but the data are different. Why has it been set to '3.B' in the files CH4_luluc_GWP100.csv?
  #   

gwpch4 <-  28  #  28 GWP of IPCC-AR5 for CH4
gwpn2o <- 265  # 265 GWP of IPCC-AR5 for N2O

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
jrcbox <- paste0("C:/Users/", myuser, "/ownCloud/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(jrcbox, "EDGAR-FOOD/202108/")
edgar_uncfolder <- paste0(jrcbox, "EDGAR-FOOD/202006_unc/")
edgar_uncfolder <- paste0(edgar_folder, "unc/")

if(FALSE) extractEDGARprocesses(unc.table)

source("edgarfood_unc_functions.r")
source("edgarfood_unc_loaddata.r")
source("edgarfood_unc_aggregate.r")
source("../capriextract/f_tools.r")

load(f.loadfood(doload = FALSE)) #FALSE to reload rdata file, TRUE to load from EDGAR exports
load(f.loadtotal(doload = FALSE))
toload <- paste0(edgar_folder, scan(paste0(edgar_folder, "/edgar_food_last.txt"), what = "character"))
load(toload)

write.xlsx(head(unc.table_foodshare), file=paste0(edgar_uncfolder, "unc.table_foodshare_head.xlsx"))
write.xlsx(head(unc.total), file=paste0(edgar_uncfolder, "unc.total_head.xlsx"))
uncmap <- unique(unc.total[, .(processes, ipcc06, ipccX)])
uncmap[, ipccdet := gsub("\\.", "", ipccX)]

ipccx <- unique(uncmap$ipccdet)
ipccd <- unique(A$IPCC_for_std_report_detailed)


sort(ipccd[! ipccd %in% ipccx])
sort(ipccx[! ipccx %in% ipccd])
ipccx[ ipccx %in% ipccd]


# See edgar-food.V6.r
#fileAB <- paste0(edgar_input, "IPCC_input_for_adrian_06082021.xlsx")
#A <- as.data.table(read.xlsx(fileAB, sheet = "EDGAR_GHG_food_EMI", startRow = 2, na.strings = "NULL"))
write.xlsx(head(A), file=paste0(edgar_uncfolder, "edgarfood_input.xlsx"))


# uniquefields <- c("processes", 
#                   "FOOD_system_stage_detailed",  #Works also with FOOD_system_stage
#                   "ipcc06", #There are a few extra lines
#                   "gas")

  

#Level below which emission factors are assumed to be correlated
#Emission factors at/above this level are assumed to be uncorrelated 

## IPCC defines the uncertainty for CH4 and N2O by category
## (for example it says:
## Public Power, co-generation and district heating Commercial, Institutional and Residential 
## combustion Industrial combustion: CH4 uncertainty in the range 50 to 150%).
## 
## For CO2 the uncertianty is defined by the carbon content of the fuel, 
## and IPCC gives detailed tables for that.
## Therefore I assumed that all processes using the same fuel should be considered 
## as correlated (even if in different categories, e.g. Energy and Agriculture). 
## EDGAR's codes hold the fuel info in the third group of three letters of each code.

## Define uncertainty correlations by category by default
## ==> Second level in ipcc06 categorization

# ---> Define CorrLevel which is used for the first step (partial correlated aggregation)  
file.agg=paste0(edgar_folder, "unc_elements_aggregates.rdata")
if(do.aggregate) {
  
  unc.table <- rbind(food=unc.table_foodshare, 
                     total=unc.total, fill = TRUE, idcol = TRUE)
  
  unc.table <- unc.table[emi != 0]
  unc.table[, sector := substr(ipcc06, 1, 1)]
  unc.table[, CorrLevel := substr(ipcc06, 1, 3)]
  unc.table[is.na(FOOD_system_stage), FOOD_system_stage := "TOTAL"]
  unc.table[is.na(FOOD_system_stage_detailed), FOOD_system_stage_detailed := "TOTAL"]
  unc.table[is.na(FOOD_system_compartment), FOOD_system_compartment := "TOTAL"]
  unc.table[grepl("^1", ipcc06) & gas=="CO2", CorrLevel := substr(processes, 9, 11)]
  setnames(unc.table, "FOOD_system_stage_detailed", "stage")
  # ---> Ensure that all emissions are captured, define level of correlation generically
  unc.table[is.na(CorrLevel), CorrLevel := sector]
  
  reuseAR5data <- TRUE
  if(reuseAR5data){ 
    convertAR5_to_AR6 
    #Align the sectors to ipcc96 which is used in the edgarfood data
    unc.table[, sector := ifelse(grepl("^2.[DG]", ipcc06), 3, 
                                 ifelse(sector==3, 4, 
                                        ifelse(sector==4, 6, 
                                               ifelse(sector==5, 7, 
                                                      ifelse(sector=="L", 5, sector)))))]
  }
  emi_country_gas_stage_g1 <- aggsector(unc.table = unc.table)
  if(reuseAR5data){
    emi_country_gas_stage_g1[, emishare := emi/sum(emi), by=.(country, gas, sector, stage)]
    edarsector <- copy(edgarfood)
    edarsector <- edarsector[! part %in% c("EDGAR_nFOOD", "FAO_nFOOD") & Year==2018]
    edarsector[part %in% c("EDGAR_total", "FAO_total"), stagedet := "TOTAL"]
    edarsector[stagedet=="LULUCF", stagedet := "LULUC"]
    edarsector[, sector := substr(ipcc, 1, 1)]
    edarsector <- edarsector[, .(ktCO2eq = sum(ktCO2eq, na.rm = TRUE)), 
                             by=.(Country_code_A3, gas, sector, stagedet)]
    edarsector[grepl("HFC", gas) | gas=="SF6", gas := "Fgases"]
    emimerge <- merge(emi_country_gas_stage_g1, edarsector[, .(country=Country_code_A3, gas, sector, stage=stagedet, ktCO2eq)], 
                      by=c("country", "gas", "sector", "stage"), all = TRUE)
    write.xlsx(emimerge[is.na(ktCO2eq)], file="emimerge_na.xlsx")
    emimerge <- emimerge[! is.na(ktCO2eq) & ktCO2eq != 0]
    emimerge[is.na(emishare) & stage != "TOTAL", `:=` (emishare = 1, `.id` = "food")]
    emimerge[is.na(emishare) & stage == "TOTAL", `:=` (emishare = 1, `.id` = "total")]
    emimerge[, emi := emishare * ktCO2eq]
    emimerge[is.na(CorrLevel) & sector==5, CorrLevel:="LUL"]
    # Missing Correlation Levels --> provisional assignment (as they are very few)
    # Attention - sectors are for IPCC96, CorrLevl for IPCC06
    emimerge[is.na(CorrLevel) & sector==2, CorrLevel:="2"]
    emimerge[is.na(CorrLevel) & sector==6, CorrLevel:="4.C"] # Combustion
    emimerge[is.na(CorrLevel) & sector==4, CorrLevel:="3.A"] # Combustion
    emimerge[is.na(CorrLevel) & sector==1, CorrLevel:="1.A"] # Combustion
    
    # Fill missing uncertainties with 0.5
    emimerge[is.na(unc.max), `:=` (unc.min=0.5, unc.max=0.5)]
    
  }
  
  l <- agguncertainty(emi_country_gas_stage_g1 = emimerge, file.agg = file.agg)
  unc.elements <- l[[1]]
  emiall <- l[[2]]
  emiall[, `:=` (emi=emi/1000000, emi.low=emi * (1-unc.min)/1000000, emi.hig = emi * (1 + unc.max)/1000000)]
  save(unc.table, emimerge, emiall, unc.elements, file=paste0(edgar_uncfolder, "unc.table.rdata"))
  write.xlsx(emiall[country=="ALL" & gas=="GHG" & stage=="TOTAL" & `.id`=="food"][order(`.id`, sector)], 
             file = paste0(edgar_uncfolder, "emi_ranges4_ipcc12411.xlsx"))
  curfile <- paste0(edgar_folder, "unc.table_total.rdata"); if(file.exists(curfile)) file.remove(curfile)
  curfile <- paste0(edgar_folder, "unc.table_food.rdata"); if(file.exists(curfile)) file.remove(curfile)
}else{
  load(file.agg)   
  load(file=paste0(edgar_folder, "unc.table.rdata"))
}
#
# ---> Steps 3: Calculate asymmetric distributions
#               Determing emission ranges
# 
file.asym=paste0(edgar_folder, "emi_asym.rdata")
emi_asym <- f.asymmetric(emi)
emi_asym <- emi_asym[! is.na(country)]
save(emi_asym, file=file.asym)
#
#  --- Step 4: Calculate shares
#

emi.shares <- emissionSharesUncertainty(emi_asym, gas1 = "CH4")[] 
emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, gas1 = "N2O")[] 
emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, gas1 = "CO2")[] 
for(i in unique(emi_asym$stage)){
  if(i == "TOTAL"){next()}
  emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, stage1 = i)[] 
}
for(i in unique(emi_asym$sector)){
  for(j in unique(emi_asym$gas)){
    emi.shares <- emissionSharesUncertainty(dtorig = emi_asym, emi_old = emi.shares, 
                                            sector1 = i, sector2 = i, id1 = "food", id2 = "total",
                                            gas1 = j, gas2 = j)
  }
}

ipccunc <- emi_asym[.id=="food" & country=="ALL" & stage == "TOTAL" & gas == "GHG" |
                      .id=="food" & country=="ALL" & gas == "GHG" & sector=="TOTAL" |
                      .id=="food" & country=="ALL" & stage == "TOTAL" & gas == "GHG" & sector=="L" ]
ipccunc[, `:=` (emi=emi/1000000, emi.unc.lo=emi.unc.lo/1000000, emi.unc.up=emi.unc.up/1000000)][]

fx <- createWorkbook()
addWorksheet(fx, sheetName = "shares")
writeData(fx, sheet = "shares", emi.shares)
addWorksheet(fx, sheetName = "levels")
writeData(fx, sheet = "levels", ipccunc)
saveWorkbook(fx, file = paste0(edgar_folder, "emishares.xlsx"), overwrite = TRUE)
