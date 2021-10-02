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
  do.importfood <- TRUE
  do.importtotal <- TRUE
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

if(FALSE) extractEDGARprocesses(unc.table)

source("edgarfood_unc_functions.r")
source("edgarfood_unc_loaddata.r")
source("edgarfood_unc_aggregate.r")
source("../capriextract/f_tools.r")

load(f.loadfood(doload = do.importfood)) #FALSE to reload rdata file, TRUE to load from EDGAR exports
load(f.loadtotal(doload = do.importtotal))

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
  
  # Aggregate by country, gas and stage
  
  l <- agguncertainty(unc.table, file.agg = file.agg)
  unc.elements <- l[[1]]
  emi <- l[[2]]
  save(unc.table, emi, unc.elements, file=paste0(edgar_folder, "unc.table.rdata"))
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
