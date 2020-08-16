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
  do.aggregate <- FALSE
  do.calcasym <- FALSE
##
##
####################################################################################################


gwpch4 <-  28  #  28 GWP of IPCC-AR5 for CH4
gwpn2o <- 265  # 265 GWP of IPCC-AR5 for N2O

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "/projects/edgar/data/202006_unc/")

if(FALSE) extractEDGARprocesses(unc.table)

source("edgarfood_unc_functions.r")
source("edgarfood_unc_loaddata.r")
source("edgarfood_unc_aggregate.r")

load(f.loadfood(doload = do.importfood)) #FALSE to reload rdata file, TRUE to load from EDGAR exports
load(f.loadtotal(doload = do.importtotal))

unc.table <- rbind(food=unc.table_foodshare, 
                   total=unc.table, fill = TRUE, idcol = TRUE)

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
  unc.table[, sector := substr(ipcc06, 1, 1)]
  unc.table[, CorrLevel := substr(ipcc06, 1, 3)]
  unc.table[grepl("^1", ipcc06) & gas=="CO2", CorrLevel := substr(processes, 9, 11)]
  # ---> Ensure that all emissions are captured, define level of correlation generically
  unc.table[is.na(CorrLevel), CorrLevel := sector]
  
  # Aggregate by country, gas and stage
  
  l <- agguncertainty(unc.table, file.agg = file.agg)
  unc.elements <- l[[1]]
  emi <- l[[2]]
}else{
  load(file.agg)   
}
#
# ---> Steps 3: Calculate asymmetric distributions
#               Determing emission ranges
# 
file.asym=paste0(edgar_folder, "emi_asym.rdata")
emi_asym <- f.asymmetric(emi)
save(emi_asym, file=file.asym)
#
#  --- Step 4: Calculate shares
#

emi.shares <- emissionSharesUncertainty(emi_asym, gas1 = "CH4")[] 
emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, gas1 = "N2O")[] 
emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, gas1 = "CO2")[] 
for(i in unique(emi_asym$FOOD_system_stage_detailed)){
  if(i == "TOTAL"){next()}
  emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, stage1 = i)[] 
}
save(emi_asym, emi.shares, file=paste0(edgar_folder, "emi_uncertainties.rdata"))
