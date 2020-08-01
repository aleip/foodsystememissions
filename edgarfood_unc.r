require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

rm(list=ls())

gwpch4 <- 1 #  28 GWP of IPCC-AR5 for CH4
gwpn2o <- 1 # 295 GWP of IPCC-AR5 for N2O

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "/projects/edgar/data/202006_unc/")

load(paste0(edgar_folder, "CO2_UncCorr_2015_unc.table_food_share.Rdata"))
unc.co2 <- unc.table
unc.co2$gas <- "CO2"
load(paste0(edgar_folder, "ch4_UncCorr_2015_unc.table_food_share.Rdata"))
unc.ch4 <- as.data.table(unc.table)
unc.ch4$gas <- "CH4"
unc.ch4[, emi := emi * gwpch4] #GWP of IPCC-AR5 for CH4
load(paste0(edgar_folder, "n2o_UncCorr_2015_unc.table_food_share.Rdata"))
unc.n2o <- unc.table
unc.n2o$gas <- "N2O"
unc.ch4[, emi := emi * gwpn2o] #GWP of IPCC-AR5 for N2O

unc.table <- unique(as.data.table(rbind(unc.co2, rbind(unc.ch4, unc.n2o))))
unc.table$xFlag <- F

uniquefields <- c("processes", 
                  "FOOD_system_stage_detailed",  #Works also with FOOD_system_stage
                  "ipcc06", #There are a few extra lines
                  "gas")

# Extract EDGAR processes
extractEDGARprocesses <- function(unc.table){
  processes <- unique(unc.table[, .(ipcc06, processes)])
  processes[, `:=` (proc1 = substr(processes, 1, 3),
                    proc2 = substr(processes, 5, 7),
                    proc3 = substr(processes, 9, 11),
                    proc4 = substr(processes, 13, 15))]
  wb <- createWorkbook(); 
  ws <- addWorksheet(wb, "proc1"); ws <- writeData(wb, "proc1", unique(processes[, .(proc1)]))
  ws <- addWorksheet(wb, "proc2"); ws <- writeData(wb, "proc2", unique(processes[, .(proc2)]))
  ws <- addWorksheet(wb, "proc3"); ws <- writeData(wb, "proc3", unique(processes[, .(proc3)]))
  ws <- addWorksheet(wb, "proc4"); ws <- writeData(wb, "proc4", unique(processes[, .(proc4)]))
  
  ws <- writeData(wb, "proc1", unique(processes[grepl("^1", ipcc06), .(proc1)]), startCol = 3)
  ws <- writeData(wb, "proc3", unique(processes[grepl("^1", ipcc06), .(proc3)]), startCol = 3)
  ws <- writeData(wb, "proc1", unique(processes[, .(ipcc06, proc1)]), startCol = 5)
  ws <- writeData(wb, "proc2", unique(processes[, .(ipcc06, proc2)]), startCol = 5)
  ws <- writeData(wb, "proc3", unique(processes[, .(ipcc06, proc3)]), startCol = 5)
  ws <- writeData(wb, "proc4", unique(processes[, .(ipcc06, proc4)]), startCol = 5)
  wb <- saveWorkbook(wb, file=paste0(edgar_folder, "EDGAR_processes.xlsx"), overwrite = TRUE)
}
extractEDGARprocesses(unc.table)


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
unc.table[, CorrLevel := substr(ipcc06, 1, 3)]
unc.table[grepl("^1", ipcc06) & gas=="CO2", CorrLevel := substr(processes, 9, 11)]
# ---> Ensure that all emissions are captured, define level of correlation generically
unc.table[is.na(CorrLevel), CorrLevel := substr(ipcc06, 1, 1)]

# Aggregate by country, gas and stage

# Code based on fnct in file functions_combining_uncertainty.R
# Efisio Solazzo, May 2020
f.comb_unc <- function(unc.EF, unc.AD) {
  if(!is.na(unc.EF) & !is.na(unc.AD)) {
    
    unc <- sqrt(unc.EF^2 + unc.AD^2) # eq3.1 IPCC guidelines 2006 (pag 3.28) )
  } else {
    unc <- NA
  }
  return(unc)
}

f.aggregate_subcategory <- function(
  emi,       # Numeric: Emission level
  unc,       # Numeric: Relative uncertainty. 
  bCountry,  # Logical: Flag on correlation across countries
  xFlag      # Flag indicating biofuel/no biofuel. EDGAR-FOOD data don't have this flag.
  
){
  
  # Set initial values
  unc.cat <- 0; unc.cat.x <-0; emi<-abs(emi)
  
  ## General formula for error propagation:
  ## simplified for the case of no combination coefficients
  ## 
  ## sigma(SUM_i(x_i))^2
  ## = SUM_i(sigma(x_i)^2)
  ##   + SUM_i ( SUM_j<>i (rho_ij * sigma_i * sigma_j))
  ##   
  ##  Simplified for rho_ij = 1 @ALL i, j
  
  ## Successively summation of uncertainties:
  ## See Bond et al 2004; Bergamaschi et al., 2017; Olivier et al., 2002: Netherland's NIR
  ## sigma(x+y)^2 = sigma(x)^2 + sigma(y)^2 + 2 sigma(x) sigma(y)
  ##              = [ sigma(x) + sigma(y) ]^2
  
  
  for (i in 1:length(unc)){
    if (is.na(emi[i]) | emi[i]==0.) {next()}
    # if(emi[i] !=0 & is.na(unc[i])) {cat(' check i= ', i, '\n'); unc[i] <- 0}# {unc[i] <- 0} # mmhhh
    # #   if(b.cat=='Margatita' & now_run =='CO2') {bCountry[i] <- F}
    # #     bCountry[i] <- F # sensitivity case to see the effect of having implemented the bCountry flag option
    if (!xFlag[i]){ #exclude all biofuels
      if(bCountry[i]==T ){ #uncorrelated
        
        ## SUM squares thus (i) square unc so far, (ii) add new unc, (iii) square root
        unc.cat <- sqrt( unc.cat^2 + (emi[i]*unc[i])^2 )
        
      } else if (bCountry[i]==F){ #correlated
        
        ## SUM standard deviations as for formula above
        unc.cat <- unc.cat + emi[i]*unc[i]
      }
       
    # if(emi[i] !=0 & is.na(unc[i])) {cat(' check i= ', i, '\n'); unc[i] <- 0}# {unc[i] <- 0} # mmhhh
    # if (!xFlag[i]){ #exclude all biofuels
    #     unc.cat <- unc.cat + sqrt((emi[i]*unc[i])^2)
    #   } if (bCountry[i]==F){ #correlated
    #     for(j in 1:length(unc)){
    #       # All correlations in this section are 'mutual' thus no matrix needed
    #       # Sum-up only for those which are flagged correlated
    #       if(is.na(emi[j] | emi[j]==0. | i==j | bCountry[j]==T)){next()}
    #       unc.cat <- unc.cat + (emi[i]*unc[i] + emi[j]*unc[j])
    #     }
    #   }
       
    } else { # do the same for biofuels
      if(bCountry[i]==T ){ #uncorrelated
        unc.cat.x <- sqrt((emi[i]*unc[i])^2 + unc.cat.x^2)
        
      } else if (bCountry[i]==F){ #correalted
        unc.cat.x <- (emi[i]*unc[i])+ unc.cat.x
      }
    } 
  }
  combined.unc <- f.comb_unc(unc.cat, unc.cat.x)
  rel.unc <- combined.unc/sum(emi, na.rm=T)
  
  return(rel.unc)
} # end function


f.asymmetric <- function(dtorig, 
                         dtn = c("emi", "unc.min", "unc.max"), #Current default names of relevant columns
                         dtoutput = FALSE,
                         flag = "olivier"
){
  
  # dt = data table with columns
  # dtn = names of columns giving mean, min and max
  # dtoutput: if TRUE returns all values calculated, keeping original values
  #           if FALSE overwrites uncertainty estimates with new ones
  # flag: if 'lognormal' use lognormal distribution
  #       if 'olivier' use Olivier et al 2002 [FULL REFERENCE??], bottom of table 9
  # mean = mean emissions
  # min = relative uncertainty to calculate the lower limit of the uncertainty range
  # max = relative uncertainty to calculate the upper limit of the uncertainty range
  
  # Capture column names if they are different from mean, min, max
  #         The order of the columns must be kept though!
  dt <- copy(dtorig)
  setnames(dt, dtn, c("emi", "min", "max"))
  
  dt[, emisign := sign(emi)]
  dt[, emi := abs(emi)]
  
  #Asymmetric uncertainty is calculated only when lower bound is >=50%
  #  ---> Added by ES on the 12 July YEAR 2020 ???. 
  #       Asymmetric uncertainty is calculated only when lower bound is >=50%
  #       as suggested by Margarita (mail 11 July 2019)
  dt <- dt[, asymetric := min >= 0.5]
  
  if(flag == "lognormal"){
    
    dt <- dt[asymetric == TRUE, `:=` (
      
      # geometric emi EQ 3.5 page 3.61 WHICH REFERENCE ???
      mu_g.min = exp(log(emi)-0.5*log(1+(min*100/200)^2)), 
      mu_g.max = exp(log(emi)-0.5*log(1+(max*100/200)^2)),
      
      # geometric standard deviation EQ 3.6 page 3.61
      sigma_g.max = exp(sqrt(log(1+(max*100/200)^2))),      
      sigma_g.min = exp(sqrt(log(1+(min*100/200)^2))))]
    
    # Calculate relative uncertainties from the emi
    dt <- dt[asymetric == TRUE, `:=` (CI.min = abs(exp(log(mu_g.min)-1.96*log(sigma_g.min))-emi)/emi,
                                      CI.max = abs(exp(log(mu_g.max)+1.96*log(sigma_g.max))-emi)/emi)]   
  }else if(flag == "olivier"){
    
    # see olivier et al 2002, bottom of table 9.
    dt <- dt[asymetric == TRUE, `:=` (CI.min = 1/(1+min), CI.max = max)]
    
  }
  
  dt <- dt[asymetric == FALSE, `:=` (CI.min = min, CI.max = max)]
  
  # Calculate absolute range of emissions
  dt[, emi := emi * emisign]
  dt <- dt[, `:=` (emi.unc.lo = emi * (1 - emisign * abs(CI.min)),
                   emi.unc.up = emi * (1 + emisign * abs(CI.max)))]
  
  # Prepare for output of function
  if(! dtoutput){
    dt <- dt[asymetric == TRUE, `:=` (min = CI.min, max = CI.max)]
    dt <- dt[, .SD, .SDcols = setdiff(names(dt), c("asymetric", "emisign", "mu_g.min", "mu_g.max", 
                                                   "sigma_g.min", "sigma_g.max", "CI.min", "CI.max"))]
  }
  
  return(dt)
  
}


# Stepwise aggregation. 
# First aggregate up to the level of correlation (bCountry influences if sub-categories are
#       correlated or un-correlated)
# Second, aggregation 'above' correlation level (thus always non-correlation assumed)
# Third step is to calculate asymmetric distributions for high uncertainly levels

# ---> Step 1: Aggregation considering (partially) correlated emission uncertainties

emi_country_gas_stage_g1 <- 
  unc.table[, .(emi=sum(emi),
                unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = iFlag, xFlag = xFlag),
                unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = iFlag, xFlag = xFlag)
  ), by=.(country, FOOD_system_stage_detailed, gas, CorrLevel)]
emi_country_gas_stage_g1[, sector := substr(CorrLevel, 1, 1)]

#
# ---> Steps 2: Aggregation without correlated emissions
# 
emi_country_gas_stage_g1[, bCountry := FALSE]
emi_country_gas_stage_g1[, xFlag := FALSE]
emi_country_gas_stage_sector <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(country, FOOD_system_stage_detailed, gas, sector)]

emi_gas_stage_sector <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(FOOD_system_stage_detailed, gas, sector)]
emi_gas_stage_sector$country <- "ALL"

emi_gas_stage <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(FOOD_system_stage_detailed, gas)]
emi_gas_stage$country <- "ALL"
emi_gas_stage$sector <- "TOTAL"

emi_gas <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(gas)]
emi_gas$country <- "ALL"
emi_gas$sector <- "TOTAL"
emi_gas$FOOD_system_stage_detailed <- "TOTAL"


emi_stage <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(FOOD_system_stage_detailed)]
emi_stage$country <- "ALL"
emi_stage$sector <- "TOTAL"
emi_stage$gas <- "GHG"

emi <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  )]
emi$country <- "ALL"
emi$sector <- "TOTAL"
emi$gas <- "GHG"
emi$FOOD_system_stage_detailed <- "TOTAL"
emi <- rbind(emi_country_gas_stage_sector, 
                  rbind(emi_gas_stage_sector, 
                        rbind(emi_gas_stage, 
                              rbind(emi_stage, 
                                    rbind(emi_gas, emi)))))


#
# ---> Steps 3: Calculate asymmetric distributions
#               Determing emission ranges
# 
emi_asym <- f.asymmetric(emi)
#
#  --- Step 4: Calculate shares
#
emissionSharesUncertainty <- function(dtorig, emi_old=NULL, isShare=TRUE, 
                                      country1="ALL", stage1="TOTAL", gas1="GHG", sector1="TOTAL",
                                      country2="ALL", stage2="TOTAL", gas2="GHG", sector2="TOTAL"){
  # 
  # Purpose of the function:
  # - Calculate uncertainty of 'emission shares'
  # - Input is a data table (dtorig) with two rows for which the share and its uncertainty is needed
  # - The rows are identified by country, food system stage, gas, and sector
  # - Optional the variable isShare=FALSE can be set if the uncertainty if the 
  #   ratio of two uncorrelated emissions need to be returned. Otherwise the function
  #   assumes that the smaller emissions source is fully part of the larger one.
  # - The resulting share and uncertainty is added to an existing data table (emi_old)
  #   containing previously calculated shares
  # 
  # Example
  doexample <- FALSE
  if(doexample){
    country1 <- "ALL"; country2 <- "ALL"; 
    stage1 <- "TOTAL"; stage2 <- "TOTAL"
    gas1 <- "GHG"; gas2 <- "GHG"; 
    sector1 <- "TOTAL"; sector2 <- "TOTAL"
  }
  # Calculation according to: 
  # f = A+B
  # sf = SQRT( (df/dA)^2 sA^2 + (df/dB)^2 * sB^2)
  # df/dA = B/(A+B)^2
  # df/dB = - A/(A+B)^2
  # sf = 1/(A+B)^2 * SQRT(A^2 sB^2 + B^2 sA^2)
  # sf = Term1 * SQRT(Term2 + Term3)
  
  dt <- copy(dtorig)
  
  # Clean column names
  dt <- dt[, -c("emi.unc.lo", "emi.unc.up"), with=FALSE]
  setnames(dt, "FOOD_system_stage_detailed", "stage")
  emi1 <- dt[country==country1 & stage==stage1 & gas==gas1 & sector==sector1]
  emi2 <- dt[country==country2 & stage==stage2 & gas==gas2 & sector==sector2]
  
  # Check which one is the share of the other
  correctOrder <- emi1$emi < emi2$emi
  if(correctOrder){
    setnames(emi1, names(emi1), paste0(names(emi1), "A"))
    setnames(emi2, names(emi2), paste0(names(emi2), "AB"))
  }else{
    setnames(emi1, names(emi1), paste0(names(emi1), "AB"))
    setnames(emi2, names(emi2), paste0(names(emi2), "A"))
  }
 
  # Combine both 
  emi <- cbind(emi1, emi2)
  
  # Calculate share
  emi[, share := emiA / emiAB]
  
  # Determine uncertainty of the residual B
  emi[, maxB := sqrt((maxAB*emiAB)^2 - (maxA*emiA)^2)/(emiAB-emiA)]
  
  if(isShare){
    # A is a part of AB --> more complex calculation as emissions are correlated
    emi[, `:=` (
      Term1 = 1/(emiAB)^2,
      Term2 = emiA^2 * ((emiAB-emiA) * maxB)^2,
      Term3 = (emiAB-emiA)^2 * (emiA * maxA)^2
    )]
    emi[, `:=` (
      min = Term1 * sqrt(Term2 + Term3),
      max = Term1 * sqrt(Term2 + Term3)
    )]
  }else{
    # A is not part of AB --> both emissions are uncorrelated
    emi[, `:=` ( min = sqrt( (emiA * minA)^2 + (emiAB * minAB)^2)/emiAB,
                 max = sqrt( (emiA * maxA)^2 + (emiAB * maxAB)^2)/emiAB)]
  }
  
  # Calculate lower and upper bound of confidence intervall
  emi[, `:=` (
    share.unc.lo = share * (1 - min),
    share.unc.up = share * (1 + max))]
  
  if(! is.null(emi_old)){
    emi <- rbind(emi_old, emi)
  }
  
  return(emi) 
}


emi.shares <- emissionSharesUncertainty(emi_asym, gas1 = "CH4")[] 
emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, gas1 = "N2O")[] 
emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, gas1 = "CO2")[] 
for(i in unique(emi_asym$FOOD_system_stage_detailed)){
  if(i == "TOTAL"){next()}
  emi.shares <- emissionSharesUncertainty(emi_asym, emi_old = emi.shares, stage1 = i)[] 
}
save(emi_asym, emi.shares, file=paste0(edgar_folder, "emi_uncertainties.rdata"))
