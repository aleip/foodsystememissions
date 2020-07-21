require(data.table)
require(openxlsx)
require(ggplot2)

rm(list=ls())

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "/projects/edgar/data/202006_unc/")

load(paste0(edgar_folder, "CO2_UncCorr_2015_unc.table_food_share.Rdata"))
unc.co2 <- unc.table
unc.co2$gas <- "CO2"
load(paste0(edgar_folder, "ch4_UncCorr_2015_unc.table_food_share.Rdata"))
unc.ch4 <- unc.table
unc.ch4$gas <- "CH4"
load(paste0(edgar_folder, "n2o_UncCorr_2015_unc.table_food_share.Rdata"))
unc.n2o <- unc.table
unc.n2o$gas <- "N2O"

unc.table <- unique(as.data.table(rbind(unc.co2, rbind(unc.ch4, unc.n2o))))
unc.table$xFlag <- F

uniquefields <- c("processes", 
                  "FOOD_system_stage_detailed",  #Works also with FOOD_system_stage
                  "ipcc06", #There are a few extra lines
                  "gas")

#Level below which emission factors are assumed to be correlated
#Emission factors at/above this level are assumed to be uncorrelated 
emi.keys.ch4 <- c('1.A', '1.B.1','1.B.2','2','3.A.1','3.A.2','3.C.7','4.A','4.D')
emi.keys.n2o <- c('1.A', '2.B','3.A', '3.C.1', '3.C.4',  '3.C.5', '3.C.6',  '4','5.A')
emi.keys.co2 <- c('1.A.1', '1.A.2','1.A.3','1.B', '2','3', '5')

for(s in emi.keys.co2){ unc.table[gas=="CO2" & grepl(s, ipcc06), CorrLevel := s] }
for(s in emi.keys.ch4){ unc.table[gas=="CH4" & grepl(s, ipcc06), CorrLevel := s] }
for(s in emi.keys.n2o){ unc.table[gas=="N2o" & grepl(s, ipcc06), CorrLevel := s] }

# To ensure that all emissions are captured, define level of correlation generically
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
  
  
  for (i in 1:length(unc)){
    if (is.na(emi[i]) | emi[i]==0.) {next()}
    if(emi[i] !=0 & is.na(unc[i])) {cat(' check i= ', i, '\n'); unc[i] <- 0}# {unc[i] <- 0} # mmhhh
    #   if(b.cat=='Margatita' & now_run =='CO2') {bCountry[i] <- F}
    #     bCountry[i] <- F # sensitivity case to see the effect of having implemented the bCountry flag option
    if (!xFlag[i]){ #exclude all biofuels
      if(bCountry[i]==T ){ #uncorrelated
        unc.cat <- sqrt((emi[i]*unc[i])^2 + unc.cat^2)
        
      } else if (bCountry[i]==F){ #correlated
        unc.cat <- (emi[i]*unc[i])+ unc.cat
      }
      
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

emi_gas_stage <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(FOOD_system_stage_detailed, gas)]

emi_stage <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  ), by = .(FOOD_system_stage_detailed)]


emi <- 
  emi_country_gas_stage_g1[, .(emi=sum(emi),
                               unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                               unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
  )]

#
# ---> Steps 3: Calculate asymmetric distributions
#               Determing emission ranges
# 
emi_country_gas_stage_sector_asym <- f.asymmetric(emi_country_gas_stage_sector)
emi_gas_stage_sector_asym <- f.asymmetric(emi_gas_stage_sector)
emi_gas_stage_sector_asym$country <- "ALL"
emi_gas_stage_asym <- f.asymmetric(emi_gas_stage)
emi_gas_stage_asym$country <- "ALL"
emi_gas_stage_asym$sector <- "TOTAL"
emi_stage_asym <- f.asymmetric(emi_stage)
emi_stage_asym$country <- "ALL"
emi_stage_asym$sector <- "TOTAL"
emi_stage_asym$gas <- "GHG"
emi_asym <- f.asymmetric(emi)
emi_asym$country <- "ALL"
emi_asym$sector <- "TOTAL"
emi_asym$gas <- "GHG"
emi_asym$FOOD_system_stage_detailed <- "TOTAL"

emi_asym <- rbind(emi_country_gas_stage_sector_asym, 
                        rbind(emi_gas_stage_sector_asym, 
                              rbind(emi_gas_stage_asym, 
                                    rbind(emi_stage_asym, emi_asym))))

#
#  --- Step 4: Calculate shares
#
returnshares <- function(dtorig, 
                         country1="ALL", stage1="TOTAL", gas1="GHG", sector1="TOTAL",
                         country2="ALL", stage2="TOTAL", gas2="GHG", sector2="TOTAL"){
  # Example
  country1 <- "ALL"; country2 <- "ALL"; stage1 <- "Packaging"; stage2 <- "Production"
  gas1 <- "GHG"; gas2 <- "GHG"; sector1 <- "TOTAL"; sector2 <- "TOTAL"
  
  
  dt <- copy(dtorig)
  emi1 <- dt[country==country1 & FOOD_system_stage_detailed==stage1 & gas==gas1 & sector==sector1]
  emi2 <- dt[country==country2 & FOOD_system_stage_detailed==stage2 & gas==gas2 & sector==sector2]
  emi <- emi1
  emi[, `:=` (
    country1 = country1,
    stage1 = stage1,
    gas1 = gas1,
    sector1 = sector1,
    country2 = country2,
    stage2 = stage2,
    gas2 = gas2,
    sector2 = sector2,
    share = emi1$emi / emi2$emi,
    min = sqrt(emi1$min^2 + emi2$min^2),
    max = sqrt(emi1$max^2 + emi2$max^2))]
  emi[, `:=` (
    share.unc.lo = share * (1 - min),
    share.unc.up = share * (1 + max))]
  
  return(emi) 
  
}


emi.shares <- returnshares(emi_asym, gas1 = "CH4") 


