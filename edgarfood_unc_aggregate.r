agguncertainty <- function(unc.table, file.agg="unc_elements_aggregates.rdata"){
  
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
    ), by=.(.id, country, FOOD_system_stage_detailed, gas, CorrLevel, sector)]
  #
  # ---> Steps 2: Aggregation without correlated emissions
  # 
  emi_country_gas_stage_g1[, bCountry := FALSE]
  emi_country_gas_stage_g1[, xFlag := FALSE]
  emi_country_gas_stage_sector <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
    ), by = .(.id, country, FOOD_system_stage_detailed, gas, sector)]
  
  emi_gas_stage_sector <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
    ), by = .(.id, FOOD_system_stage_detailed, gas, sector)]
  emi_gas_stage_sector$country <- "ALL"
  
  emi_gas_stage <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
    ), by = .(.id, FOOD_system_stage_detailed, gas)]
  emi_gas_stage$country <- "ALL"
  emi_gas_stage$sector <- "TOTAL"
  
  emi_gas <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
    ), by = .(.id, gas)]
  emi_gas$country <- "ALL"
  emi_gas$sector <- "TOTAL"
  emi_gas$FOOD_system_stage_detailed <- "TOTAL"
  
  
  emi_stage <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
    ), by = .(.id, FOOD_system_stage_detailed)]
  emi_stage$country <- "ALL"
  emi_stage$sector <- "TOTAL"
  emi_stage$gas <- "GHG"
  
  emi <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), bCountry = bCountry, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), bCountry = bCountry, xFlag = xFlag)
    ), by = .(.id)]
  
  
  emi$country <- "ALL"
  emi$sector <- "TOTAL"
  emi$gas <- "GHG"
  emi$FOOD_system_stage_detailed <- "TOTAL"
  emi <- rbind(emi_country_gas_stage_sector, 
               emi_gas_stage_sector, 
               emi_gas_stage, 
               emi_stage, 
               emi_gas, 
               emi)
  
  save(emi_country_gas_stage_g1, emi, file=file.agg)
  return(list(emi_country_gas_stage_g1, emi))
  
}
