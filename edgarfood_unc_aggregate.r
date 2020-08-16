agguncertainty <- function(unc.table, file.agg="unc_elements_aggregates.rdata"){
  
  # Stepwise aggregation. 
  # First aggregate up to the level of correlation (uncorr influences if sub-categories are
  #       correlated or un-correlated)
  # Second, aggregation 'above' correlation level (thus always non-correlation assumed)
  # Third step is to calculate asymmetric distributions for high uncertainly levels
  
  # ---> Step 1: Aggregation considering (partially) correlated emission uncertainties
  
  unc.table[, uncorr := FALSE]
  emi_country_gas_stage_g1 <- 
    unc.table[, .(emi=sum(emi),
                  unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                  unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by=.(.id, country, stage, gas, CorrLevel, sector)]
  print(emi_country_gas_stage_g1[, sum(emi, na.rm = TRUE), by = .(.id)][])
  
  
  #
  # ---> Steps 2a: Aggregation over countries - emissions are correlated by corrlevel
  # 
  emi_country_gas_stage_g1[, uncorr := FALSE]
  emi_country_gas_stage_g1[, xFlag := FALSE]
  emi_gas_stage_sector_corrlevel <- 
    emi_country_gas_stage_g1[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, stage, gas, sector, CorrLevel)]
  print(emi_gas_stage_sector_corrlevel[, sum(emi, na.rm = TRUE), by = .(.id)][])
  
  #
  # ---> Steps 2b: Aggregation without correlated emissions
  # 
  emi_gas_stage_sector_corrlevel[, uncorr := TRUE]
  emi_gas_stage_sector_corrlevel[, xFlag := FALSE]
  
  emi_gas_stage_sector <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, stage, gas, sector)]
  emi_gas_stage_sector$country <- "ALL"
  emi_gas_stage_sector[, sum(emi, na.rm = TRUE), by = .(.id)][]
  
  ### ---> Aggregate by country and one of: gas, stage, sector
  emi_gas_stage <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, stage, gas)]
  emi_gas_stage$country <- "ALL"
  emi_gas_stage$sector <- "TOTAL"
  emi_gas_stage[, sum(emi, na.rm = TRUE), by = .(.id)][]
  
  emi_sector_stage <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, sector, stage)]
  emi_sector_stage$country <- "ALL"
  emi_sector_stage$gas <- "GHG"
  
  emi_gas_sector <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, sector, gas)]
  emi_gas_sector$country <- "ALL"
  emi_gas_sector$stage <- "TOTAL"
  
  ### ---> Aggregate to totals by coountry and one of : gas, stage, sector
  emi_sector <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, sector)]
  emi_sector$country <- "ALL"
  emi_sector$stage <- "TOTAL"
  emi_sector$gas <- "GHG"
  
  emi_gas <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, gas)]
  emi_gas$country <- "ALL"
  emi_gas$sector <- "TOTAL"
  emi_gas$stage <- "TOTAL"
  
  
  emi_stage <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id, stage)]
  emi_stage$country <- "ALL"
  emi_stage$sector <- "TOTAL"
  emi_stage$gas <- "GHG"
  
  ### ---> aggregate to total emissions
  emi <- 
    emi_gas_stage_sector_corrlevel[, .(emi=sum(emi, na.rm = TRUE),
                                 unc.min=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.min), uncorr = uncorr, xFlag = xFlag),
                                 unc.max=f.aggregate_subcategory(emi = emi, unc = as.numeric(unc.max), uncorr = uncorr, xFlag = xFlag)
    ), by = .(.id)]
  
  
  emi$country <- "ALL"
  emi$sector <- "TOTAL"
  emi$gas <- "GHG"
  emi$stage <- "TOTAL"
  emi <- rbind(emi_gas_stage_sector, 
               emi_gas_stage, 
               emi_gas_sector,
               emi_sector_stage,
               emi_stage, 
               emi_sector,
               emi_gas, 
               emi)
  
  emi <- emi[emi != 0]
  emi <- unique(emi)
  
  save(emi_country_gas_stage_g1, emi_gas_stage_sector_corrlevel, emi, file=file.agg)
  return(list(emi_country_gas_stage_g1, emi))
  
}
