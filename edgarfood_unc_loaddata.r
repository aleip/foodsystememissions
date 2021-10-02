# These functions load historical uncertainty table & extract relative uncertainties

f.loadfood <- function(doload=TRUE){
  
  file.foodshare <- paste0(edgar_folder, "unc.table_food.rdata")
  
  if(doload){
    
    # Load data for food system emissions
    load(paste0(edgar_uncfolder, "CO2_UncCorr_2015_unc.table_food_share.Rdata"))
    uncfood.co2 <- as.data.table(unc.table); rm(unc.table)
    uncfood.co2$gas <- "CO2"
    load(paste0(edgar_uncfolder, "ch4_UncCorr_2015_unc.table_food_share.Rdata"))
    uncfood.ch4 <- as.data.table(unc.table); rm(unc.table)
    uncfood.ch4$gas <- "CH4"
    uncfood.ch4[, emi := emi * gwpch4] #GWP of IPCC-AR5 for CH4
    load(paste0(edgar_uncfolder, "n2o_UncCorr_2015_unc.table_food_share.Rdata"))
    uncfood.n2o <- as.data.table(unc.table); rm(unc.table)
    uncfood.n2o$gas <- "N2O"
    uncfood.n2o[, emi := emi * gwpn2o] #GWP of IPCC-AR5 for N2O
    
    uncfood.f <- fread(paste0(edgar_uncfolder, "F-gases_unc.table_food.csv"))
    uncfood.f <- uncfood.f[, -c(names(uncfood.f)[grepl("^V", names(uncfood.f))], "EDGAR_Sector", "GHGs"), with=FALSE]
    setnames(uncfood.f, 
             c("IPCC06", "Country_code_A3", "iFLAG"), 
             c("ipcc06", "country", "iFlag"))
    uncfood.f[, `:=` (ipccX="", gas="Fgases")]
    uncfood.f[, emi := as.numeric(emi)]
    
    ## Load LULUCF data
    unclulucffood.co2 <- fread(paste0(edgar_uncfolder, "CO2_luluc_GWP100.csv"))
    unclulucffood.co2$gas <- "CO2"
    unclulucffood.CH4 <- fread(paste0(edgar_uncfolder, "CH4_luluc_GWP100.csv"))
    unclulucffood.CH4$gas <- "CH4"
    unclulucffood.N2O <- fread(paste0(edgar_uncfolder, "N2O_luluc_GWP100.csv"))
    unclulucffood.N2O$gas <- "N2O"
    unclulucffood <- rbind(unclulucffood.co2, rbind(unclulucffood.CH4, unclulucffood.N2O))
    unclulucffood[, ipcc06 := "LULUCF"]
    unclulucffood[, processes := "FOLU"]
    
    unc.table_foodshare <- unique(rbind(uncfood.co2, 
                                        uncfood.ch4, 
                                        uncfood.n2o, 
                                        uncfood.f, 
                                        unclulucffood))
    unc.table_foodshare$xFlag <- F
    
    #Total GHG emissions
    print(unc.table_foodshare[, sum(emi, na.rm = TRUE), by=.(gas)])
    print(unc.table_foodshare[, sum(emi, na.rm = TRUE)])
    
    SOD.edgarfood <- 12307448
    SOD.foodtotal <- 18002877.0272335
    print(1-unc.table_foodshare[, sum(emi, na.rm = TRUE)]/SOD.foodtotal)
    save(uncfood.co2, uncfood.ch4, uncfood.n2o, 
         uncfood.f, unclulucffood, 
         unc.table_foodshare, file=file.foodshare)
  }
  return(file.foodshare)
}


f.loadtotal <- function(doload=TRUE){
  # Load data for non-food system emissions
  # From Efisio - email 20200810: CH4 needs to be divide by 25 and multiplied by 28
  #                               N2O needs to be divided by 298 and multiplied by 265
  
  
  file.totals <- paste0(edgar_folder, "unc.table_total.rdata")
  
  if(doload){
    
    load(paste0(edgar_uncfolder, "CO2_EDGAR_UncCorr_2015_unc.table.Rdata"))
    unc.co2 <- as.data.table(unc.table); rm(unc.table)
    unc.co2[, emio := emi] #GWP of IPCC-AR5 for CH4
    unc.co2$gas <- "CO2"
    
    ch4corr <- 25
    load(paste0(edgar_uncfolder, "CH4_EDGAR_UncCorr_2015_unc.table.Rdata"))
    unc.ch4 <- as.data.table(unc.table); rm(unc.table)
    unc.ch4$gas <- "CH4"
    unc.ch4[, emio := emi] #GWP of IPCC-AR5 for CH4
    unc.ch4[, emi := emi * gwpch4 / ch4corr] #GWP of IPCC-AR5 for CH4
    
    load(paste0(edgar_uncfolder, "N2O_EDGAR_UncCorr_2015_unc.table.Rdata"))
    n2ocorr <- 298
    unc.n2o <- as.data.table(unc.table); rm(unc.table)
    unc.n2o$gas <- "N2O"
    unc.n2o[, emio := emi] #GWP of IPCC-AR5 for N2O
    unc.n2o[, emi := emi * gwpn2o / n2ocorr] #GWP of IPCC-AR5 for N2O
    
    #F-gases
    fgases <- fread(paste0(edgar_uncfolder, "f_gases_EDGAR_total_AR5.csv"))
    fgases[, emi := gsub("NULL", "0", emi)]
    fgases[, emi := as.numeric(emi)]
    fgases <- fgases[, -c("name", "IPCC96", "Edgar process")]
    fgases[, `:=` (ipcc06 = 2, ipccX = 2, gas = "Fgases", emio=emi)]
    
    # fgasedgar <- edgarfood[Substance %in% c("GWP_100_SF6", 
    #                                      unique(edgarfood$Substance)[grepl("HFC", unique(edgarfood$Substance))])]
    # fgas <- fgasedgar[variable==2015, 
    #                   .(processes = EDGAR_SECTOR, ipcc06=substr(ipcc, 1, 1), ipccX = substr(ipcc, 1, 1), 
    #                     country = Country_code_A3, iFlag=FALSE, emi=value, 
    #                     unc.emi.min = value * 0.7, unc.emi.max = value * 1.7, 
    #                     unc.min = 0.7, unc.max = 0.7, gas="Fgas", Substance)]
    
    # Load total FAO
    load(paste0(edgar_folder, "../202005/", "edgar_food_20200617.rdata"))
    lulucf <- edgarfood[variable==2015 & part=="FAO_total", 
                        .(processes = EDGAR_SECTOR, ipcc06="LULUCF", ipccX = "LULCF", 
                          country = Country_code_A3, iFlag=FALSE, emi=value, 
                          unc.emi.min = value * 0.5, unc.emi.max = value * 1.5, 
                          unc.min = 0.5, unc.max = 0.5, gas=Substance, emio=value)]
    
    unc.total <- unique(rbind(unc.co2, unc.ch4, unc.n2o, fgases, lulucf))
    unc.total$xFlag <- F
    
    #Total GHG emissions
    print(unc.total[, sum(emi, na.rm = TRUE), by=.(gas)])
    print(unc.total[, sum(emi, na.rm = TRUE)])
    
    SOD.total <- 53698292.3360208
    print(1-unc.total[, sum(emi, na.rm = TRUE)]/SOD.total)
    
    save(unc.co2, unc.ch4, unc.n2o, 
         fgases, lulucf, 
         unc.total, file=file.totals)
  }
  
  return(file.totals)
  
}

load(f.loadfood(doload = do.importfood)) #FALSE to reload rdata file, TRUE to load from EDGAR exports
load(f.loadtotal(doload = do.importtotal))

unc.total <- unc.total[! is.na(emi), .(processes, gas, ipcc06, ipccX, country, unc.min, unc.max, iFlag, xFlag)]
unc.foodshare <- unc.table_foodshare[! is.na(emi), .(processes, gas, ipcc06, ipccX, country, unc.min, unc.max, iFlag, xFlag)]
unc.total$unc <- "TOTAL"
unc.foodshare$unc <- "FOOD"
unc <- rbind(unc.total, unc.foodshare)
unc <- unique(unc[! is.na(processes)])
unc <- dcast.data.table(unc, processes+gas+ipcc06+ipccX+country+iFlag+xFlag~unc, value.var = c("unc.min", "unc.max"))

# Check that not all relative uncertainties are identical
#unc[, rat_min := unc.min_TOTAL/unc.min_FOOD][, rat_max := unc.max_TOTAL/unc.max_FOOD]

toload <- paste0(edgar_folder, scan(paste0(edgar_folder, "/edgar_food_last.txt"), what = "character"))
load(toload)



