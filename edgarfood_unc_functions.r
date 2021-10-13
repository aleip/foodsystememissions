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
  uncorr,  # Logical: Flag on correlation across countries
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
    # #   if(b.cat=='Margatita' & now_run =='CO2') {uncorr[i] <- F}
    # #     uncorr[i] <- F # sensitivity case to see the effect of having implemented the uncorr flag option
    if (!xFlag[i]){ #exclude all biofuels
      if(uncorr[i]==T ){ #uncorrelated
        
        ## SUM squares thus (i) square unc so far, (ii) add new unc, (iii) square root
        unc.cat <- sqrt( unc.cat^2 + (emi[i]*unc[i])^2 )
        
      } else if (uncorr[i]==F){ #correlated
        
        ## SUM standard deviations as for formula above
        unc.cat <- unc.cat + emi[i]*unc[i]
      }
      
      # if(emi[i] !=0 & is.na(unc[i])) {cat(' check i= ', i, '\n'); unc[i] <- 0}# {unc[i] <- 0} # mmhhh
      # if (!xFlag[i]){ #exclude all biofuels
      #     unc.cat <- unc.cat + sqrt((emi[i]*unc[i])^2)
      #   } if (uncorr[i]==F){ #correlated
      #     for(j in 1:length(unc)){
      #       # All correlations in this section are 'mutual' thus no matrix needed
      #       # Sum-up only for those which are flagged correlated
      #       if(is.na(emi[j] | emi[j]==0. | i==j | uncorr[j]==T)){next()}
      #       unc.cat <- unc.cat + (emi[i]*unc[i] + emi[j]*unc[j])
      #     }
      #   }
      
    } else { # do the same for biofuels
      if(uncorr[i]==TRUE ){ #uncorrelated
        unc.cat.x <- sqrt((emi[i]*unc[i])^2 + unc.cat.x^2)
        
      } else if (uncorr[i]==FALSE){ #correalted
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


emissionSharesUncertainty <- function(dtorig, emi_old=NULL, isShare=TRUE, remTerms = FALSE, 
                                      id1="food", country1="ALL", stage1="TOTAL", gas1="GHG", sector1="TOTAL",
                                      id2="food", country2="ALL", stage2="TOTAL", gas2="GHG", sector2="TOTAL"){
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
  # 
  # f = share = A/(A+B)
  # 
  # sf = SQRT( (df/dA)^2 sA^2 + (df/dB)^2 * sB^2)
  # 
  # See https://www.wolframalpha.com/widgets/gallery/view.jsp?id=ecfd3a449d1c38d762d0ae078b7c7208
  #     for derivatives df/dx
  # df/dA = B/(A+B)^2
  # df/dB = - A/(A+B)^2
  # sf = 1/(A+B)^2 * SQRT(A^2 sB^2 + B^2 sA^2)
  # sf = Term1 * SQRT(Term2 + Term3)
  
  dt <- copy(dtorig)
  
  # Clean column names
  dt <- dt[, -c("emi.unc.lo", "emi.unc.up"), with=FALSE]
  emi1 <- dt[.id==id1 & country==country1 & stage==stage1 & gas==gas1 & sector==sector1]
  emi2 <- dt[.id==id2 & country==country2 & stage==stage2 & gas==gas2 & sector==sector2]
  
  # Only one of both exist --> add empty line
  #                        --> calculations only if not
  #cat(nrow(emi1) * nrow(emi2))
  if(nrow(emi1) * nrow(emi2) > 0){
    
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
    # max(0, ...) because the uncertainty of the total could be reduced
    emi[, maxB := sqrt(max(0, (maxAB*emiAB)^2 - (maxA*emiA)^2))/(emiAB-emiA)]
    emi[, minB := sqrt(max(0, (minAB*emiAB)^2 - (minA*emiA)^2))/(emiAB-emiA)]
    
    if(isShare){
      # A is a part of AB --> more complex calculation as emissions are correlated
      emi[, Term1 := 1/(emiAB)^2]
      emi[, `:=` (
        Term2max = emiA * ((emiAB-emiA) * maxB)*Term1,
        Term2min = emiA * ((emiAB-emiA) * minB)*Term1,
        Term3max = (emiAB-emiA) * (emiA * maxA)*Term1,
        Term3min = (emiAB-emiA) * (emiA * minA)*Term1
      )]
      emi[, `:=` (
        min = sqrt(Term2min^2 + Term3min^2),
        max = sqrt(Term2max^2 + Term3max^2)
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
    # Additional restrictions under the assumption that emiA dominates the uncertainty:
    #   --> in this case the upper limit is calculated from the assumption that 
    #       emiA goes to max, but emiB remains constant
    emi[share.unc.up > 1, 
        share.unc.up := min(1, emiA * (1 + maxA)/
          (emiAB + emiA * maxA - (emiAB-emiA) * maxB))]
    if(! is.null(emi_old)){
      emi <- rbind(emi_old, emi, fill = TRUE)
    }
    if(remTerms) emi <- emi[, -c("maxB", "minB", "Term1", "Term2min", "Term2max", "Term3min", "Term2max")]
    #print(emi)
    return(emi) 
  }else{
    if(! is.null(emi_old)){
      return(emi_old)
    }
    
  }
}

#
convertAR5_to_AR6 <- function(){
  save(unc.table, file="C:/Users/adrian/ownCloud/EDGAR-FOOD/202108/unc/unc.table_AR5.rdata")

  # Load IPCC GWPs - EDGAR provided the data using AR5-GWP
  # They need to be converted for AR6
  ipccgwps <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "100_yr_gwps"))
  ipccgwpch4 <- data.table(read.xlsx(paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "CH4_gwps"))
  ipccgwpch4 <- unique(ipccgwpch4[, .(ipcc=sector_code, gwp_ar6)])
  
  #For some categories there are 2 GWPs
  ipccch4 <- unique(ipccgwpch4[, .(ipcc, gwp_ar6)])
  ipccch4 <- ipccch4[, .(gwp_ar6 = mean(gwp_ar6)), by=.(ipcc)]
  #assign manually the GWP values from the IPCC table & load mapped table
  #write.xlsx(unique(uncch4[, .(ipcc06)]), file=paste0(edgar_folder, "ipcc06_gwpar6.xlsx")
  ipccch4 <- as.data.table(read.xlsx(xlsxFile = paste0(edgar_folder, "ipcc06_gwpar6.xlsx"), sheet = "Sheet 1"))
  #ipccch4 <- as.data.table(read.xlsx(xlsxFile = paste0(edgar_folder, "unc/IPCC96_2_IPCC06.xlsx"), sheet = "Sheet1"))
  
  uncgas <- merge(unc.table[gas!="CH4"],ipccgwps, by="gas", all.x = TRUE)
  uncch4 <- merge(unc.table[gas=="CH4"], ipccch4[, .(ipcc06, gwp_ar6)], by = "ipcc06")
  unc <- rbind(uncgas, uncch4)
  
  
  #gwpsar5 <- c(28, 1, 265, 1, 0, 1, 1, 1)
  unc[, gwp_ar5 := ifelse(gas=="CH4", 28, ifelse(gas=="N2O", 265, 1))]
  unc[!is.na(gwp_ar6), `:=` (emi = emi / gwp_ar5 * gwp_ar6)]
  
  unc.table <- unc
  save(unc.table, file="C:/Users/adrian/ownCloud/EDGAR-FOOD/202108/unc/unc.table_AR6.rdata")
  return(unc.table)
  
  
}
