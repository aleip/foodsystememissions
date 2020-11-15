getgwp <- function(ar=6, gas=NULL){
  
  gases<-c("CH4bio", "CH4fos","CO2","N2O","Aggregate GHGs","NMVOC","PFCs","HFCs", "Unspecified mix of HFCs and PFCs")
  
  # Values from Third Assessment Report 2001 http://www.grida.no/publications/other/ipcc_tar/?src=/climate/ipcc_tar/wg1/212.htm
  gwpstar <- c(23, 23,1,296,1,0,1,1,1)
  # Values from Forth Assessment Report 2007 https://www.ipcc.ch/publications_and_data/ar4/wg1/en/ch2s2-10-2.html
  gwpsar4 <- c(25, 25,1,298,1,0,1,1,1)
  # Values from Fifth Assessment Report 2013 (page 714) https://ipcc.ch/pdf/assessment-report/ar5/wg1/WG1AR5_Chapter08_FINAL.pdf
  gwpsar5 <- c(28, 28, 1, 265, 1, 0, 1, 1, 1)

  # Values from Sixth Assessment Report 2021 (see GHG_Metrics_Guidelines_Final_06Aug.pdf)
  # Specificy: Biogenic CH4: 32, 
  #            Fossil CH4: 34.75
  gwpsar6 <- c(32, 34.75, 1, 261, 1, 0, 1, 1, 1)
  
  gwps <- data.table(
    gases = gases,
    ar3 = gwpstar, 
    ar4 = gwpsar4,
    ar5 = gwpsar5,
    ar6 = gwpsar6
  )
  
  
  curgwps <- gwps[, c("gases", paste0("ar", ar)), with=FALSE]
  
  if(! is.null(gas)){
    
    # Use 'CH4bio' for 'CH4'
    gas <- data.table(gases=gas)
    gas[gases == "CH4", gases := "CH4bio"]
    
    curgwp <- merge(gas, curgwps[gases %in% gas[[1]]], by="gases", sort=FALSE)[[2]]
    
  }else{
    curgwp <- curgwps
  }
  
  return(curgwp)
  
}
