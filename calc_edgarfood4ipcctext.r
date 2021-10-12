require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

#rm(list=ls())

if(Sys.info()[4]=="D01RI1600881"){ myuser <- "leipadr" }
myuser <- Sys.info()[6]
google <- paste0("C:/Users/", myuser, "/google/")
jrcbox <- paste0("C:/Users/", myuser, "/ownCloud/")
manuscripts <- paste0(google, "../literature/manuscripts")
edgar_folder <- paste0(google, "edgar")
edgar_folder2020 <- paste0(jrcbox, "EDGAR-FOOD/202011/")
edgar_folder <- paste0(jrcbox, "EDGAR-FOOD/202108/")
edgar_input <- paste0(edgar_folder, "input_edgar/")
fao_folder <- paste0(google, "/projects/faostat_landuse/")
fao_folder <- edgar_folder
ipcc_folder <- paste0(google, "/projects/ipccwg3/xcutting/dms05report_data_energy_emissions/")

last <- paste0(edgar_folder, scan(paste0(edgar_folder, "/edgar_food_last.txt"), what = "character"))
if(! exists("edgarfood")) {load(last)}
load(gsub("edgar_food", "EDGAR-FOOD_by_globalshares_ar6", last))
if(! exists("ipcc")) {ipcc <- as.data.table(read.xlsx(xlsxFile = paste0(edgar_folder, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "data"))}

### TECHNICAL SUMMARY
## Food systems currently contributed some 25-41% to global greenhouse gas (GHG) emissions in 2018
## ==> Calculated by Efisio, see email 20211007
## 
## While the share of global GHG emissions from the food systems has decreased since 1990, the absolute emissions increased from 14 to 16 TgCO2eq per year during the period 1990 to 2018
global[year %in% c(1990, 2018), .(year, Foodtotal=Foodtotal/1000000, shareIncLULUCF)]

## In industrialized countries, emissions decreased from 5.5 to 4.5 Tg CO2eq, 
## while the share of food system emissions increased slightly from 23 to 24 %.
ind[year %in% c(1990, 2018), .(year, Foodtotal=Foodtotal/1000000, shareIncLULUCF)]


## In developing countries, emissions increased from 10 to 12 TgCO2eq 
## with share on total emissions decreasing from 65 to 35 %.  
dev[year %in% c(1990, 2018), .(year, Foodtotal=Foodtotal/1000000, shareIncLULUCF)]

## The global average per capita emissions from food systems decreased from 2.7 to 2.2 t CO2eq per annum. 
popglob <- ipcc[, .(population=mean(population, na.rm = TRUE)), by=.(ISO, year)][, .(population=sum(population, na.rm = TRUE)), by=.(year)]
merge(global[year %in% c(1990, 2018), .(year, Foodtotal=Foodtotal)], popglob[year %in% c(1990, 2018)], by="year")[, tCO2eq_per_cap := (Foodtotal*1000) / population][]
 

## CO2 emissions remained stable at about 7.5 Tg CO2eq per year 
## with decreasing emissions from land use change, 
## but strongly increasing emissions from energy use, and increasing emissions from CH4 and N2O. 
dcast.data.table(edgarfood[Year %in% c(1990, 2018) & part %in% c("EDGAR_FOOD", "FAO_FOOD"), .(ktCO2eq=sum(ktCO2eq)), by=.(gas, Year)], Year ~ gas, value.var = "ktCO2eq")
dcast.data.table(edgarfood[Year %in% c(1990, 2018) & part %in% c("EDGAR_FOOD", "FAO_FOOD"), .(ktCO2eq=sum(ktCO2eq)), by=.(compartment, Year)], Year ~ compartment, value.var = "ktCO2eq")



stop()
industry <- edgarfood[grepl("^2", ipcc) & variable==2015 & ! is.na(stage), sum(ar6),
                      by=.(ipcc, gas, stage, stagedet, compartment)]

industry <- industry[!grepl("^2B[3-5]", ipcc), subcat := substr(ipcc, 1, 2)]
industry <- industry[grepl("^2B[3-5]", ipcc), subcat := substr(ipcc, 1, 3)]
industry <- industry[, .(value=sum(V1)), by=.(subcat)]

ammonia_nitricacid <- industry[subcat=="2B1", value]
halocarbons_sulphurhexafluoride <- industry[subcat=="2B5", value]

write.xlsx(ammonia_nitricacid, file="industry_emissions.xlsx")
