require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

myuser <- Sys.info()["user"]
google <- paste0("C:/Users/", myuser, "/google/")
edgar_folder <- paste0(google, "../OwnCloud/EDGAR-FOOD/")
edgar_version <- "/202108/"
plot_folder <- paste0(edgar_folder, edgar_version, "plots/")
ipcc_folder <- paste0(google, "/projects/ipccwg3/xcutting/dms05report_data_energy_emissions/")
ipcc_file <- paste0(ipcc_folder, "GHG emissions data (100 yr GWPs).xlsx")

cost_folder <- paste0(google, "literature/mendeley/Springmann_data/")
# File sent for draft manuscript as submitted to GFS, used for FOD
# based on IMPACT estimates
cost_file_1027 <- "report_cost_1027.csv"

source("../capriextract/f_tools.r")


# File sent for manuscript as submitted to Lancet Planetary Health, Nov 2020
# Based on ICP data
# "ICP data are actual measurements of food prices done by the World Bank. The new baseline is 2017."
# Sheet "version"
# - File: C:\GAMSdir\HEALTH\Global Health Module\diet_costs\ICP_full_cost_2017_r.gdx
# - Date: 11/11/2020 19:16:06
# - Parameter report_cost total costs (billion per year)
# - Index order = 1 2 3 4 5 6 7
# - No filter active

#cost_file_1111 <- "report_cost_1111.xlsx"
#Update 20200905
cost_file_1111 <- "diet_cost_data_1111_upd.xlsx"

#cost1027 <- fread(paste0(cost_folder, cost_file_1027))
cost1111 <- data.table(read.xlsx(xlsxFile = paste0(cost_folder, cost_file_1111), sheet = "diet_costs"))
#cost2use1027 <- cost1027[Year==2010 & `Waste scenario`=="full_w" & Metric=="abs" & 
#                   `Diet scenarios`=="BMK" & `Socio-econ scenario`=="SSP2_BASE" &
#                   `Kcal scenario`=="BMK", #Alernative: 2100 kcal
#                 .(Region,cost=Value)]
cost2use1111 <- cost1111[Year==2017 & `Waste.scenario`=="full_w" & Metric=="abs" & 
                   `Diet.scenario`=="BMK" & `Socio-economic.scenario`=="SSP2_BASE" &
                   `Cost.item`=="market", #Alernative: 2100 kcal
                 .(Region,cost=Value)]


# Population data from the EAT-Lancet report
#pop_file <- "EAT_Lancet_release_soioecon2.csv"
#pop <- fread(paste0(cost_folder, pop_file), )
#pop <- pop[Item=="POP" & Year==2010 & SSP=="SSP2", .(Region, pop=Value)]

# Load IPCC Population data
# They need to be converted for AR6
#ipccpop <- data.table(read.xlsx(ipcc_file, sheet = "supplementary_data"))
#ipccpop <- ipccpop[year==2017, .(Region=ISO, pop=POP)]



# For consistency need to use Population data provided by Springmann
pop <- data.table(read.xlsx(xlsxFile = paste0(cost_folder, cost_file_1111), sheet = "population"))
pop <- pop[Year==2017 & `Socio-economic.scenario`=="SSP2_BASE", .(Region, pop=Value)]


# Merge with Springmann pop
#cost2use <- merge(cost2use1111, ipccpop, by="Region", all.x=TRUE)
cost2use <- merge(cost2use1111, pop, by="Region", all.x=TRUE)


# cost is in billion per year
# pop is in million
# ipccpop is in cap
# costPcap in $per cap and day
# cost is calculated as absolute $/(day*cap) * 365 days/year * cap = M$/year  -- / 1000 --> G$/yr 
# wrong?? cost2use <- cost2use[, .(Region, costPcap=cost, pop, cost= (cost*365/100)/(pop/1000000))]
cost2use <- cost2use[, .(Region, costPcap=cost, pop, cost= (cost*365*pop/1000))]

## Cleaning of countries/changing of codes from older version (2019 - for FOD)
## --> kept here as doen't do harm
# Remove country groups
cost2use <- cost2use[! Region %in% c("BLT", #Baltic --> need to calculate average of Estonia, Lithuania, ...
                                     "GRL", #Greenland no data
                                     "CRB", "OAO", "OBN", "OIO", "OPO", "OSA", # Various 'others'
                                     "RA" #Rest of Arab Peninsula
)]
#Adapt to EDGAR country codes
cost2use <- cost2use[Region=="BLX", Region:="BEL"] #Belgium and Luxembourg --> BEL
cost2use <- cost2use[Region=="CHM", Region:="CHN"] #China
cost2use <- cost2use[Region=="CHP", Region:="CHE"] #Switzerland
cost2use <- cost2use[Region=="FNP", Region:="FIN"] #Finland
cost2use <- cost2use[Region=="FRP", Region:="FRA"] #France
cost2use <- cost2use[Region=="GSA", Region:="GUY"] #Guyana
cost2use <- cost2use[Region=="ITP", Region:="ITA"] #Italy
cost2use <- cost2use[Region=="MOR", Region:="MAR"] #Morocco
cost2use <- cost2use[Region=="SPP", Region:="ESP"] #Spain
cost2use <- cost2use[Region=="UKP", Region:="GBR"] #United Kingdom

load(paste0(edgar_folder, edgar_version, "/", 
            scan(paste0(edgar_folder, edgar_version, "/edgar_food_last.txt"), what = "character")))

# Load IPCC Population data
# They need to be converted for AR6
ipccreg <- data.table(read.xlsx(ipcc_file, sheet = "region_classification"))
ipccreg <- data.table(read.xlsx(paste0(edgar_folder, edgar_version, "ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx"), sheet = "region_classification"))
ipccreg <- ipccreg[, .(countries=ISO, dev=region_ar6_dev, reg5=region_ar6_6_short, reg5long = region_ar6_6, reg10=region_ar6_10, reg22=region_ar6_22)]

edgarex <- edgarfood[Year==2015 & part %in% c("EDGAR_FOOD", "FAO_FOOD"),
                     .(value = sum(ktCO2eq, na.rm = TRUE)), by = .(countries = Country_code_A3, compartment)]
edgarex <- dcast.data.table(edgarex, countries ~ compartment, value.var = "value", fill = 0)
#edgarex <- merge(edgarex, countrytable[, .(countries = Country_code_A3, region=C_group_IM24_sh, dev=dev_country)])
# EDGAR has combined Serbia and Montenegro (SCG)
# --> use Serbia as proxy
# Update TSU 20201201 - Keep Serbia only and delete 'Serbia and Montenegro' ==> consistent with this approach
ipccreg <- ipccreg[countries!="SRB"]
cost2use[Region=="SRB", Region := "SCG"]


write.xlsx(ipccreg, file = gsub(".xlsx", "_ipccreg.xlsx", ipcc_file))
edgarex <- merge(edgarex, ipccreg, by="countries")
edgarex <- edgarex[! is.na(dev)]

edgarcost <- merge(edgarex, cost2use, by.x = "countries", by.y = "Region", all.x = TRUE, all.y = TRUE)
edgarcost <- edgarcost[! is.na(dev)]
misscountries <- edgarcost[is.na(costPcap), .(countries, Energy, Industry, Landbased, Waste, cost, pop)]
misscountries <- merge(misscountries, ipccreg, by="countries")
write.xlsx(misscountries, file = paste0(plot_folder, "Costdata_missing_countries_", today(), ".xlsx"))

# LOAD Global Burden of Disease Data
# Downloaded from http://ghdx.healthdata.org/gbd-results-tool
# Selection:
# measure = 1
#           1 = Deaths
#           2 = DALYs (Disability-Adjusted Life Years)
#           3 = YLDs (Years Lived with Disability)
#           4 = YLLs (Years of Life Lost)
# cause = 294 = All causes
# sex = 3 = Both sexes
# age = 22 = All ages
# metric = 2
#        1	Number
#        2	Percent
#        3	Rate

# year = 1990, 2000, 2010, 2015
# cause = 
#     rei_id	      rei_name	        parent_id	rei_type	level	sort_order
#       169	All risk factors	              169	    Risk	    0	1
#        92	Child and maternal malnutrition	203	    Risk	    2	40
#       110	Dietary risks	                  203	    Risk	    2	60
#       108	High body-mass index	          104	    Risk	    2	85
#       
#       203	Behavioral risks                169	    Risk	    1	3
#       104	Metabolic risks	                169	    Risk	    1	4




gdb4plot <- fread(file=paste0(google, "projects/globalburdenofdisease/IHME-GBD_2017_DATA-565b5ddb-1.csv"))
gdb4plot <- dcast.data.table(gdb4plot[year==2015, .(location, rei, val)],
                             location ~ rei, value.var = 'val', fill=0)
gdb4plot <- gdb4plot[, .(location, val_all = `169`, val_BMI = `108`, val_child = `92`, val_dietary = `110`)]

#gdb2edgar <- fread(file=paste0(edgar_folder, "regions/mapcountriesGBD_EDGAR.csv"))
gdb2edgar <- fread(file=paste0(edgar_folder, edgar_version, "mapcountriesGBD_EDGAR.csv"))
gdb4plot <- merge(gdb4plot, gdb2edgar[, .(`Location ID`, ISOcode)], by.x="location", by.y="Location ID", all.x=TRUE)


gdb4plot <- gdb4plot[! is.na(ISOcode)]
gdb4plot <- gdb4plot[ISOcode != "", -"location", with=FALSE]

edgarcostgdb <- merge(edgarcost, gdb4plot[, .(countries=ISOcode, val_all, val_BMI, val_child, val_dietary)], by="countries", all.x = TRUE)
edgarcostgdb_abs <- edgarcostgdb[, .(countries, dev, reg5, reg10, reg22, 
                                     tot=Energy+Industry+Landbased+Waste,
                                     land=Landbased,
                                     energy=Energy+Industry,
                                     pop, cost=pop*costPcap,
                                     val_all_abs=pop*val_all,
                                     val_BMI_abs=pop*val_BMI,
                                     val_child_abs=pop*val_child,
                                     val_dietary_abs=pop*val_dietary)]
cols2sum <- c("tot", "land", "energy", "pop", "cost", "val_all_abs", "val_BMI_abs", "val_child_abs", "val_dietary_abs")
e1 <- edgarcostgdb_abs[!is.na(pop) & !is.na(val_all_abs), lapply(.SD, sum, na.rm=TRUE), by=.(countries), .SDcols=cols2sum]
e2 <- edgarcostgdb_abs[!is.na(pop) & !is.na(val_all_abs), lapply(.SD, sum, na.rm=TRUE), by=.(reg10), .SDcols=cols2sum]
e3 <- edgarcostgdb_abs[!is.na(pop) & !is.na(val_all_abs), lapply(.SD, sum, na.rm=TRUE), by=.(reg22), .SDcols=cols2sum]
e4 <- edgarcostgdb_abs[!is.na(pop) & !is.na(val_all_abs), lapply(.SD, sum, na.rm=TRUE), by=.(dev), .SDcols=cols2sum]

# If S+middle Africa overlays W Africa --> combine
edgarcostgdb_abs[, reg21 := reg22]
edgarcostgdb_abs[reg21 == "Southern and middle Africa", reg21 := "S+W+middle Africa"]
edgarcostgdb_abs[reg21 == "Western Africa", reg21 := "S+W+middle Africa"]
e5 <- edgarcostgdb_abs[!is.na(pop) & !is.na(val_all_abs), lapply(.SD, sum, na.rm=TRUE), by=.(reg21), .SDcols=cols2sum]

e1$scale <- "country"; setnames(e1, "countries", "reg")
e2$scale <- "reg10"; setnames(e2, "reg10", "reg")
e3$scale <- "reg22"; setnames(e3, "reg22", "reg")
e4$scale <- "development"; setnames(e4, "dev", "reg")
e5$scale <- "reg21"; setnames(e5, "reg21", "reg")

e4plot <- rbind(e1, e2, e3, e4, e5)

# Convert back to per capita data
e4plot <- e4plot[, .(
  scale, reg, tot, land, energy, pop,
  
  # Convert back to per capita data
  costPcap = cost/pop,
  val_all = val_all_abs/pop,
  val_BMI = val_BMI_abs/pop,
  val_child = val_child_abs/pop,
  val_dietary = val_dietary_abs/pop,
  
  # Calculate GHG metrics
  # tot in kt CO2eq, pop in cap ==> convert to tCO2eq/cap by dividing by 1000
  foodtCO2cap = 1000*tot/pop,
  enind2landenind = energy / (land+energy),
  
  # Find a suitable transformation for bubble size
  # based on foodtCO2cap
  sqrtktPcap = ((tot/pop) /1000)^0.5
  
)]

e4plot <- e4plot[, 
                 dominance := ifelse(val_child/(val_child+val_BMI+val_dietary)>0.62, 1, 
                                     ifelse(val_BMI/(val_child+val_BMI+val_dietary)>0.23,2, 3))]
hist(e4plot$dominance)
hist(e4plot$foodtCO2cap)
summary(edgarcostgdb)
e4plot[pop != 0, .(emismn = min(foodtCO2cap),
                   emis05 = quantile(foodtCO2cap, 0.05),
                   emis50 = quantile(foodtCO2cap, 0.50),
                   emis95 = quantile(foodtCO2cap, 0.95),
                   emismx = max(foodtCO2cap),
                   sharmn = min(enind2landenind),
                   shar05 = quantile(enind2landenind, 0.05),
                   shar50 = quantile(enind2landenind, 0.50),
                   shar95 = quantile(enind2landenind, 0.95),
                   sharmx = max(enind2landenind),
                   costmn = min(costPcap),
                   cost05 = quantile(costPcap, 0.05),
                   cost50 = quantile(costPcap, 0.50),
                   cost95 = quantile(costPcap, 0.95),
                   costmx = max(costPcap)
)]

do.intensityplot <- function(dt, doprint=FALSE){
  
  plotdata <- copy(dt)
  
  sel <- unique(plotdata$scale)
  
  plotdata <- plotdata[sqrtktPcap<100, 
                       .(reg, regbot, shifth,
                         x=costPcap, 
                         y=as.numeric(enind2landenind), 
                         BMI=sqrtktPcap, 
                         Diet=sqrtktPcap*(val_dietary+val_child)^2/(val_BMI+val_dietary+val_child)^2, 
                         Child=sqrtktPcap*val_child^2/(val_BMI+val_dietary+val_child)^2
                       )]
  px <- plotdata[x>5 & x<6]
  px <- plotdata
  px <- px[order(BMI, decreasing=TRUE)]
  px <- as.data.frame(px)
  pxscale <- 5000
  pxscale <- 250
  
  #p <- ggplot(x, aes(costPcap, land2landenergy)) + geom_point(color = x$val_all, size  = 5*x$sqrtktPcap)
  p <- ggplot(px, aes(x, y)) 
  #p <- p + geom_point(shape=21, size  = px$ALL, fill="grey", colour="black", stroke=0.2)
#  p <- p + geom_point(shape=21, size  = pxscale*px$BMI, fill="blue", stroke=0)
#  p <- p + geom_point(shape=21, size  = pxscale*px$Diet, fill="yellow", stroke=0)
#  p <- p + geom_point(shape=21, size  = pxscale*px$Child, fill="red", stroke=0)
  p <- p + geom_point(shape=21, size  = pxscale*px$BMI, fill="#5190CC", stroke=0)
  p <- p + geom_point(shape=21, size  = pxscale*px$Diet, fill="#F7A800", stroke=0)
  p <- p + geom_point(shape=21, size  = pxscale*px$Child, fill="#DE4911", stroke=0)
  p <- p + # ggtitle("Food system GHG intensity and diet-related deaths") + 
    xlab(bquote(paste("Whole sale cost for food ($", cap^{-1}, day^{-1}, ")"))) +
    ylab("Share of GHG emissions from energy")
  #p <- p + geom_text(aes(label=px$reg, vjust=px$BMI*1.5), size=3)
  
  p <- p + theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 panel.grid.major.y = element_line(size = 0.1, linetype = 'solid', colour = "black"),
                 panel.background = element_blank())
  
  p <- p + scale_y_continuous(breaks=seq(0, 1, 0.1))
  nfactor <- 0.005
  pn <- p + geom_text(label=px$reg, nudge_x = px$shifth, nudge_y=px$BMI + nfactor, check_overlap = FALSE, size = 3)
  pn <- pn + geom_text(label=px$regbot, nudge_x = px$shifth, nudge_y=-px$BMI - nfactor, check_overlap = FALSE, size = 3)
  print(pn) 
  
  if(doprint) {
    png(filename = paste0(plot_folder, "ghgintensityplot_", sel, today(), ".png"), width=10000, height=5000, units="px", res=1000)
    print(pn)
    dev.off()
  }  
  
  return(pn)
}

# Shorten region names before plotting
e4plotf <- copy(e4plot)
e4plotf[, reg := gsub("[Ss]outhern", "S", reg)]
e4plotf[, reg := gsub("[Nn]orthern", "N", reg)]
e4plotf[, reg := gsub("[Ee]astern", "E", reg)]
e4plotf[, reg := gsub("[Ww]estern", "W", reg)]
e4plotf[, reg := gsub(" and ", "+", reg)]
e4plotf$regbot <- ""
e4plotf[reg=="Eurasia", `:=` (reg="", regbot=reg)]
e4plotf[reg=="South-East Asia", `:=` (reg="", regbot=reg)]
e4plotf[reg=="Australia & New Zealand", `:=` (reg="", regbot=reg)]
e4plotf[reg=="W Africa", `:=` (reg="", regbot=reg)]
#e4plotf[reg=="South America", `:=` (reg="", regbot=reg)]
e4plotf[reg=="North Africa", `:=` (reg="", regbot=reg)]
#e4plotf[reg=="Developing Pacific", `:=` (reg="Developing", regbot="Pacific")]

e4plotf[, shifth := 0]
e4plotf[reg=="S+E Europe", shifth := -0.25]
e4plotf[reg=="Caribbean", shifth := -0.1]
e4plotf[reg=="Developing Pacific", shifth := -0.1]
e4plotf[reg=="N+W Europe", shifth := 0.1]


pcounrty <- do.intensityplot(e4plotf[scale=="country"], doprint = TRUE)
pdev <- do.intensityplot(e4plotf[scale=="development"], doprint = TRUE)
preg <- do.intensityplot(e4plotf[scale=="reg22"], doprint = TRUE)
preg <- do.intensityplot(e4plotf[scale=="reg21"], doprint = TRUE)
preg <- do.intensityplot(e4plotf[scale=="reg10"], doprint = TRUE)

#e4plot[scale=="region"][order(costPcap)][, .(reg, tot, costPcap, sqrtktPcap, enind2landenind)]


#save(gdb, gdb4plot, edgar, cost2use, edgarcost, edgarcostgdb, e4plot, file=paste0(plot_folder, "ghgintensityplot", today(), ".rdata"))
save(gdb4plot, edgar, cost2use, edgarcost, edgarcostgdb, e4plot, e4plotf, file=paste0(plot_folder, "ghgintensityplot", today(), ".rdata"))
wb <- createWorkbook()
ws <- addWorksheet(wb, sheetName = "edgarcostgdb")
ws <- writeData(wb, sheet = "edgarcostgdb", edgarcostgdb)
ws <- addWorksheet(wb, sheetName = "countries")
ws <- writeData(wb, sheet="countries", e4plot[scale=="country"])
ws <- addWorksheet(wb, sheetName = "agg_reg10")
ws <- writeData(wb, sheet="agg_reg10", e4plot[scale=="reg10"])
ws <- addWorksheet(wb, sheetName = "agg_reg22")
ws <- writeData(wb, sheet="agg_reg22", e4plot[scale=="reg22"])
ws <- addWorksheet(wb, sheetName = "agg_dev")
ws <- writeData(wb, sheet="agg_dev", e4plot[scale=="dev"])
flx <- paste0(edgar_folder, edgar_version, "edgar_cost_gdb.xlsx")
saveWorkbook(wb, file=flx, overwrite = TRUE)


