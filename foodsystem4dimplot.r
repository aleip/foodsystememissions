require(data.table)
require(openxlsx)
require(ggplot2)
require(dplyr)

myuser <- Sys.info()["user"]
google <- paste0("C:/Users/", myuser, "/google/")
edgar_folder <- paste0(google, "projects/edgar/")
plot_folder <- paste0(edgar_folder, "plots/")

cost_folder <- paste0(google, "literature/mendeley/Springmann_data/")
cost_file <- "report_cost_1027.csv"
pop_file <- "EAT_Lancet_release_soioecon2.csv"
cost <- fread(paste0(cost_folder, cost_file))
pop <- fread(paste0(cost_folder, pop_file), )
pop <- pop[Item=="POP" & Year==2010 & SSP=="SSP2", .(Region, pop=Value)]
cost2use <- cost[Year==2010 & `Waste scenario`=="full_w" & Metric=="abs" & 
                   `Diet scenarios`=="BMK" & `Socio-econ scenario`=="SSP2_BASE" &
                   `Kcal scenario`=="BMK", #Alernative: 2100 kcal
                 .(Region,cost=Value)]
cost2use <- merge(cost2use, pop, by="Region", all.x=TRUE)
# cost is in billion per year
# pop is in million
# costPcap in $per cap and day
cost2use <- cost2use[, costPcap := 1000*cost/pop/65]

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

load(paste0(edgar_folder, "data/202005/edgar_food_20200617.rdata"))

edgarex <- dcast.data.table(edgarfood[variable==2015 & compartment != "", 
                                      .(value = sum(value, na.rm = TRUE)), 
                                      by = .(countries = Country_code_A3, compartment)], 
                                      countries ~ compartment, value.var = "value", fill = 0)
edgarex <- merge(edgarex, countrytable[, .(countries = Country_code_A3, region=C_group_IM24_sh, dev=dev_country)])

edgarcost <- merge(edgarex, cost2use, by.x = "countries", by.y = "Region", all = TRUE)


impactregions <- fread(paste0(google, "literature/mendeley/Springmann_data/impactregions.csv"))
edgarregions <- fread(paste0(edgar_folder, "regions/ipcc_ar6_edgar_data_gwp100_classifications_regions.csv"))

misscountries <- edgarcost[is.na(Landbased) | is.na(costPcap), .(countries, Energy, Industry, Landbased, Waste, cost, costPcap)]
misscountries <- merge(misscountries, impactregions, by.x="countries", by.y="IMPACT", all.x=TRUE)
misscountries <- merge(misscountries, edgarregions[, .(ISO, edgar=name)], by.x="countries", by.y="ISO", all.x=TRUE)
misscountries[!is.na(regions)]
misscountries[!is.na(edgar)]
write.csv(misscountries, file=paste0(plot_folder, "foodsystem4dimplot_missingcountries.csv"))

okcountries <- edgarcost[! (is.na(Landbased) | is.na(costPcap)), .(countries, Energy, Industry, Landbased, Waste, cost, costPcap)]
okcountries <- merge(okcountries, edgarregions[, .(ISO, edgar=name)], by.x="countries", by.y="ISO", all.x=TRUE)

write.csv(okcountries[, .(countries, edgar)], paste0(plot_folder, "foodsystem4dimplot_edgarcountries4plot.csv"))
edgarcost <- edgarcost[!is.na(Landbased) & !is.na(costPcap)]



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

gdb2edgar <- fread(file=paste0(edgar_folder, "regions/mapcountriesGBD_EDGAR.csv"))
gdb4plot <- merge(gdb4plot, gdb2edgar[, .(`Location ID`, ISOcode)], by.x="location", by.y="Location ID", all.x=TRUE)
gdb4plot <- gdb4plot[! is.na(ISOcode)]
gdb4plot <- gdb4plot[ISOcode != "", -"location", with=FALSE]

edgarcostgdb <- merge(edgarcost, gdb4plot[, .(countries=ISOcode, val_all, val_BMI, val_child, val_dietary)], by="countries", all.x = TRUE)

edgarcostgdb_abs <- edgarcostgdb[, .(countries, region, dev, 
                                     tot=Energy+Industry+Landbased+Waste,
                                     land=Landbased,
                                     energy=Energy+Industry,
                                     pop, cost=pop*costPcap,
                                     val_all_abs=pop*val_all,
                                     val_BMI_abs=pop*val_BMI,
                                     val_child_abs=pop*val_child,
                                     val_dietary_abs=pop*val_dietary)]
cols2sum <- c("tot", "land", "energy", "pop", "cost", "val_all_abs", "val_BMI_abs", "val_child_abs", "val_dietary_abs")
e1 <- edgarcostgdb_abs[, lapply(.SD, sum, na.rm=TRUE), by=.(countries), .SDcols=cols2sum]
e2 <- edgarcostgdb_abs[, lapply(.SD, sum, na.rm=TRUE), by=.(region), .SDcols=cols2sum]
e3 <- edgarcostgdb_abs[, lapply(.SD, sum, na.rm=TRUE), by=.(dev), .SDcols=cols2sum]
e1$scale <- "country"; setnames(e1, "countries", "reg")
e2$scale <- "region"; setnames(e2, "region", "reg")
e3$scale <- "development"; setnames(e3, "dev", "reg")

e4plot <- rbind(e1, e2, e3)

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
  foodtCO2cap = tot/pop,
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
e4plot[, .(emismn = min(foodtCO2cap),
           emis05 = quantile(foodtCO2cap, 0.05),
           emis50 = quantile(foodtCO2cap, 0.50),
           emis95 = quantile(foodtCO2cap, 0.95),
           emismx = max(foodtCO2cap),
           sharmn = min(land2landenergy),
           shar05 = quantile(land2landenergy, 0.05),
           shar50 = quantile(land2landenergy, 0.50),
           shar95 = quantile(land2landenergy, 0.95),
           sharmx = max(land2landenergy),
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
                       .(reg, 
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
  pxscale <- 7
  
  #p <- ggplot(x, aes(costPcap, land2landenergy)) + geom_point(color = x$val_all, size  = 5*x$sqrtktPcap)
  p <- ggplot(px, aes(x, y)) 
  #p <- p + geom_point(shape=21, size  = px$ALL, fill="grey", colour="black", stroke=0.2)
  p <- p + geom_point(shape=21, size  = pxscale*px$BMI, fill="blue", stroke=0)
  p <- p + geom_point(shape=21, size  = pxscale*px$Diet, fill="yellow", stroke=0)
  p <- p + geom_point(shape=21, size  = pxscale*px$Child, fill="red", stroke=0)
  p <- p + ggtitle("Food system GHG intensity and diet-related deaths") + 
    xlab("Whole sale cost for food [$ cap-1 day-1]. Source: Springmann et al.") +
    ylab("Share of GHG emissions from energy")
  
  # p <- p + geom_text(label=px$reg, 
  #                  nudge_x = 0.0, 
  #                  nudge_y=px$BMI/80, 
  #                  check_overlap = FALSE, # Seems to put it very far away 
  #                  size = 3)
  p <- p + geom_text(aes(label=px$reg, vjust=px$BMI*1.5), size=3)
  #p <- p + geom_scatterpie(aes(x=x, y=y, r=size), data=x, cols=c("BMI", "Dietary", "Child"), color=NA)
  print(p) 
  
  if(doprint) {
    png(filename = paste0(plot_folder, "ghgintensityplot_", sel, today(), ".png"), width=10000, height=5000, units="px", res=1000)
    print(p)
    dev.off()
  }  
  
  return(p)
}

pcounrty <- do.intensityplot(e4plot[scale=="country"], doprint = TRUE)
pdev <- do.intensityplot(e4plot[scale=="development"], doprint = TRUE)
preg <- do.intensityplot(e4plot[scale=="region"], doprint = FALSE)

preg + geom_text(label=e4plot[scale=="region"][1:5]$reg, 
                 nudge_x = 0.0, 
                 nudge_y=e4plot[scale=="region"][1:5]$sqrtktPcap + 0.01, 
                 check_overlap = TRUE, 
                 size = 3)


e4plot[scale=="region"][order(costPcap)][, .(reg, tot, costPcap, sqrtktPcap, enind2landenind)]


save(gdb, gdb4plot, edgar, cost2use, edgarcost, file=paste0(plot_folder, "ghgintensityplot", today(), ".rdata"))


