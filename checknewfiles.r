require(data.table)
require(openxlsx)
require(ggplot2)

rm(list=objects())
mygas <- 'CH4'

load(paste0("C:/Users/adrian/google/projects/edgar/data/202006_unc/", mygas, "_UncCorr_2015_unc.table.Rdata"))
if(mygas == "CH4") {
  ch4 <- unc.summary
  rm(unc.summary)
}else{
  ch4 <- unc.table
  rm(unc.table)
}
ch4 <- ch4[grepl("TNR", ch4$processes),]
load(paste0("C:/Users/adrian/google/projects/edgar/data/202006_unc/", mygas, "_UncCorr_2015_unc.table_food_share.Rdata"))
ch42 <- unc.table
ch42 <- ch42[grepl("TNR", ch42$processes),]
rm(unc.table)
View(ch4)
View(ch42)
ch43 <- unique(rbind(ch4, ch42))
View(ch43)
