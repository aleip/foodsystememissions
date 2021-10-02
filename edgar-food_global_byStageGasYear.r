require(data.table)
require(openxlsx)
load("C:/Users/adrian/ownCloud/EDGAR-FOOD/202011/edgar_food_20210323.rdata")

wb <- openxlsx::createWorkbook(creator="Adrian Leip", title = "EDGAR-FOOD", subject = "GHG emissions by stage and year")

stagegas <- edgarfood[, sum(ar5), by=.(gas, stagedet, variable)]
stagegas <- dcast.data.table(stagegas, stagedet + gas ~ variable, value.var = "V1")

ws <- addWorksheet(wb, sheetName="AR5")
writeDataTable(wb, x = stagegas, sheet = "AR5")
stagegas <- edgarfood[, sum(ar6), by=.(gas, stagedet, variable)]
stagegas <- dcast.data.table(stagegas, stagedet + gas ~ variable, value.var = "V1")
ws <- addWorksheet(wb, sheetName="AR6")
writeDataTable(wb, x = stagegas, sheet = "AR6")
saveWorkbook(wb, file="edgar-food_global_byStageGasYear.xlsx", overwrite = TRUE)
