## See version #1

industry <- edgarfood[grepl("^2", ipcc) & variable==2015 & ! is.na(stage), sum(ar6),
                      by=.(ipcc, gas, stage, stagedet, compartment)]

industry <- industry[!grepl("^2B[3-5]", ipcc), subcat := substr(ipcc, 1, 2)]
industry <- industry[grepl("^2B[3-5]", ipcc), subcat := substr(ipcc, 1, 3)]
industry <- industry[, .(value=sum(V1)), by=.(subcat)]

ammonia_nitricacid <- industry[subcat=="2B1", value]
halocarbons_sulphurhexafluoride <- industry[subcat=="2B5", value]

write.xlsx(ammonia_nitricacid, file="industry_emissions.xlsx")
