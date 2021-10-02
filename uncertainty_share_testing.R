# 24 August 2020
# E. Solazzo, JRC

# for the food-system emission paper:
# how does the uncertinty of the share (e.g. food vs total) change with respect to correlation?

library(MASS)
library(data.table)

set.seed(123)
nn        <- 200
mean.tot  <- 54
mean.food <- 18
mean.nfood <- 54-18
sigma.t <- 5 #(half the 95% CI)
sigma.f <- 3
tot  <- rnorm(nn, mean=mean.tot,  sd=5)
food <- rnorm(nn, mean=mean.food, sd=7)

#sigma.t <- sqrt(var(tot))
#sigma.f <- sqrt(var(food)
sigma1 <- sigma.t; sigma2 <- sigma.f
#

sd.emi.share <- vector(); sd.emi.sum <- vector(); sd.emi.rel <- vector();k<-1
for (r in seq(0,1,0.1)){ # increase correlation in cov matrix
  
  
  cov.matrix <- matrix(c(sigma1^2, rep(r*sigma1*sigma2,2), sigma2^2), nrow=2  )
  
  # generate two distributions with inctreasing level of correaltion
  sample <- mvrnorm(nn, c(mean.nfood, mean.food), cov.matrix, empirical=T) # use empirical = TRUE to specify empirical 

  #Share of food to (food + non-food)
  share <- sample[,2]/ rowSums(sample) # simulate the share ratio (A/(A+B))
  unc.MC.share <- sqrt(var(share))
  sd.emi.share[k] <- unc.MC/(mean(share))
  
  #Relation between food and non-food)
  relf2nf <- sample[,2]/sample[, 1]
  unc.MC.rel <- sqrt(var(relf2nf))
  sd.emi.rel[k] <- unc.MC/(mean(relf2nf))

  ## Sum of food and non-food
  sum <-  rowSums(sample) # simulate the sum A+B
  unc.MC.sum <- sqrt(var(sum))
  sd.emi.sum[k] <- unc.MC/(mean(sum))
  
  k <- k+1
}

plot(sd.emi.share)
plot(sd.emi.rel)
plot(sd.emi.sum)
plot(share)

ctr <- as.data.table(read.xlsx(xlsxFile = paste0(edgar_folder, "../../classifications/Country categories plus alpha codes.xlsx"),
          sheet = "Breakdown_list_dev_level"))
ctr <- ctr[, .(Country_code_A3=ISO, dev = region_ar6_dev)]

load("C:/Users/adrian/google/projects/edgar/data/202005/edgar_food_20200527.rdata")
totemissions[, `:=` (food = EDGAR_FOOD + FAO_FOOD, total = EDGAR_FOOD + EDGAR_nFOOD + FAO_total)]
totemissions[, `:=` (share = food/total)]
totemissions <- merge(totemissions, ctr, by="Country_code_A3")

hist(totemissions[share > 0 & share < 1]$share, breaks = seq(0, 1, 0.02))
png(file="foodshare_countries_hist.png")
hist(totemissions[share > 0 & share < 1]$share, breaks = seq(0, 1, 0.02))
dev.off()
totemissions[, .(noutl = sum(share>0 & share < 1), outl = sum(share<0 | share > 1))][]

hist(totemissions[share > -1 & share < 3]$share, breaks = seq(-1, 3, 0.05))
png(file="foodshare_countries_hist2.png")
hist(totemissions[share > -1 & share < 3]$share, breaks = seq(-1, 3, 0.05))
dev.off()
totemissions[, .(noutl = sum(share>-1 & share < 3), outl = sum(share < -1 | share > 3))][]

hist(totemissions[share > -1 & share < 3 & variable==2015]$share, breaks = seq(-1, 3, 0.05))
png(file="foodshare_countries_hist2015.png")
hist(totemissions[share > -1 & share < 3 & variable==2015]$share, breaks = seq(-1, 3, 0.05))
dev.off()
totemissions[, .(noutl = sum(share>-1 & share < 3), outl = sum(share < -1 | share > 3))][]


hist(totemissions[share > -1 & share < 3 & variable==2015 & dev == "developed", share], breaks = seq(-1, 3, 0.05))
hist(totemissions[share > -1 & share < 3 & variable==2015 & dev == "developing", share], breaks = seq(-1, 3, 0.05))
hist(totemissions[share > -1 & share < 3 & variable==2015 & dev == "ldc", share], breaks = seq(-1, 3, 0.05))

summary(totemissions[share > -1 & share < 3 & variable==2015 & dev == "developed", share])
summary(totemissions[share > -1 & share < 3 & variable==2015 & dev == "developing", share])
summary(totemissions[share > -1 & share < 3 & variable==2015 & dev == "ldc", share])
sd(totemissions[share > -1 & share < 3 & variable==2015 & dev == "developed", share])
sd(totemissions[share > -1 & share < 3 & variable==2015 & dev == "developing", share])
sd(totemissions[share > -1 & share < 3 & variable==2015 & dev == "ldc", share])


#---- case of the sum 
sd.emi.perc <- vector(); k<-1
for (r in seq(0,1,0.1)){ # increase correlation in cov matrix
  
  
  cov.matrix <- matrix(c(sigma1^2, rep(r*sigma1*sigma2,2), sigma2^2), nrow=2  )
  
  # generate two distributions with inctreasing level of correaltion
  
  sample <- mvrnorm(nn, c(mean.tot, mean.food), cov.matrix, empirical=T) # use empirical = TRUE to specify empirical 
  
  sum <-  rowSums(sample) # simulate the sum A+B
  unc.MC <- sqrt(var(sum))
  sd.emi.perc[k] <- unc.MC/(mean(sum))
  
  k <- k+1
}

plot(sd.emi.perc)

