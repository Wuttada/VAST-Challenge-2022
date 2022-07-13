library(tidyverse)
library(bsts)
library(lubridate)

FJ_Exp <- FinancialJournal %>% group_by(participantId) %>% 
  filter(category != "Wage" & category != "RentAdjustment")
FJ_Exp$weekday <- wday(FJ_Exp$timestamp, label=TRUE, abbr=FALSE)

FJ_Exp$amount.abs <- abs(FJ_Exp$amount)
FJ_Exp$Day <- strftime(FJ_Exp$timestamp, "%d")
FJ_Exp$Month <- strftime(FJ_Exp$timestamp, "%m")
FJ_Exp$Year <- strftime(FJ_Exp$timestamp, "%Y")

FJ_Shelter<- FJ_Exp %>% 
  filter(category == "Shelter")
FJ_Food<- FJ_Exp %>% 
  filter(category == "Food")
FJ_Recreation<- FJ_Exp %>% 
  filter(category == "Recreation")
FJ_Education<- FJ_Exp %>% 
  filter(category == "Education")

####EXTRACTED WEEKDAY FINANCIAL DATA
FJ_Exp_weekend<- FJ_Exp %>% 
  filter(weekday %in%  c("Saturday", "Sunday"))
####EXTRACTED WEEKEND FINANCIAL DATA
FJ_Exp_weekday<- FJ_Exp %>% 
  filter(weekday %in%  c("Monday","Tuesday","Wednesday","Thursday","Friday"))

####AGGREGATE SEPERATED DATA OF EACH DAY INTO ONE RECORD
Exp_aggr_wkd <- aggregate(amount.abs ~ Day + Month + Year,
                         FJ_Exp_weekday,
                         FUN = mean)

Exp_aggr_wkn <- aggregate(amount.abs ~ Day + Month + Year,
                          FJ_Exp_weekend,
                          FUN = mean)


####CREATE BSTS MODEL FOR WEEKDAY AND WEEKEND SPENDING             
ss_wkd <- AddLocalLinearTrend(list(), Exp_aggr_wkd$amount.abs)
ss_wkd <- AddSeasonal(ss_wkd, Exp_aggr_wkd$amount.abs, nseasons = 15)

model_wkd <- bsts(Exp_aggr_wkd$amount.abs, state.specification = ss_wkd, niter = 1000)
plot(model_wkd, cex = 0.5, pch = 16)
plot(model_wkd , "comp",  cex = 0.5, pch = 16, width = 15, length = 20)

pred_wkd <- predict(model_wkd, horizon = 30)
plot(pred_wkd, plot.original = 500)


####

ss_wkn <- AddLocalLinearTrend(list(), Exp_aggr_wkn$amount.abs)
ss_wkn <- AddSeasonal(ss_wkn, Exp_aggr_wkn$amount.abs, nseasons = 15)

model_wkn <- bsts(Exp_aggr_wkn$amount.abs, state.specification = ss_wkn, niter = 1000)
plot(model_wkn, cex = 0.5, pch = 16)
plot(model_wkn , "comp",  cex = 0.5, pch = 16, width = 15, length = 20)

pred_wkn <- predict(model_wkn, horizon = 30)
plot(pred_wkn, plot.original = 500)

#### MODEL FOR EVERYDAY EXPENSE
Exp_aggr_all <- aggregate(amount.abs ~ Day + Month + Year,
                          FJ_Exp,
                          FUN = mean)

ss_all <- AddLocalLinearTrend(list(), Exp_aggr_all$amount.abs)
ss_all <- AddSeasonal(ss_all, Exp_aggr_all$amount.abs, nseasons = 15)

model_all <- bsts(Exp_aggr_all$amount.abs, state.specification = ss_all, niter = 1000)
plot(model_all, cex = 0.5, pch = 16)
plot(model_all, "comp",  cex = 0.5, pch = 16, width = 15, length = 20)

pred_all <- predict(model_all, horizon = 30)
plot(pred_all, plot.original = 600)
