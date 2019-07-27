#Data Preprocessing ----------------------
#load data frame
dfFullYr10 <- readRDS("data/FullData.rds")

#create year, month, day attributes
dfFullYr10$Year <- year(dfFullYr10$Date)
dfFullYr10$Quarter <- quarter(dfFullYr10$Date)
dfFullYr10$Month <- month(dfFullYr10$Date)
dfFullYr10$Day <- day(dfFullYr10$Date)
dfFullYr10$Hour <- hour(dfFullYr10$Date)

#Converting date/time to single attribute
DateTime <- paste(dfFullYr10$Date,dfFullYr10$Time)
dfFullYr10 <-cbind(dfFullYr10,DateTime, stringsAsFactors=FALSE)
dfFullYr10 <- select(dfFullYr10, -c(Date, Time))

#Formate to POSIXct
dfFullYr10$DateTime <- as.POSIXct(dfFullYr10$DateTime, "%Y/%m/%d %H:%M:%S")
# Add the time zone
attr(dfFullYr10$DateTime, "tzone") <- "Europe/Paris"

#move back to first position
dfFullYr10 <- dfFullYr10[,c(ncol(dfFullYr10), 1:(ncol(dfFullYr10)-1))]

#creating "global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3"
dfFullYr10 <- dfFullYr10 %>% mutate(Sub_metering_0 = Global_active_power*1000/60 - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

#rename
dfFullYr10 %>% 
  rename(
    SM_Others = Sub_metering_0,
    SM_Kitchen = Sub_metering_1,
    SM_Laundry = Sub_metering_2,
    SM_HeaterAC = Sub_metering_3) ->dfFullYr10

##Delete attribute >> dfFullYr10$X <- NULL
##Rearrange columns >> 
dfFullYr10 <- dfFullYr10[,c(1:5,14,6:8,9:13)]
##Write to csv without column numbers >>
#write.csv(dfFullYr10, "FullData10.csv", row.names=FALSE)


#forecasting 2010 ----------------
dataForecast <- dfFullYr %>%
  #filter(Year == 2010) %>%
  group_by(YEAR = year(DateTime),
           MONTH = month(DateTime),
           DAY = day(DateTime)) %>%
  summarise(SM_Others = sum(SM_Others),
            SM_Kitchen = sum(SM_Kitchen),
            SM_Laundry = sum(SM_Laundry),
            SM_HeaterAC = sum(SM_HeaterAC),
            GAP = sum(Global_active_power)) %>% 
  ungroup() %>%
  mutate(DateTime = paste0(YEAR,"-",MONTH,"-",DAY),
         DateTime = as.POSIXct(DateTime, "%Y-%m-%d"))

tsForecast <- ts(dataForecast$GAP, frequency=365)
fitGAP <- tslm(tsForecast ~ trend + season) 
summary(fitGAP)

## Create the forecast for sub-meter 1. Forecast ahead 20 time periods 
forecastfitGAP <- forecast(fitGAP, h=34)
## Plot the forecast for sub-meter 1. 
autoplot(forecastfitGAP)

componentstsForecast <- decompose(tsForecast)
## Plot decomposed sub-meter 1 
plot(componentstsForecast)
tsForecastAdjusted <- tsForecast - componentstsForecast$seasonal
autoplot(tsForecastAdjusted)

tsGAP_HW10 <- HoltWinters(tsForecastAdjusted, beta=FALSE, gamma=FALSE)
plot(tsGAP_HW10)

tsGAP_HW10for <- forecast(tsGAP_HW10, h=25)
plot(tsGAP_HW10for, ylab= "Watt-Hours", xlab="Time -GAP")


#beta/gamma true
tsGAP_HW10 <- HoltWinters(tsForecast)
plot(tsGAP_HW10)

tsGAP_HW10for <- forecast(tsGAP_HW10, h=399)
plot(tsGAP_HW10for,ylim = c(0, 4050), ylab= "Watt-Hours", xlab="Time -GAP")

#reove seasonal and random to predict future trends
tsForecastAdjusted1 <- tsForecast - componentstsForecast$seasonal - componentstsForecast$random

fitGAPAdjusted <- tslm(tsForecastAdjusted1 ~ trend + season) 
summary(fitGAPAdjusted)

## Create the forecast. Forecast ahead 34 time periods 
forecastfitGAPAdjusted <- forecast(fitGAPAdjusted, h=34)
## Plot the forecast for sub-meter 1. 
plot(forecastfitGAPAdjusted)
