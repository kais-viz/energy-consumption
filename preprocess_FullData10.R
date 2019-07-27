library(dplyr)
#Load file

yr07 <- read.csv("data/yr_2007.csv")
yr08 <- read.csv("data/yr_2008.csv")
yr09 <- read.csv("data/yr_2009.csv")

dfFullYr <- bind_rows(yr07, yr08, yr09)

#Converting date/time to single attribute
DateTime <- paste(dfFullYr$Date,dfFullYr$Time)
dfFullYr <-cbind(dfFullYr,DateTime, stringsAsFactors=FALSE)
dfFullYr <- select(dfFullYr, -c(Date, Time))

dfFullYr$DateTime <- as.POSIXct(dfFullYr$DateTime, "%Y/%m/%d %H:%M:%S")
attr(dfFullYr$DateTime, "tzone") <- "Europe/Paris"

#move back to first position
dfFullYr <- dfFullYr[,c(ncol(dfFullYr), 1:(ncol(dfFullYr)-1))]

#creating "global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3"
dfFullYr <- dfFullYr %>% mutate(Sub_metering_0 = Global_active_power*1000/60 - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

dfFullYr$Year <- year(dfFullYr$DateTime)
dfFullYr$Month <- month(dfFullYr$DateTime)
dfFullYr$Day <- day(dfFullYr$DateTime)
dfFullYr$Hour <- hour(dfFullYr$DateTime)

#rename
names(dfFullYr)[6] <- "SM_Others"
names(dfFullYr)[7] <- "SM_Kitchen"
names(dfFullYr)[8] <- "SM_Laundry"
names(dfFullYr)[9] <- "SM_HeaterAC"
