#Loading Libraries ---------------------------------
if(!require(pacman)) install.packages("pacman")
p_load(dplyr, lubridate, devtools, tidyr, ggplot2, plotly, knitr, kableExtra, 
       shiny,shinydashboard, TTR, forecast, ggfortify)

#Data Preprocessing ----------------------
#load data frame
dfFullYr <- readRDS("data/FullData.rds")

#Formate to POSIXct
dfFullYr$DateTime <- as.POSIXct(dfFullYr$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(dfFullYr$DateTime, "tzone") <- "Europe/Paris"

##Delete attribute >> dfFullYr$X <- NULL
##Rearrange columns >> dfFullYr <- test[,c(1:5,12,6:8,14,11,13,10,9)]
##Write to csv without column numbers >> write.csv(dfFullYr, "FullData.csv", row.names=FALSE)