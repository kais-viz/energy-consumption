#Loading Libraries and Dataset preprocess --------------
source("EC_Preprocess.R")

## Data visualizations
## Subset the second week of 2008 - All Observations
houseWeek <- filter(dfFullYr, Year == 2008 & Week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

ggplot(houseWeek, aes(x=DateTime, y=Sub_metering_2)) +
  geom_line()

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(dfFullYr, Year == 2008 & Month == 1 & Day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(dfFullYr, 
                     DateTime > "2008-01-10" & 
                       DateTime < "2008-01-16" &
                       #Month == 2 & 
                       #Week == 1 & 
                       #Day == 8 & 
                       (Minute == 0 | 
                          Minute == 10 | 
                          Minute == 20 | 
                          Minute == 30 | 
                          Minute == 40 | 
                          Minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



#Visualizations --------------------------
#Energy consumption in a winter day by meters 
EnergyConsumption <- dfFullYr

Winter_week <- EnergyConsumption %>%
  filter(DateTime > "2008-02-10" & DateTime < "2008-02-16")

ggplot(Winter_week) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a winter day')

#Energy consumption in a autumn day by meters  
Autumn_week <- EnergyConsumption %>%
  filter(DateTime > "2008-10-13" & DateTime < "2008-10-19")

ggplot(Autumn_week) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a autumn day')

#Energy consumption in a spring day by meters
Spring_week <- EnergyConsumption %>%
  filter(DateTime > "2009-5-04" & DateTime < "2009-5-10")

ggplot(Spring_week) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a spring day')

#Energy consumption in a summer day by meters
Summer_week <- EnergyConsumption %>%
  filter(DateTime > "2009-06-4" & DateTime < "2009-06-14")

ggplot(Summer_week) + 
  #geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  #geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  #geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a summer day')

###
summed07 <- dfFullYr %>%
  rename(SM_Other = Sub_metering_0, SM_Kitchen = Sub_metering_1,SM_Laundry = Sub_metering_2,SM_HeatCool = Sub_metering_3) %>%
  filter(Year == 2007) %>%
  select(-c("DateTime")) %>%
  summarise_all(funs(sum)) %>%
  gather(key = "Submetters", value = "value", SM_Other, SM_Kitchen, SM_Laundry, SM_HeatCool)

summed08 <- dfFullYr %>%
  rename(SM_Other = Sub_metering_0, SM_Kitchen = Sub_metering_1,SM_Laundry = Sub_metering_2,SM_HeatCool = Sub_metering_3) %>%
  filter(Year == 2008) %>%
  select(-c("DateTime")) %>%
  summarise_all(funs(sum)) %>%
  gather(key = "Submetters", value = "value", SM_Other, SM_Kitchen, SM_Laundry, SM_HeatCool)

summed09 <- dfFullYr %>%
  rename(SM_Other = Sub_metering_0, SM_Kitchen = Sub_metering_1,SM_Laundry = Sub_metering_2,SM_HeatCool = Sub_metering_3) %>%
  filter(Year == 2009) %>%
  select(-c("DateTime")) %>%
  summarise_all(funs(sum)) %>%
  gather(key = "Submetters", value = "value", SM_Other, SM_Kitchen, SM_Laundry, SM_HeatCool)

#Bar chart 2007 power consumption
ggplot(summed07, aes(x=Submetters, y=value/1000, fill=Submetters)) + 
  geom_bar(stat="identity", width=1) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Kilo Watt", title = "Power Consumption in 2007")

#Bar chart 2008 power consumption
ggplot(summed08, aes(x=Submetters, y=value/1000, fill=Submetters)) + 
  geom_bar(stat="identity", width=1) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Kilo Watt", title = "Power Consumption in 2008")

#Bar chart 2009 power consumption
ggplot(summed09, aes(x=Submetters, y=value/1000, fill=Submetters)) + 
  geom_bar(stat="identity", width=1) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Kilo Watt", title = "Power Consumption in 2009")
