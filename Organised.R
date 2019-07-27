#Loading Libraries and Dataset preprocess --------------
source("EC_Preprocess.R")

#Visualizations --------------------------
EnergyConsumption <- dfFullYr

#Energy consumption in a winter day by meters 
Winter_day <- EnergyConsumption %>%
  filter(day(DateTime) == 11 & month(DateTime) == 2 & year(DateTime) == 2008)
#filter(DateTime > "2008-02-11" & DateTime < "2008-02-12")

ggplot(Winter_day) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  theme_minimal()+
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a 2008 winter day')

SMALLTS <- ts(Winter_day$Sub_metering_0,
              frequency = 60)


tsAVG <- SMA(SMALLTS,n=60)
plot.ts(tsAVG)

autoplot(SMALLTS, ts.colour = 'red', xlab = "Hour", ylab = "Watt Hours", main = "Sub-meter 0")

#Energy consumption in an autumn week by meters 
Autumn_week <- EnergyConsumption %>%
  filter(DateTime > "2008-11-10" & DateTime < "2008-11-16")

ggplot(Autumn_week) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a 2008 autumn week')

#Enery Consumption in Spring
Spring_Period <- EnergyConsumption %>%
  filter(DateTime > "2008-03-21" & DateTime < "2008-06-20") %>%
  group_by(DateTime = DateTime, 
            HOUR = hour(DateTime), 
            DAY = day(DateTime),
            MONTH = month(DateTime)) %>%
  summarise(SM_Others = sum(Sub_metering_0),
            SM_Kitchen = sum(Sub_metering_1),
            SM_Laundry = sum(Sub_metering_2),
            SM_HeaterAirCon = sum(Sub_metering_3)) %>% 
  ungroup()

Spring_Period_Gathered <- Spring_Period %>% 
  gather(key = "Submeters", 
         value = "value", 
         SM_Others, SM_Kitchen, SM_Laundry, SM_HeaterAirCon)

ggplot(Spring_Period_Gathered) +
  geom_line(aes(x= DateTime, y= value, color=Submeters)) +
  facet_grid(Submeters ~.) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a 2008 autumn week')

ggplot(Spring_Period_Gathered) +
  geom_line(aes(x= DateTime, y= value, color=Submeters)) +
  facet_grid(Submeters ~.) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh', title = 'Energy Consumption in a 2008 autumn week') +
  theme(legend.position="none")

ggplot(Spring_Period) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a 2008 autumn week')

## Plot sub-meter 3 with autoplot - add labels, color
SMALLTS <- ts(Spring_Period$SM_Others,
              frequency = 1440)


tsAVG <- SMA(SMALLTS,n=1440)
plot.ts(tsAVG)

autoplot(SMALLTS, ts.colour = 'red', xlab = "Days", ylab = "Watt Hours", main = "Sub-meter 3")


SubSet1year <- EnergyConsumption %>% 
  filter(day(DateTime) == 11 & month(DateTime)==2) %>%
  group_by(DateTime = DateTime, 
           DAY = day(DateTime), 
           MONTH = month(DateTime), YEAR = year(DateTime)) %>%
  summarise(SM_Others = sum(Sub_metering_0),
            SM_Kitchen = sum(Sub_metering_1),
            SM_Laundry = sum(Sub_metering_2),
            SM_HeaterAirCon = sum(Sub_metering_3)) %>% 
  mutate(SumSubmeter = SM_Others + SM_Kitchen + SM_Laundry + SM_HeaterAirCon) %>%
  ungroup() %>% 
  gather(key = "Submetters", value = "value", SM_Others, SM_Kitchen, SM_Laundry, SM_HeaterAirCon, SumSubmeter)

Winter_day <- EnergyConsumption %>%
  filter(day(DateTime) == 11 & month(DateTime) == 2 & year(DateTime) == 2008)
  #filter(DateTime > "2008-02-11" & DateTime < "2008-02-12")

ggplot(Winter_day) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a 2008 winter day')
  
plot_ly(Winter_day, x = ~DateTime, y = ~Sub_metering_2, type = 'scatter', mode = 'lines')

SubSet1year <- EnergyConsumption %>% 
  filter(year(DateTime) == 2007 | year(DateTime) == 2009) %>%
  group_by(DateTime = DateTime, 
           DAY = day(DateTime), 
           MONTH = month(DateTime), YEAR = year(DateTime)) %>%
  summarise(SM_Others = sum(Sub_metering_0),
            SM_Kitchen = sum(Sub_metering_1),
            SM_Laundry = sum(Sub_metering_2),
            SM_HeaterAirCon = sum(Sub_metering_3)) %>% 
  mutate(SumSubmeter = SM_Others + SM_Kitchen + SM_Laundry + SM_HeaterAirCon) %>%
  ungroup() %>% 
  gather(key = "Submetters", value = "value", SM_Others, SM_Kitchen, SM_Laundry, SM_HeaterAirCon, SumSubmeter)


ggplot(SubSet1year, aes(x = MONTH, y = value, color = Submetters)) +
  geom_smooth(se=F) +
  facet_grid(YEAR ~.) +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption 2007 & 2009 Comparison')+
  theme_minimal()


SMALLTS <- ts(SubSet1year$value,
                     frequency = 7, start=c(2007,1))
tstest <- SMA(SMALLTS,n=60)
plot.ts(tstest)

#Energy consumption in a autumn day by meters  
Autumn_week <- EnergyConsumption %>%
  filter(DateTime > "2008-11-10" & DateTime < "2008-11-16")

ggplot(Autumn_week) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a 2008 autumn week')

#Energy consumption in a spring day by meters
Spring_day <- EnergyConsumption %>%
  filter(DateTime > "2009-5-09" & DateTime < "2009-5-10")

ggplot(Spring_day) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a spring day')

#Energy consumption in a summer day by meters
Summer_day <- EnergyConsumption %>%
  filter(DateTime > "2009-07-25" & DateTime < "2009-07-26")

ggplot(Summer_day) + 
  geom_line(aes(x=DateTime, y=Sub_metering_0, color='Submeter 0')) +
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='Submeter 1')) +
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='Submeter 2')) +
  geom_line(aes(x=DateTime, y=Sub_metering_3, color='Submeter 3')) +
  scale_colour_brewer(palette = "Set1") +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption in a summer day')

summer <- EnergyConsumption %>% filter(Quarter == 3)
#test
testt <- EnergyConsumption %>%
  filter(DateTime > "2007-04-28" & DateTime < "2007-04-29")

dat <- data.frame(
  time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)


#Bargraph tests ----------
# Very basic bar graph
ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity")

# Map the time of day to different fill colors
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(stat="identity")

ggplot(data=Subset3years, aes(x=YEAR, y=SumSM, fill=QUARTER)) +
  geom_bar(stat="identity", position ="dodge")
#+ facet_grid(Year ~ . )

EnergyConsumption$Year <- as.factor(EnergyConsumption$Year)
EnergyConsumption$Quarter <- as.factor(EnergyConsumption$Quarter)

## This would have the same result as above
# ggplot(data=dat, aes(x=time, y=total_bill)) +
#    geom_bar(aes(fill=time), stat="identity")

# Add a black outline
# No legend, since the information is redundant
ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)

#The consumption chart comparison between 07 and 09
SubSet1year <- dfFullYr %>% 
  filter(year(DateTime) == 2007 | year(DateTime) == 2009) %>%
  group_by(DateTime = DateTime, DAY = day(DateTime), 
           MONTH = month(DateTime), YEAR = year(DateTime)) %>%
  summarise(SM_Others = sum(SM_Others),
            SM_Kitchen = sum(SM_Kitchen),
            SM_Laundry = sum(SM_Laundry),
            SM_HeaterAC = sum(SM_HeaterAC)) %>% 
  mutate(SumSubmeter = SM_Others + SM_Kitchen + SM_Laundry + SM_HeaterAC) %>%
  ungroup() %>% 
  gather(key = "Submetters", value = "value", SM_Others, SM_Kitchen, SM_Laundry, SM_HeaterAC, SumSubmeter)

#use ggplot to show the comparison
ggplot(SubSet1year, aes(x = MONTH, y = value, color = Submetters)) +
  geom_smooth(se=F) +
  facet_grid(YEAR ~.) +
  labs(x='DateTime', y='kWh',color="Legend", title = 'Energy Consumption 2007 & 2009 Comparison')+
  theme_minimal()

#barchart
SubSet1year$MONTH <- as.factor(SubSet1year$MONTH)
ggplot(Subset1years, aes(x=Month, y=SumSM, fill=QUARTER)) +
  geom_bar(stat="identity", position ="dodge2")
#+ facet_grid(Year ~ . )
