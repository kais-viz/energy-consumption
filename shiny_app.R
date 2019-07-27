## Shiny Dashboard: Energy Consumption
## Author: Kais Kawar
## 
## Notes: You need the dataset FullData10.csv and style.css in order to run this code

#Loading Libraries ---------------------------------
if(!require(pacman)) install.packages("pacman")
p_load(dplyr, lubridate, devtools, tidyr, ggplot2, knitr, kableExtra, 
       shiny, plotly,shinydashboard)

#Preprocess ----------------------------------------
dfFullYr <- readRDS("data/FullData10.rds")
#dfFullYr <- read.csv("data/FullData10.csv", row.names = NULL)
dfFullYr$DateTime <- as.POSIXct(dfFullYr$DateTime, "%Y/%m/%d %H:%M:%S")
attr(dfFullYr$DateTime, "tzone") <- "Europe/Paris"

#Functions -----------------------------------------
#Function that takes in two args, n for the data range and gran for the grouping in min, hr or days
userInput <- function(n, gran) {
  temp <- NULL
  if(gran == "Minute"){
    temp <- dfFullYr %>%
      filter(DateTime >= n$input_date[1] & DateTime <= n$input_date[2])
  } else if (gran == "Hour") {
    temp <- dfFullYr %>%
      filter(DateTime >= n$input_date[1] & DateTime <= n$input_date[2]) %>%
      group_by(YEAR = year(DateTime),
               MONTH = month(DateTime),
               DAY = day(DateTime),
               HOUR = hour(DateTime)) %>%
      summarise(SM_Others = sum(SM_Others),
                SM_Kitchen = sum(SM_Kitchen),
                SM_Laundry = sum(SM_Laundry),
                SM_HeaterAC = sum(SM_HeaterAC)) %>% 
      ungroup() %>%
      mutate(DateTime = paste0(YEAR,"-",MONTH,"-",DAY," ",HOUR,":00:00"),
             DateTime = as.POSIXct(DateTime, "%Y-%m-%d %h:%m:%s"))
  }else if(gran == "Day") {
    temp <- dfFullYr %>%
      filter(DateTime >= n$input_date[1] & DateTime <= n$input_date[2]) %>%
      group_by(YEAR = year(DateTime),
               MONTH = month(DateTime),
               DAY = day(DateTime)) %>%
      summarise(SM_Others = sum(SM_Others),
                SM_Kitchen = sum(SM_Kitchen),
                SM_Laundry = sum(SM_Laundry),
                SM_HeaterAC = sum(SM_HeaterAC)) %>% 
      ungroup() %>%
      mutate(DateTime = paste(YEAR,MONTH,DAY, sep = "-"),
             DateTime = as.POSIXct(DateTime, "%Y-%m-%d"))
  } else {
    temp <- dfFullYr %>%
      filter(DateTime >= n$input_date[1] & DateTime <= n$input_date[2]) %>%
      group_by(YEAR = year(DateTime),
               MONTH = MONTH(DateTime)) %>%
      summarise(SM_Others = sum(SM_Others),
                SM_Kitchen = sum(SM_Kitchen),
                SM_Laundry = sum(SM_Laundry),
                SM_HeaterAC = sum(SM_HeaterAC)) %>% 
      ungroup() %>%
      mutate(DateTime = paste(YEAR,MONTH, sep = "-"))
  }
  return(temp)
}

#Get sum of submeters and cost
getSum <- function(d1, d2) {
  return(dfFullYr %>% 
           filter(d1 <= DateTime & d2 >= DateTime) %>% 
           summarise(sumSM0kwh = (sum(SM_Others) / 1000),
                     sumSM1kwh = (sum(SM_Kitchen) / 1000),
                     sumSM2kwh = (sum(SM_Laundry) / 1000),
                     sumSM3kwh = (sum(SM_HeaterAC) / 1000),
                     sumTotalkwh = sumSM0kwh + sumSM1kwh + sumSM2kwh + sumSM3kwh) %>% 
           mutate(SM0CostEuro = c(sumSM0kwh * 0.15),
                  SM1CostEuro = c(sumSM1kwh * 0.15),
                  SM2CostEuro = c(sumSM2kwh * 0.15),
                  SM3CostEuro = c(sumSM3kwh * 0.15),
                  SMTotalCostEuro = c(sumTotalkwh * 0.15)) %>% 
           round())
}

#Get sum for certain submeter
getSubmeterSum <- function(d1,d2, sm) {
  sums <- getSum(d1,d2)
  return(sums[,sm])
}

#Title function
setTitle <- function(n) {
  temp_newdate1 <- strptime(as.character(n$input_date[1]), "%Y-%m-%d")
  temp_txtdate1 <- format(temp_newdate1, "%b %d, %Y")
  temp_newdate2 <- strptime(as.character(n$input_date[2]), "%Y-%m-%d")
  temp_txtdate2 <- format(temp_newdate2, "%b %d, %Y")
  title <- paste0("Your Power Consumption Between ", temp_txtdate1, " & ", 
                  temp_txtdate2)
  return(title)
}

## Dashboard compenents -------------------------------
#Components of the interface, and inputs/outputs
ui <- dashboardPage(
  dashboardHeader(
    title = "Energy Consumption",
    titleWidth = 230
  ),
  dashboardSidebar(
    dateRangeInput(inputId = "input_date", "Date range",
                   min = "2007-01-01",
                   max= "2010-11-26",
                   start = "2007-01-01",
                   end= "2007-2-31",
                   format = "yyyy-mm-dd",
                   startview = "month",
                   autoclose = TRUE),
    selectInput("input_gran", "Choose Detail:", 
                choices = list("Minute", "Hour","Day"), selected = "Day")
  ),
  dashboardBody(
    tabsetPanel(type="tab",
                tabPanel(id=1,"Power Usage Data", plotlyOutput(outputId = "output_plot1"),
                         fluidRow(
                           includeCSS("style.css"),
                           column(3,valueBoxOutput("kitchen")),
                           column(3,valueBoxOutput("laundry")),
                           column(3,valueBoxOutput("heaterac")),
                           column(3,valueBoxOutput("others"))),
                         fluidRow(
                           includeCSS("style.css"),
                           box(
                             status = "info",
                             width = 8,
                             #background = 'red',
                             htmlOutput("totalConsumption")
                           ))
                         ),
                tabPanel(id=2,"Recommendations", 
                         fluidRow(
                           box(
                             width = 6,
                             collapsible=TRUE,
                             background = "green",
                             p("Good morning, Tim. You consumed 23% less energy in the kitchen than the previous week. Nice Work!")
                           )),
                         fluidRow(
                           box(
                             width = 6,
                             collapsible=TRUE,
                             background = "red",
                             p("The software detected that your refrigerator energy efficiency level is too low. ", tags$a(href="www.amazon.de", "Click here"), "for a catered lst of efficient refrigerators and get a 10% discount!!"
                               , icon("tag", lib = "font-awesome"))
                           ))
                         )
                )
  )
)

#Takings input/output from the UI and we can create elements and embed plots
server <- function(input, output) {
  output$output_plot1 <- renderPlotly({
    plot_ly(userInput(input, input$input_gran), x = ~DateTime, y = ~SM_Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      #add_trace(y = ~SM_Kitchen, name = 'Kitchen', mode = 'lines') %>%
      add_trace(y = ~SM_Laundry, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~SM_HeaterAC, name = 'Water Heater & AC', mode = 'lines')%>%
      add_trace(y = ~SM_Others, name = 'Other Appliances', mode = 'lines') %>%
      layout(title = setTitle(input),
        xaxis = list(title = "Date"),
        yaxis = list (title = "Power (watt-hours)"))
  })
  output$kitchen <- renderValueBox({
    sm1 <-getSubmeterSum(input$input_date[1], input$input_date[2], "sumSM1kwh")
    sm1c <-getSubmeterSum(input$input_date[1], input$input_date[2], "SM1CostEuro")
    
    infoBox(
      title="Kitchen",
      subtitle = paste0(formatC(sm1, digits = 0, format = "f"), " kWh"),
      value = paste0("\u20ac",formatC(sm1c, digits = 0, format = "f"), " Euros"),
      icon = icon("utensils"),
      color = "aqua"#if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })
  output$laundry <- renderValueBox({
    sm2 <-getSubmeterSum(input$input_date[1], input$input_date[2], "sumSM2kwh")
    sm2c <-getSubmeterSum(input$input_date[1], input$input_date[2], "SM2CostEuro")
    
    infoBox(
      title="Laundry",
      subtitle = paste0(formatC(sm2, digits = 0, format = "f"), " kWh"),
      value = paste0("\u20ac",formatC(sm2c, digits = 0, format = "f"), " Euros"),
      icon = icon("tshirt"),
      color = "orange"
    )
  })
  output$heaterac <- renderValueBox({
    sm3 <-getSubmeterSum(input$input_date[1], input$input_date[2], "sumSM3kwh")
    sm3c <-getSubmeterSum(input$input_date[1], input$input_date[2], "SM3CostEuro")
    
    infoBox(
      title="Heater & AC",
      subtitle = paste0(formatC(sm3, digits = 0, format = "f"), " kWh"),
      value = paste0("\u20ac",formatC(sm3c, digits = 0, format = "f"), " Euros"),
      icon = icon("burn"),
      color = "green"
    )
  })
  output$others <- renderValueBox({
    sm0 <-getSubmeterSum(input$input_date[1], input$input_date[2], "sumSM0kwh")
    sm0c <-getSubmeterSum(input$input_date[1], input$input_date[2], "SM0CostEuro")
    infoBox(
      title="Other Appliances",
      subtitle = paste0(formatC(sm0, digits = 0, format = "f"), " kWh"),
      value = paste0("\u20ac",formatC(sm0c, digits = 0, format = "f"), " Euros"),
      icon = icon("plug"),
      color = "red"
    )
  })
  output$totalConsumption <- renderText({
    smTotal <-getSubmeterSum(input$input_date[1], input$input_date[2], "sumTotalkwh")
    smTotalc <-getSubmeterSum(input$input_date[1], input$input_date[2], "SMTotalCostEuro")
    paste0(h4("Your total energy consumption for the selected duration was ", 
              tags$b(formatC(smTotal, digits = 0, format = "f")), 
              " kWh, and the total cost is \u20ac", 
              tags$b(formatC(smTotalc, digits = 0, format = "f"))))
  })
}

#Run server to see results
shinyApp(ui, server)


##visualizations

# SubSet1year <- dfFullYr %>% 
#   #filter(year(DateTime) == 2009) %>%
#   group_by(DAY = day(DateTime), 
#            MONTH = month(DateTime), YEAR = year(DateTime)) %>%
#   summarise(SM_Others = sum(SM_Others),
#             SM_Kitchen = sum(SM_Kitchen),
#             SM_Laundry = sum(SM_Laundry),
#             SM_HeaterAC = sum(SM_HeaterAC)) %>% 
#   mutate(SumSubmeter = SM_Others + SM_Kitchen + SM_Laundry + SM_HeaterAC) %>%
#   ungroup() %>%
#   mutate(DateTime = paste(YEAR,MONTH,DAY, sep = "-"),
#          DateTime = as.POSIXct(DateTime, "%Y-%m-%d")) %>%
#   arrange(DateTime)
# 
# 
# ggplot(SubSet1year, aes(x = DAY, y = SumSubmeter/1000, color = factor(YEAR))) +
#   geom_smooth(se=F) +
#   # facet_grid(YEAR ~.) +
#   labs(x='Day', y='kWh',color="Legend", title = 'Energy Consumption Yearly Comparison')+
#   theme_minimal()
# 
# plot_ly(SubSet1year, x = ~DateTime, y = ~SumSubmeter, name = 'Sum Submeter', type = 'scatter', mode = 'lines') %>%
#   layout(#title = setTitle(input),
#     title = "Power Consumption in a 2008 Winter Monday (11th Feb, 08)",
#     xaxis = list(title = "Day"),
#     yaxis = list (title = "Power (watt-hours)"))

temp <- dfFullYr %>%
  filter(DateTime >= start & DateTime <= end) %>%
  group_by(YEAR = year(DateTime),
           MONTH = month(DateTime),
           DAY = day(DateTime),
           HOUR = hour(DateTime)) %>%
  summarise(SM_Others = sum(SM_Others),
            SM_Kitchen = sum(SM_Kitchen),
            SM_Laundry = sum(SM_Laundry),
            SM_HeaterAC = sum(SM_HeaterAC)) %>% 
  ungroup() %>%
  mutate(DateTime = paste0(YEAR,"-",MONTH,"-",DAY," ",HOUR,":00:00"),
         DateTime = as.POSIXct(DateTime, "%Y-%m-%d %h:%m:%s"))
