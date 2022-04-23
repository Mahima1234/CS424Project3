library(shinydashboard)
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(DT)
library(leaflet)
library(rgdal)
library(shinyjs)
library(data.table)
library(plyr)



Data = ldply(list.files(pattern="*.csv", full.name = TRUE), fread)
#Data <- fread("Taxi8.csv")
Data <- as(Data, "data.frame")
#colnames(Data) <- c("Trip_Start_Timestamp","Trip_Seconds","Trip_Miles","Pickup_Community_Area","Dropoff_Community_Area","Company")
colnames(Data) <- c("index", "Trip_Start_Timestamp","Trip_Seconds","Trip_Miles","Pickup_Community_Area","Dropoff_Community_Area","Company")
Data$dayOfTheWeek <- weekdays(mdy_hms(Data$Trip_Start_Timestamp))
Data$dayOfTheYear <- date(mdy_hms(Data$Trip_Start_Timestamp))
Data$hourOfDay <- hour(mdy_hms(Data$Trip_Start_Timestamp))
Data$monthOfYear <- months(mdy_hms(Data$Trip_Start_Timestamp))
Data$Trip_Km <- Data$Trip_Miles * 1.609344



dayOfTheWeekTab <- as(count(weekdays(mdy_hms(Data$Trip_Start_Timestamp))), "data.frame")
dayOfTheYearTab <- as(count(date(mdy_hms(Data$Trip_Start_Timestamp))), "data.frame")
hourOfTheDayTab <- as(count(hour(mdy_hms(Data$Trip_Start_Timestamp))), "data.frame")
monthOfYearTab <- as(count(months(mdy_hms(Data$Trip_Start_Timestamp))), "data.frame")
tripMilesTab <- as(count(Data$Trip_Miles), "data.frame")
tripSecondsTab <- as(count(Data$Trip_Seconds), "data.frame")

rv1 <- reactiveValues()
rv1$myDf <- NULL
rv2 <- reactiveValues()
rv2$myDf <- NULL
rv3 <- reactiveValues()
rv3$myDf <- NULL

myspdf = readOGR(dsn = getwd(), layer = "geo_export_a5d1cc2d-a8cf-452d-8722-7de9a66a1a86")
myspdf.df <- as(myspdf, "data.frame")
colnames(myspdf.df) <- c("area", "Pickup_Community_Area", "Dropoff_Community_Area", "comarea", "comarea_id", "community", "perimeter", "shape_area", "shape_len")
myspdf.df$Pickup_Community_Area <- as.integer(myspdf.df$Pickup_Community_Area)
myspdf.df$Dropoff_Community_Area <- as.integer(myspdf.df$Dropoff_Community_Area)

newDf <- myspdf.df[c(2, 6)]
Data <- merge(Data, newDf, by.x = "Pickup_Community_Area", by.y = "Pickup_Community_Area")
Data <- merge(Data, newDf, by.x = "Dropoff_Community_Area", by.y = "Pickup_Community_Area")


pickupPercentage <- c()
dropoffPercentage <- c()
for(i in unique(myspdf.df$Pickup_Community_Area)){
  pickupPercentage <- append(pickupPercentage, (nrow(Data[Data$Pickup_Community_Area == i,])/nrow(Data)) * 100)
  dropoffPercentage <- append(dropoffPercentage, (nrow(Data[Data$Dropoff_Community_Area == i,])/nrow(Data)) * 100)
}
myspdf$pickup <- pickupPercentage
myspdf.df$pickup <- pickupPercentage
myspdf$dropoff <- dropoffPercentage
myspdf.df$dropoff <- dropoffPercentage

companyOptions <- sort(unique(Data$Company))
commAreaOptions <- sort(unique(myspdf.df$community))



ui <- dashboardPage(
  dashboardHeader(title = "CS 424 Project 3"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
    sidebarMenu(
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      menuItem("Dashboard", tabName = "default", icon = NULL),
      menuItem("Community Area", tabName = "CommunityArea", icon = NULL),
      menuItem("Company", tabName = "Company", icon = NULL),
      radioButtons("morkm", "Choose Miles or KM", choices = c("Miles", "KM")),
      radioButtons("time", "Choose 12-hour time or 24-hour time", choices = c("12-hour", "24-hour")),
      radioButtons("pickordrop", "Choose Pickup or Dropoff", choices = c("Pickup", "Dropoff")),
      selectizeInput("communityArea1", "Select Community Area", commAreaOptions),
      selectInput("company", "Select Company", companyOptions, selected = "Taxi Affiliation Services")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "default",
        column(3,
          fluidRow(
            id = "BoxOne",
            box(status = "primary", width = 20,
                plotOutput("dayOfTheWeekPlot", height = 300),
            ),
            box(status = "primary", width = 20,
                plotOutput("monthOfYearPlot", height = 300),
            ),
            box(status = "primary", width = 20,
                DT::dataTableOutput("dayOfTheWeekTable", height = 300)
            ),
            box(status = "primary", width = 20,
                DT::dataTableOutput("monthOfTheYearTable", height = 300)
            )
          )
        ),
        column(1,
               #Spacer Column  
        ),
        column(3,
            fluidRow(
              id = "BoxTwo",
              box(status = "primary", width = 20,
                  plotOutput("dayOfTheYearPlot", height = 300),
              ),
              box(status = "primary", width = 20,
                  plotOutput("tripSeconds", height = 300),
              ),
              box(status = "primary", width = 20,
                  DT::dataTableOutput("dayOfTheYearTable", height = 300)
              ),
              box(status = "primary", width = 20,
                  DT::dataTableOutput("tripSecondsTable", height = 300)
              )
            ),
        ),
        column(1,
               #Spacer Column  
        ),
        column(3,
               fluidRow(
                 id = "BoxThree",
                 box(status = "primary", width = 20,
                     plotOutput("hourOfDayPlot", height = 300),
                 ),
                 box(status = "primary", width = 20,
                     plotOutput("tripMiles", height = 300),
                 ),
                 box(status = "primary", width = 20,
                     DT::dataTableOutput("hourOfTheDayTable", height = 300)
                 ),
                 box(status = "primary", width = 20,
                     DT::dataTableOutput("tripMilesTable", height = 300)
                 )
               ),
        ),
      ),
      
      tabItem(
        tabName = "CommunityArea",
        fluidRow(
          id = "PickupCommunity",
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("dayOfTheWeekPickupMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("monthOfTheYearPickupMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheWeekPickupTable", height = 300)
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("monthOfTheYearPickupTable", height = 300)
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("dayOfTheYearPickupMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("tripSecondsPickupMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheYearPickupTable", height = 300)
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripSecondsPickupTable", height = 300)
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("hourOfTheDayPickupMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("tripMilesPickupMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("hourOfTheDayPickupTable", height = 300)
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripMilesPickupTable", height = 300)
                   )
                 )
          ),
          column(3,
                 leafletOutput(outputId = "map1", height = 650),
          ),
        ),
        fluidRow(
          id = "DropoffCommunity",
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("dayOfTheWeekDropoffMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("monthOfTheYearDropoffMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheWeekDropoffTable", height = 300)
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("monthOfTheYearDropoffTable", height = 300)
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("dayOfTheYearDropoffMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("tripSecondsDropoffMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheYearDropoffTable", height = 300)
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripSecondsDropoffTable", height = 300)
                   )
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("hourOfTheDayDropoffMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("tripMilesDropoffMap", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("hourOfTheDayDropoffTable", height = 300)
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripMilesDropoffTable", height = 300)
                   )
                 )
          ),
          column(3,
                 leafletOutput(outputId = "map2", height = 650),
          ),
        ),
        
      ),
      tabItem(
        tabName = "Company",
        fluidRow(
          id = "CompanyPickupCommunity",
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("companyDayOfTheWeekPickup", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("companyMonthOfTheYearPickup", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheWeekPickupCompanyTable", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("monthOfTheYearPickupCompanyTable", height = 300),
                   ),
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("companyDayOfTheYearPickup", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("companyTripSecondsPickup", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheYearPickupCompanyTable", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripSecondsPickupCompanyTable", height = 300),
                   ),
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("companyHourOfTheDayPickup", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("companyTripMilesPickup", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("hourOfTheDayPickupCompanyTable", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripMilesPickupCompanyTable", height = 300),
                   ),
                 )
          ),
          column(3,
                 leafletOutput(outputId = "map3", height = 650),
          ),
          
        ),
        fluidRow(
          id = "CompanyDropoffCommunity",
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("companyDayOfTheWeekDropoff", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("companyMonthOfTheYearDropoff", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheWeekDropoffCompanyTable", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("monthOfTheYearDropoffCompanyTable", height = 300),
                   ),
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("companyDayOfTheYearDropoff", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("companyTripSecondsDropoff", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("dayOfTheYearDropoffCompanyTable", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripSecondsDropoffCompanyTable", height = 300),
                   ),
                 )
          ),
          column(3,
                 fluidRow(
                   box(status = "primary", width = 20,
                       plotOutput("companyHourOfTheDayDropoff", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       plotOutput("companyTripMilesDropoff", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("hourOfTheDayDropoffCompanyTable", height = 300),
                   ),
                   box(status = "primary", width = 20,
                       DT::dataTableOutput("tripMilesDropoffCompanyTable", height = 300),
                   ),
                 )
          ),
          column(3,
                 leafletOutput(outputId = "map4", height = 650),
          ),
        ),
      )
      
    ),

  )
)

server <- function(input, output, session) {
  output$dayOfTheWeekPlot <- renderPlot({
    ggplot(Data, aes(dayOfTheWeek)) + geom_bar(fill="steelblue")
  })
  output$dayOfTheYearPlot <- renderPlot({
    ggplot(Data, aes(dayOfTheYear)) + geom_bar(fill="steelblue")
  })
  output$hourOfDayPlot <- renderPlot({
    if(input$time == "12-hour"){
      ggplot(Data, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
    }else{
      ggplot(Data, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
    }
  })
  output$monthOfYearPlot <- renderPlot({
    ggplot(Data, aes(monthOfYear)) + geom_bar(fill="steelblue")
  })
  output$tripSeconds <- renderPlot({
    ggplot(Data, aes(Trip_Seconds)) + geom_histogram(color = "white", fill = "steelblue")
  })
  output$tripMiles <- renderPlot({
    if(input$morkm == "Miles"){
      ggplot(Data, aes(Trip_Miles)) + geom_histogram(color = "white", fill = "steelblue")
    }else{
      ggplot(Data, aes(Trip_Km)) + geom_histogram(color = "white", fill = "steelblue")
    }
  })
  
  output$dayOfTheWeekTable <- renderDataTable({
    DT::datatable({
      dayOfTheWeekTab
    },
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Day", "Rides")
    )
  })
  
  output$dayOfTheYearTable <- renderDataTable({
    DT::datatable({
      dayOfTheYearTab
    },
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Rides")
    )
  })
  
  
  output$hourOfTheDayTable <- renderDataTable({
    DT::datatable({
      hourOfTheDayTab
    },
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE,colnames = c("Hour", "Rides")
    )
  })
  
  output$monthOfTheYearTable <- renderDataTable({
    DT::datatable({
      monthOfYearTab
    },
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Month", "Rides")
    )
  })
  
  output$tripMilesTable <- renderDataTable({
    DT::datatable({
      tripMilesTab 
    },
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Miles", "Rides")
    )
  })
  
  output$tripSecondsTable <- renderDataTable({
    DT::datatable({
      tripSecondsTab 
    },
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Seconds", "Rides")
    )
  })
  
  
  output$map1 <- renderLeaflet({
    if(input$pickordrop == "Pickup"){
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$pickup, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(pickup),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }else{
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$dropoff, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(dropoff),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }

  })
  
  output$map2 <- renderLeaflet({
    if(input$pickordrop == "Pickup"){
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$pickup, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(pickup),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }else{
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$dropoff, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(dropoff),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }
    
  })

  
  output$map3 <- renderLeaflet({
    if(input$pickordrop == "Pickup"){
      nrow(Data[Data$Pickup_Community_Area == i,])/nrow(Data)
      companyData <- Data[Data$Company == input$company,]
      companyPickupPercentage <- c()
      for(i in unique(myspdf.df$Pickup_Community_Area)){
        companyPickupPercentage <- append(companyPickupPercentage, (nrow(companyData[companyData$Pickup_Community_Area == i,])/nrow(companyData)) * 100)
      }
      myspdf$companyPickup <- companyPickupPercentage
      myspdf.df$companyPickup <- companyPickupPercentage
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$companyPickup, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(companyPickup),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }else{
      nrow(Data[Data$Dropoff_Community_Area == i,])/nrow(Data)
      companyData <- Data[Data$Company == input$company,]
      companyDropoffPercentage <- c()
      for(i in unique(myspdf.df$Pickup_Community_Area)){
        companyDropoffPercentage <- append(companyDropoffPercentage, (nrow(companyData[companyData$Dropoff_Community_Area == i,])/nrow(companyData)) * 100)
      }
      myspdf$companyDropoff <- companyDropoffPercentage
      myspdf.df$companyDropoff <- companyDropoffPercentage
      
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$companyDropoff, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(companyDropoff),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }
  })
  
  
  
  
  
  output$map4 <- renderLeaflet({
    if(input$pickordrop == "Pickup"){
      nrow(Data[Data$Pickup_Community_Area == i,])/nrow(Data)
      companyData <- Data[Data$Company == input$company,]
      companyPickupPercentage <- c()
      for(i in unique(myspdf.df$Pickup_Community_Area)){
        companyPickupPercentage <- append(companyPickupPercentage, (nrow(companyData[companyData$Pickup_Community_Area == i,])/nrow(companyData)) * 100)
      }
      myspdf$companyPickup <- companyPickupPercentage
      myspdf.df$companyPickup <- companyPickupPercentage
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$companyPickup, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(companyPickup),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }else{
      nrow(Data[Data$Dropoff_Community_Area == i,])/nrow(Data)
      companyData <- Data[Data$Company == input$company,]
      companyDropoffPercentage <- c()
      for(i in unique(myspdf.df$Pickup_Community_Area)){
        companyDropoffPercentage <- append(companyDropoffPercentage, (nrow(companyData[companyData$Dropoff_Community_Area == i,])/nrow(companyData)) * 100)
      }
      myspdf$companyDropoff <- companyDropoffPercentage
      myspdf.df$companyDropoff <- companyDropoffPercentage
      
      p <- quantile(myspdf.df$pickup, probs = seq(0, 1.0, by = .15), names = FALSE)
      bins <- c(as.numeric(p[1]), as.numeric(p[2]), as.numeric(p[3]), as.numeric(p[4]), as.numeric(p[5]), as.numeric(p[6]), as.numeric(p[7]), max(myspdf.df$pickup)+1)
      pal <- colorBin("YlOrRd", domain = myspdf$pickup, bins = bins)
      leaflet(myspdf) %>% addTiles() %>%
        setView(-87.63245, 41.88425, zoom = 10) %>%
        addLegend(pal = pal, values = ~myspdf$companyDropoff, opacity = 0.7, title = NULL,
                  position = "topright")%>%
        addPolygons(
          fillColor = ~pal(companyDropoff),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE,
          ),
          popup = paste(myspdf.df$pickup, "%"),
          layerId = ~myspdf$area_numbe,
          label=~community
        )
    }
  })
  
  observeEvent(input$map1_shape_click, {
    event <- input$map1_shape_click
    rv1$myDf <- data.frame(id = event$id)
    d <- Data[Data$Pickup_Community_Area == rv1$myDf$id,]
    if(input$pickordrop == "Pickup"){
      updateSelectizeInput(session, 'communityArea1', selected = d$community.x[1])
    }
    else{
      updateSelectizeInput(session, 'communityArea1', selected = d$community.y[1])
    }
  })
  
  observeEvent(input$map3_shape_click, {
    event <- input$map3_shape_click
    rv2$myDf <- data.frame(id = event$id)
    d <- Data[Data$Pickup_Community_Area == rv2$myDf$id,]
    if(input$pickordrop == "Pickup"){
      updateSelectizeInput(session, 'communityArea1', selected = d$community.x[1])
    }
    else{
      updateSelectizeInput(session, 'communityArea1', selected = d$community.y[1])
    }
  })
  
  observeEvent(input$map4_shape_click, {
    event <- input$map4_shape_click
    rv3$myDf <- data.frame(id = event$id)
    d <- Data[Data$Dropoff_Community_Area == rv3$myDf$id,]
    if(input$pickordrop == "Pickup"){
      updateSelectizeInput(session, 'communityArea1', selected = d$community.x[1])
    }
    else{
      updateSelectizeInput(session, 'communityArea1', selected = d$community.y[1])
    }
  })
  observeEvent(input$communityArea1,{
    d <- Data[Data$community.x == input$communityArea1,]
    tableDayOfTheWeekPickupMap <- as(count(weekdays(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableMonthOfTheYearPickupMap <- as(count(months(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableDayOfTheYearPickupMap <- as(count(date(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableHourOfTheDayPickupMap <- as(count(hour(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableTripMilesPickupMap <- as(count(d$Trip_Miles), "data.frame")
    tableTripSecondsPickupMap <- as(count(d$Trip_Seconds), "data.frame")
    
    output$dayOfTheWeekPickupMap <- renderPlot({
      ggplot(d, aes(dayOfTheWeek)) + geom_bar(fill="steelblue")
    })
    
    output$monthOfTheYearPickupMap <- renderPlot({
      ggplot(d, aes(monthOfYear)) + geom_bar(fill="steelblue")
    })
    
    output$dayOfTheYearPickupMap <- renderPlot({
      ggplot(d, aes(dayOfTheYear)) + geom_bar(fill="steelblue")
    })
    
    output$hourOfTheDayPickupMap <- renderPlot({
      if(input$time == "12-hour"){
        ggplot(d, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }else{
        ggplot(d, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }
    })
    
    output$tripSecondsPickupMap <- renderPlot({
      ggplot(d, aes(Trip_Seconds)) + geom_histogram()
    })
    
    output$tripMilesPickupMap <- renderPlot({
      if(input$morkm == "Miles"){
        ggplot(d, aes(Trip_Miles)) + geom_histogram()
      }
      else{
        ggplot(d, aes(Trip_Km)) + geom_histogram()
      }
    })
    output$dayOfTheWeekPickupTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheWeekPickupMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Day", "Rides")
      )
    })
    output$monthOfTheYearPickupTable <- renderDataTable({
      DT::datatable({
        tableMonthOfTheYearPickupMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Month", "Rides")
      )
    })
    
    output$dayOfTheYearPickupTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheYearPickupMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Rides")
      )
    })
    
    output$hourOfTheDayPickupTable <- renderDataTable({
      DT::datatable({
        tableHourOfTheDayPickupMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Hour", "Rides")
      )
    })
    
    output$tripMilesPickupTable <- renderDataTable({
      DT::datatable({
        tableTripMilesPickupMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Miles", "Rides")
      )
    })
    
    output$tripSecondsPickupTable <- renderDataTable({
      DT::datatable({
        tableTripSecondsPickupMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Seconds", "Rides")
      )
    })
    
    d <- Data[Data$community.y == input$communityArea1,]
    tableDayOfTheWeekDropoffMap <- as(count(weekdays(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableMonthOfTheYearDropoffMap <- as(count(months(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableDayOfTheYearDropoffMap <- as(count(date(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableHourOfTheDayDropoffMap <- as(count(hour(mdy_hms(d$Trip_Start_Timestamp))), "data.frame")
    tableTripMilesDropoffMap <- as(count(d$Trip_Miles), "data.frame")
    tableTripSecondsDropoffMap <- as(count(d$Trip_Seconds), "data.frame")
    
    output$dayOfTheWeekDropoffMap <- renderPlot({
      ggplot(d, aes(dayOfTheWeek)) + geom_bar(fill="steelblue")
    })
    output$dayOfTheYearDropoffMap <- renderPlot({
      ggplot(d, aes(dayOfTheYear)) + geom_bar(fill="steelblue")
    })
    output$hourOfTheDayDropoffMap <- renderPlot({
      if(input$time == "12-hour"){
        ggplot(d, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }else{
        ggplot(d, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }
    })
    output$monthOfTheYearDropoffMap <- renderPlot({
      ggplot(d, aes(monthOfYear)) + geom_bar(fill="steelblue")
    })
    
    output$tripSecondsDropoffMap <- renderPlot({
      ggplot(d, aes(Trip_Seconds)) + geom_histogram()
    })
    
    output$tripMilesDropoffMap <- renderPlot({
      if(input$morkm == "Miles"){
        ggplot(d, aes(Trip_Miles)) + geom_histogram()
      }
      else{
        ggplot(d, aes(Trip_Km)) + geom_histogram()
      }
    })
    
    
    
    output$dayOfTheWeekDropoffTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheWeekDropoffMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE,colnames = c("Day", "Rides")
      )
    })
    output$monthOfTheYearDropoffTable <- renderDataTable({
      DT::datatable({
        tableMonthOfTheYearDropoffMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Month", "Rides")
      )
    })
    
    output$dayOfTheYearDropoffTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheYearDropoffMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Rides")
      )
    })
    
    output$hourOfTheDayDropoffTable <- renderDataTable({
      DT::datatable({
        tableHourOfTheDayDropoffMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Hour", "Rides")
      )
    })
    
    output$tripMilesDropoffTable <- renderDataTable({
      DT::datatable({
        tableTripMilesDropoffMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Miles", "Rides")
      )
    })
    
    output$tripSecondsDropoffTable <- renderDataTable({
      DT::datatable({
        tableTripSecondsDropoffMap
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Seconds", "Rides")
      )
    })

    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    tableDayOfTheWeekPickupCompany <- as(count(weekdays(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableMonthOfTheYearPickupCompany <- as(count(months(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableDayOfTheYearPickupCompany <- as(count(date(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableHourOfTheDayPickupCompany <- as(count(hour(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableTripMilesPickupCompany <- as(count(d2$Trip_Miles), "data.frame")
    tableTripSecondsPickupCompany <- as(count(d2$Trip_Seconds), "data.frame")
    
    output$tripSecondsPickupCompanyTable <- renderDataTable({
      DT::datatable({
        tableTripSecondsPickupCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Seconds", "Rides")
      )
    })
    
    
    output$tripMilesPickupCompanyTable <- renderDataTable({
      DT::datatable({
        tableTripMilesPickupCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Miles", "Rides")
      )
    })
    
    
    output$hourOfTheDayPickupCompanyTable <- renderDataTable({
      DT::datatable({
        tableHourOfTheDayPickupCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Hour", "Rides")
      )
    })
    
    output$dayOfTheYearPickupCompanyTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheYearPickupCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Rides")
      )
    })
    
    output$dayOfTheWeekPickupCompanyTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheWeekPickupCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Day", "Rides")
      )
    })
    
    output$monthOfTheYearPickupCompanyTable <- renderDataTable({
      DT::datatable({
        tableMonthOfTheYearPickupCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Month", "Rides")
      )
    })
    
    output$companyDayOfTheWeekPickup <- renderPlot({
      ggplot(d2, aes(dayOfTheWeek)) + geom_bar(fill = "steelblue")
    })
    
    output$companyDayOfTheYearPickup <- renderPlot({
      ggplot(d2, aes(dayOfTheYear)) + geom_bar(fill = "steelblue")
    })
    
    output$companyHourOfTheDayPickup <- renderPlot({
      if(input$time == "12-hour"){
        ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }else{
        ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }
    })
    
    output$companyMonthOfTheYearPickup <- renderPlot({
      ggplot(d2, aes(monthOfYear)) + geom_bar(fill = "steelblue")
    })
    
    output$companyTripSecondsPickup <- renderPlot({
      ggplot(d2, aes(Trip_Seconds)) + geom_histogram()
    })
    
    output$companyTripMilesPickup <- renderPlot({
      if(input$morkm == "Miles"){
        ggplot(d2, aes(Trip_Miles)) + geom_histogram()
      }else{
        ggplot(d2, aes(Trip_Km)) + geom_histogram()
      }
    })
    
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    tableDayOfTheWeekDropoffCompany <- as(count(weekdays(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableMonthOfTheYearDropoffCompany <- as(count(months(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableDayOfTheYearDropoffCompany <- as(count(date(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableHourOfTheDayDropoffCompany <- as(count(hour(mdy_hms(d2$Trip_Start_Timestamp))), "data.frame")
    tableTripMilesDropoffCompany <- as(count(d2$Trip_Miles), "data.frame")
    tableTripSecondsDropoffCompany <- as(count(d2$Trip_Seconds), "data.frame")
    
    output$tripSecondsDropoffCompanyTable <- renderDataTable({
      print(tableTripSecondsDropoffCompany)
      DT::datatable({
        tableTripSecondsDropoffCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Seconds", "Rides")
      )
    })
    
    
    output$tripMilesDropoffCompanyTable <- renderDataTable({
      DT::datatable({
        tableTripMilesDropoffCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Miles", "Rides")
      )
    })
    
    
    output$hourOfTheDayDropoffCompanyTable <- renderDataTable({
      DT::datatable({
        tableHourOfTheDayDropoffCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE,colnames = c("Hour", "Rides")
      )
    })
    
    output$dayOfTheYearDropoffCompanyTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheYearDropoffCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Date", "Rides")
      )
    })
    
    output$dayOfTheWeekDropoffCompanyTable <- renderDataTable({
      DT::datatable({
        tableDayOfTheWeekDropoffCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Day", "Rides")
      )
    })
    
    output$monthOfTheYearDropoffCompanyTable <- renderDataTable({
      DT::datatable({
        tableMonthOfTheYearDropoffCompany
      },
      options = list(searching = FALSE, lengthChange = FALSE, pageLength = 5), rownames = FALSE, colnames = c("Month", "Rides")
      )
    })
   
    output$companyDayOfTheWeekDropoff <- renderPlot({
      d <- Data[Data$Company == input$company,]
      d2 <- d[d$community.y == input$communityArea1,]
      ggplot(d2, aes(dayOfTheWeek)) + geom_bar(fill = "steelblue")
    })
    output$companyDayOfTheYearDropoff <- renderPlot({
      d <- Data[Data$Company == input$company,]
      d2 <- d[d$community.y == input$communityArea1,]
      ggplot(d2, aes(dayOfTheYear)) + geom_bar(fill = "steelblue")
    })
    
    output$companyHourOfTheDayDropoff <- renderPlot({
      d <- Data[Data$Company == input$company,]
      d2 <- d[d$community.y == input$communityArea1,]
      if(input$time == "12-hour"){
        ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }else{
        ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
      }
    })
    
    output$companyMonthOfTheYearDropoff <- renderPlot({
      d <- Data[Data$Company == input$company,]
      d2 <- d[d$community.y == input$communityArea1,]
      ggplot(d2, aes(monthOfYear)) + geom_bar(fill = "steelblue")
    })
    output$companyTripSecondsDropoff <- renderPlot({
      d <- Data[Data$Company == input$company,]
      d2 <- d[d$community.y == input$communityArea1,]
      ggplot(d2, aes(Trip_Seconds)) + geom_histogram()
    })
    
    output$companyTripMilesDropoff <- renderPlot({
      d <- Data[Data$Company == input$company,]
      d2 <- d[d$community.y == input$communityArea1,]
      if(input$morkm == "Miles"){
        ggplot(d2, aes(Trip_Miles)) + geom_histogram()
      }else{
        ggplot(d2, aes(Trip_Km)) + geom_histogram()
      }
    })

  })


  output$companyDayOfTheWeekPickup <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    ggplot(d2, aes(dayOfTheWeek)) + geom_bar(fill = "steelblue")
  })
  
  output$companyDayOfTheYearPickup <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    ggplot(d2, aes(dayOfTheYear)) + geom_bar(fill = "steelblue")
  })
  
  output$companyHourOfTheDayPickup <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    if(input$time == "12-hour"){
      ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
    }else{
      ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
    }
  })
  
  output$companyMonthOfTheYearPickup <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    ggplot(d2, aes(monthOfYear)) + geom_bar(fill = "steelblue")
  })
  
  output$companyTripSecondsPickup <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    ggplot(d2, aes(Trip_Seconds)) + geom_histogram()
  })
  
  output$companyTripMilesPickup <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.x == input$communityArea1,]
    if(input$morkm == "Miles"){
      ggplot(d2, aes(Trip_Miles)) + geom_histogram()
    }else{
      ggplot(d2, aes(Trip_Km)) + geom_histogram()
    }
  })
  
  output$companyDayOfTheWeekDropoff <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    ggplot(d2, aes(dayOfTheWeek)) + geom_bar(fill = "steelblue")
  })
  output$companyDayOfTheYearDropoff <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    ggplot(d2, aes(dayOfTheYear)) + geom_bar(fill = "steelblue")
  })
  
  output$companyHourOfTheDayDropoff <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    if(input$time == "12-hour"){
      ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('12 AM', '1 AM', '2 AM', '3 AM', '4 AM', '5 AM', '6 AM', '7 AM', '8 AM', '9 AM', '10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM', '4 PM', '5 PM', '6 PM', '7 PM', '8 PM', '9 PM', '10 PM', '11 PM', '12 AM')) + theme(plot.title = element_text(hjust = 1, size = 10))
    }else{
      ggplot(d2, aes(hourOfDay)) + geom_bar(fill="steelblue") + scale_x_discrete(limits = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')) + theme(plot.title = element_text(hjust = 1, size = 10))
    }
  })
  
  output$companyMonthOfTheYearDropoff <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    ggplot(d2, aes(monthOfYear)) + geom_bar(fill = "steelblue")
  })
  output$companyTripSecondsDropoff <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    ggplot(d2, aes(Trip_Seconds)) + geom_histogram()
  })
  
  output$companyTripMilesDropoff <- renderPlot({
    d <- Data[Data$Company == input$company,]
    d2 <- d[d$community.y == input$communityArea1,]
    if(input$morkm == "Miles"){
      ggplot(d2, aes(Trip_Miles)) + geom_histogram()
    }else{
      ggplot(d2, aes(Trip_Km)) + geom_histogram()
    }
  })
  
  
  observeEvent(input$pickordrop,{
    if(input$pickordrop == "Pickup"){
      shinyjs::show("PickupCommunity")
      shinyjs::hide("DropoffCommunity")
      shinyjs::show("CompanyPickupCommunity")
      shinyjs::hide("CompanyDropoffCommunity")
    }else{
      shinyjs::show("DropoffCommunity")
      shinyjs::hide("PickupCommunity")
      shinyjs::show("CompanyDropoffCommunity")
      shinyjs::hide("CompanyPickupCommunity")
    }
    
  })
  
  
}

shinyApp(ui, server)