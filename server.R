
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)
library(shiny)

WATT <- read_csv("WATT.csv")
population <- read.csv("population.csv", header = TRUE)
population <- WATT[,1:3] %>% left_join(population, by = "StationTime")
pal <- colorQuantile(c("red", "green"), domain = sort(population$Attractiveness))
Feature <- c("Average WATT", "Median WATT", "AMWR", "Adj. No. Jobs", "Closeness Cent.", "Degree Cent.")
Value <- c(0, 0, 0, 0, 0, 0)
des_dat <- data.frame(Feature,Value)
shinyServer(function(input, output) {

  data <- reactiveValues(clickedMarker=NULL)
  datas <- reactiveValues(selectedStation = NULL)
  # produce the basic leaflet map with single marker
  output$map <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>% 
      #addMarkers(lng = WATT$stop_lon, lat = WATT$stop_lat, popup = as.character(WATT$StationTime), layerId = WATT$StationTime) %>%
      #addCircles(lng = des_map$stop_lon, lat = des_map$stop_lat, color =  pal(des_map$Attractiveness), radius = 60, fillColor = pal(des_map$Attractiveness), fillOpacity = 0.5)
      addCircleMarkers(lng = population$stop_lon, lat = population$stop_lat, 
                       color =  pal(population$Attractiveness), 
                       radius = 10, 
                       fillColor = pal(population$Attractiveness), fillOpacity = 0.5, 
                       popup = as.character(population$StationTime), 
                       layerId = population$StationTime)
  )
  #observe the marker click info and print to console when it is changed.
  observeEvent(input$map_marker_click,{
    data$clickedMarker <- input$map_marker_click
    kia <- as.data.frame(t(WATT[WATT[,1] == data$clickedMarker$id, 4:107]))
    des_dat[1,2] <- mean(kia[,1]/60)
    des_dat[2,2] <- median(kia[,1]/60)
    des_dat[3,2] <- mean(kia[,1]/60)/median(kia[,1]/60)
    des_dat[4,2] <- population[population[,1] == data$clickedMarker$id, 4]
    des_dat[5,2] <- population[population[,1] == data$clickedMarker$id, 5]
    des_dat[6,2] <- population[population[,1] == data$clickedMarker$id, 6]
    output$WATTplot <- renderPlot(
    {
      ggplot(kia, aes(as.numeric(rownames(kia)), kia[,1]/60)) + 
        geom_line(size = 1) + 
        labs(title = paste0("Selected Station: ", as.character(data$clickedMarker$id))) +
        theme(axis.title.x = element_text(face="bold", size=15), axis.text.x  = element_text(face="bold", size=10)) + 
        xlab("Time of Day") + 
        theme(axis.title.y = element_text(face="bold", size=15), axis.text.y  = element_text(face="bold", size=5)) +
        ylab("WATT (minute)") 
    }
    )
    output$`Station Features` <- renderTable({
      return(des_dat)
    })
  })
  observeEvent(input$`station No.`,{
    datas$selectedStation <- input$`station No.`
    kia <- as.data.frame(t(WATT[WATT[,1] == datas$selectedStation, 4:107]))
    des_dat[1,2] <- mean(kia[,1]/60)
    des_dat[2,2] <- median(kia[,1]/60)
    des_dat[3,2] <- mean(kia[,1]/60)/median(kia[,1]/60)
    des_dat[4,2] <- population[population[,1] == datas$selectedStation, 4]
    des_dat[5,2] <- population[population[,1] == datas$selectedStation, 5]
    des_dat[6,2] <- population[population[,1] == datas$selectedStation, 6]
    output$WATTplot <- renderPlot(
      {
        ggplot(kia, aes(as.numeric(rownames(kia)), kia[,1]/60)) + 
          geom_line(size = 1) + 
          labs(title =paste0("Selected Station: ", as.character(datas$selectedStation))) +
          theme(axis.title.x = element_text(face="bold", size=15), axis.text.x  = element_text(face="bold", size=10)) + 
          xlab("Time of Day") + 
          theme(axis.title.y = element_text(face="bold", size=15), axis.text.y  = element_text(face="bold", size=5)) +
          ylab("WATT (minute)") 
      }
    )
    output$`Station Features` <- renderTable({
      return(des_dat)
    })
  })
})
