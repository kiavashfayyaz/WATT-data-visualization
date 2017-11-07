
# This is the user-interface definition of a Shiny web application.
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

shinyUI(fluidPage(
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js")
  ),
  leafletOutput("map" , height = 700),
  p(),
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
    width = 330, height = "auto",
    
    h2("WATT explorer"),
    h4("Please select a transit station from the map or the following list to see the WATT graph throughout the day"),
    selectInput("station No.", "Station No.", WATT[,1]),
    plotOutput("WATTplot", height = 200),
    tableOutput("Station Features")
  )
  ))
