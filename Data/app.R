library(shiny)
library(scales)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)  # Added for potential date handling

Antibiotics_Map_Server = readRDS("Antibiotics_Map_Server.rds")
Temp_Map_Server = readRDS("Temp_Map_Server.rds")

UI <- fluidPage(
  titlePanel("Antibiotic Doses per Capita by Health Board"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Year", "Select Year", 
                  choices = c(2023, 2024)),
      selectInput("Month", "Select Month", 
                  choices = c("June", "July", "August", "September", 
                              "October", "November", "December"))),
    mainPanel(
      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
      plotOutput("Antibiotic_Map", height = "1000px"), plotOutput("Min_Temp_Map", height = "1000px")
      )))
  )
)

Server <- function(input, output, session) {
  
  Filtered_Data_Antibiotics <- reactive({
    Antibiotics_Map_Server %>%
      filter(Year == input$Year,
             Month == input$Month)
  })
   
  Filtered_Data_Min_Temp <- reactive({
    Temp_Map_Server %>%
      filter(Year == input$Year,
             Month == input$Month)
  })
  
  output$Antibiotic_Map <- renderPlot({
    Antibiotics_Data = Filtered_Data_Antibiotics()
    ggplot(Antibiotics_Data, aes(fill = Dose_per_capita)) +
      geom_sf(color = "black", size = 0.5) +
      scale_fill_gradient2(
        low = "#313695",
        mid = "#F5E3FA",
        high = "#A50026",
        midpoint = 2.2,
        limits = c(1.2, 3.2),
        labels = label_comma()) +
      labs(
        title = paste("Antibiotic Doses per Capita —",
                      input$Month, input$Year),
        subtitle = "By Scottish Health Boards"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  })

  output$Min_Temp_Map = renderPlot({
    Data_Min_Temp = Filtered_Data_Min_Temp()
    ggplot(Data_Min_Temp, aes(fill = Min_Temperature)) +
      geom_sf(color = "black", size = 0.5) +
      scale_fill_gradient2(
        low = "#313695",
        mid = "#F5E3FA",
        high = "#A50026",
        midpoint = 5.5,
        limits = c(-0.75, 11.75),
        labels = label_number()) +
      labs(
        title = paste("Minimum Temperature —",
                      input$Month, input$Year),
        subtitle = "By Scottish Health Boards"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(UI, Server)
