
library(shiny)
library(ggplot2) # we want to reduce our data load for our server, so we won't load the whole tidyverse - just what we need.
library(scales)
library(sf)
library(dplyr)

Final_data_server <- readRDS("Final_data_server.rds")
#You would normally expect here() to start at the project working directory (B229723, however due to the nature of Shiny.io (where we host this app), here() actually starts us at Antibiotics_Maps.


ui = fluidPage(
  titlePanel("Antibiotic Doses per Capita by Health Board"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Year", "Select Year", 
                  choices = c("2023", "2024")),
      selectInput("Month", "Select Month", 
                  choices = c("jun", "jul", "aug", "sep", "oct", "nov", "dec"))),
    mainPanel(
      plotOutput("antibiotic_map", height = "80vh", width = "100%")
    )))

server = function(input, output, session) {
  
  filtered_data = reactive({
    df = Final_data_server %>%
      filter(Year == as.numeric(input$Year),
             Month == input$Month)
  })

  
  output$antibiotic_map = renderPlot({
    ggplot(filtered_data(), aes(fill = Dose_per_capita)) +
      geom_sf(color = "white", size = 0.5)+
      scale_fill_continuous(
        low = "white",
        high = "darkblue",   
        limits = c(1.2, 3),
        labels = scales::label_comma())+
      labs(
        title = paste("Antibiotic Doses per Capita —",
                      toupper(input$Month), input$Year),
        subtitle = "Scottish Health Boards"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)