library(shiny)
library(leaflet)
library(readr)
library(RColorBrewer)

data <- read_csv("../2017_data.csv")

ui <- fluidPage(
  titlePanel("Explore Boston 311 Data for January 2017"),
  selectInput("et", "Subject:",
              c("Public Works Department" = "Public Works Department", 
                "Boston Police Department" = "Boston Police Department", 
                "Mayor's 24 Hour Hotline" = "Mayor's 24 Hour Hotline", 
                "Property Management" = "Property Management", 
                "Transportation - Traffic Division" = "Transportation - Traffic Division", 
                "Inspectional Services" = "Inspectional Services", 
                "Animal Control" = "Animal Control", 
                "Parks & Recreation Department" = "Parks & Recreation Department", 
                "Disability Department" = "Disability Department",
                "Boston Water & Sewer Commission" = "Boston Water & Sewer Commission", 
                "Civil Rights" = "Civil Rights", "Neighborhood Services" = "Neighborhood Services", 
                "Youthline" = "Youthline")),
  selectInput("bg", "Map Types:",
              c("Positron" = "CartoDB.Positron",
                "Open Street Map" = "OpenStreetMap")),
  leafletOutput(outputId="map")
)

server <- function(input, output) {
  pal <- colorFactor(topo.colors(2), data$OnTime_Status)
  output$map <- renderLeaflet({
    leaflet(data = subset(data, SUBJECT==input$et)) %>% addProviderTiles(input$bg) %>%
      addCircles(~LONGITUDE, ~LATITUDE, color = ~pal(OnTime_Status), popup = ~paste(OnTime_Status)) %>%
      addLegend("bottomright", pal = pal, values = ~OnTime_Status, title = "Status",opacity = 1)
  })
}

shinyApp(ui = ui, server = server)
