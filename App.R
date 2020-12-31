library(shiny)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(tidyr)
library(dplyr)
library(rsconnect)

# Retreive data

tweeters <- read.csv("./data/tweeters.csv")

## Create dimensions of data to be visualized

tweeters %<>% 
  mutate(Political = case_when(conservative == 1 ~ "Conservative",
                               liberal == 1 ~ "Liberal",
                               resist == 1 ~ "#resist",
                               maga == 1 ~ "#maga")) %>%
  mutate(Social = case_when(mother == 1 ~ "Mother/mom/mommy",
                            father == 1 ~ "Father/dad",
                            husband == 1 ~ "Husband",
                            wife == 1 ~ "Wife"))
# UI 

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("type", "Choose an 'identity dimension'", c("Social", "Political"))
  )
)

# Server

server <- function(input, output, session) {
  
  filteredData <- reactive({
    variable <- input$type
    tweeters$id <- tweeters[[variable]]
    filter(tweeters, !is.na(tweeters$id))
  })

  colorpal <- reactive({
    colorFactor(
      palette = c(
      "#16aa65", "#1ab1cd", "#ef9cbb","#a73d14", 
      "#003a9c", "#4d8dd4", "#9c0101", "#e70000"),
      levels = c(
        "Father/dad", "Husband", "Mother/mom/mommy", "Wife",
        "#maga", "Conservative", "#resist", "Liberal"))
  })
  
  output$map <- renderLeaflet({
    leaflet(tweeters) %>% addProviderTiles("Stamen.TonerLite") %>% #
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 1, weight = 8, color = ~pal(id),
                 fillColor = ~pal(id), 
                 opacity = 0.7, fillOpacity = 0.7
      )
  })
  
  observe({
    
    proxy <- leafletProxy("map", data = filteredData())
    proxy %>% clearControls()
    
    pal <- colorpal()
    proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~id, title = "Identity")
  })
}


shinyApp(ui, server)