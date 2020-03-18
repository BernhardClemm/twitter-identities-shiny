library(shiny)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(tidyr)
library(dplyr)

# rsconnect::deployApp('/Users/bernhardclemm/Dropbox/PhD/Ideas/Identity Twitter/Test App')

# Retreive data

tweeters <- read.csv("./data/tweeters.csv")

## Create dimensions of data to be visualized

tweeters %<>% 
  mutate(Political = case_when(conservative == 1 ~ "conservative",
                               liberal == 1 ~ "liberal",
                               resist == 1 ~ "resist",
                               maga == 1 ~ "maga")) %>%
  mutate(Social = case_when(mother == 1 ~ "mother",
                            father == 1 ~ "father",
                            husband == 1 ~ "husband",
                            wife == 1 ~ "wife"))
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
    colorFactor(palette = "Dark2", tweeters$id)
  })
  
  output$map <- renderLeaflet({
    leaflet(tweeters) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 3, weight = 3, color = ~pal(id),
                 fillColor = ~pal(id), fillOpacity = 0.7 
      )
  })
  
  observe({
    
    proxy <- leafletProxy("map", data = filteredData())
    proxy %>% clearControls()
    
    pal <- colorpal()
    proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~id)
  })
}


shinyApp(ui, server)