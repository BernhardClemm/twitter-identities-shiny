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

tweeters <- tweeters %>% 
  mutate(Social = case_when(mother == 1 ~ "Mother/mom/mommy",
                            father == 1 ~ "Father/dad",
                            husband == 1 ~ "Husband",
                            wife == 1 ~ "Wife")) %>%
  mutate(Rightwing = case_when(conservative == 1 ~ "Conservative",
                              republican == 1 ~ "Republican",
                              trump == 1 ~ "#trump2020 (or variants)",
                              # american == 1 ~ "American",
                              maga == 1 ~ "#maga")) %>%
  mutate(Leftwing = case_when(democrat == 1 ~ "Democrat",
                               liberal == 1 ~ "Liberal",
                               resist == 1 ~ "#resist (or variants)",
                              blm == 1 ~ "#blm (or variants)",
                              biden == 1 ~ "#biden2020 (or variants)")) %>%
  mutate(Allpolitical = ifelse(
    is.na(Rightwing), Leftwing, Rightwing)) %>%
  mutate(Allpolitical = factor(Allpolitical, ordered = TRUE,
                               levels = c("Democrat", "Liberal", "#resist (or variants)", 
                                          "#blm (or variants)", "#biden2020 (or variants)",
                                          "Conservative", "Republican", 
                                          "#trump2020 (or variants)", "#maga")))

# UI 

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("type", "Choose an 'identity group'", c("Social", "Left-wing", "Right-wing", "All political"))
  )
)

# Server

server <- function(input, output, session) {
  
  filteredData <- reactive({
    variable <- input$type 
    variable <- gsub("-| ", "", variable)
    tweeters$id <- tweeters[[variable]]
    filter(tweeters, !is.na(tweeters$id))
  })

  colorpal <- reactive({
    if (input$type == "Left-wing") {
      colorFactor(palette = c("#58a0e6", "#0368fa", "#025de1", "#000081", "#061a61"), tweeters$id)
    } else if (input$type == "Right-wing") {
      colorFactor(palette = c("#f31111", "#a60505", "#882424", "#680000"), tweeters$id)
    } else if (input$type == "Social") {
      colorFactor(palette = "PRGn", tweeters$id)
    } else if (input$type == "All political") {
      colorFactor(palette = c("#58a0e6", "#0368fa", "#025de1", "#000081", "#061a61",
                              "#f31111", "#a60505", "#882424", "#680000"), tweeters$id)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(tweeters) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 7.5, weight = 7.5, color = ~pal(id),
                 fillColor = ~pal(id), fillOpacity = 0.4, opacity = 0.4
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