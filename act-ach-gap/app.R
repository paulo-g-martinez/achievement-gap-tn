library(shiny)
library("tidyverse")
library("magrittr")
library("zipcode")
library("leaflet")
library("maps")
library("rgdal")
library("gdata")

ui <- fluidPage(
  titlePanel(title = "ACT Achievement Gap in TN Districts"),
  #leafletOutput("mymap", width = "100%", height = "100%")
  leafletOutput("mymap"),
  plotOutput("mysctplt")
)

server <- function(input, output, session) {
  
  #load data
  #load("counties_tn.Rda", "unsd_2016_tn.Rda", "scsd_2016_tn.Rda")
  load("counties_tn.Rda")
  load("unsd_2016_tn.Rda")
  load("scsd_2016_tn.Rda")
  load("sctplt.Rda")
  
  ## output map #################
  output$mymap <- renderLeaflet({
    leaflet(counties_tn) %>% 
      #Base groups
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo") %>%
      # Overlay groups
      addPolygons(weight = 1, color = "black", opacity = 1,
                  fillColor = "grey", fillOpacity = 0.5, 
                  group = "County Outline", 
                  label = ~str_to_title(as.character(county)),
                  highlightOptions = highlightOptions(color = "blue", weight = 3,
                                                      bringToFront = T),
                  popup = ~str_to_title(as.character(county))
                  )%>% 
      addPolygons(data = unsd_2016_tn, weight = 2, color = "navy", opacity = 1,
                  fillColor = "blue", fillOpacity = 0.5,
                  group = "Unified School-Districts", 
                  label = ~str_to_title(as.character(NAME)),
                  highlightOptions = highlightOptions(color = "orange", weight = 3,
                                                      #sendToBack = T,
                                                      bringToFront = T),
                  popup = ~paste(NAME, "<br>",
                                 "ACT Composite: ", ACT_comp, "<br>",
                                 "Pct Econ Dsadv: ", pct_ED, "<br>",
                                 "Per Pupil Expenditure: $", per_pupil_expend)
      ) %>% 
      addPolygons(data = scsd_2016_tn, weight = 2, color = "navy", opacity = 1,
                  fillColor = "blue", fillOpacity = 0.5,
                  group = "Secondary School-Districts", 
                  label = ~str_to_title(as.character(NAME)),
                  highlightOptions = highlightOptions(color = "orange", weight = 3,
                                                      #sendToBack = T,
                                                      bringToFront = T),
                  popup = ~paste(NAME, "<br>",
                                 "ACT Composite: ", ACT_comp, "<br>",
                                 "Pct Econ Dsadv: ", pct_ED, "<br>",
                                 "Per Pupil Expenditure: $", per_pupil_expend)
      ) %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Positron", "Nat Geo"),
        overlayGroups = c("County Outline", "Unified School-Districts", "Secondary School-Districts"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      hideGroup(c("Unified School-Districts", "Secondary School-Districts"))
  })
  
  #output scatter
  output$mysctplt <- renderPlot({
    sctplt
  })
}

shinyApp(ui, server)