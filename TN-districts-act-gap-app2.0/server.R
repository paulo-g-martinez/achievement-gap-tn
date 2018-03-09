# This script is based off of the 063-superzip-example on https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example, consulted Wed. Mar. 6th, 2018

# load packages
library(leaflet)
library(RColorBrewer) # from template
library(scales) # from template
library(lattice) # from template
library(tidyverse)
library(sp)
library(rgeos) # for intersecting geometries
library(ggplot2)
library(plotly)

function(input, output, session) {
  ## Interactive Map ##############
  
  #Creating the map
  output$mylflt <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo"
                       ) %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Positron", "Nat Geo"),
        #overlayGroups = c("County Outline", "Unified School-Districts", "Secondary School-Districts"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      #hideGroup(c("Unified School-Districts", "Secondary School-Districts"))'
      
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  # A reactive expression that should return the set of data in the map bounds

  "dataInBounds <- reactive({
    'if (is.null(input$mylflt_bounds))
      return(counties_tn[FALSE,])'
    if (is.null(input$mylflt_bounds))
      return(unsd_2016_tn@data[FALSE,])
    'if(is.null(input$mylflt_bounds))
      return(scsd_2016_tn@data[FALSE,])'
    bounds <- input$mylflt_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    #subseting shape files
    'unsd_2016_tn[unsd_2016_tn$NAME == 'Dyer County School District',]'
    'subset(counties_tn,
           y >= latRng[1] & y <= latRng[2] &
             x >= lngRng[1] & x <= lngRng[2])'
    subset(unsd_2016_tn, 
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
    'subset(scsd_2016_tn,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])'
    
  })"
  
  #output$scatterACTcomp <- renderPlot({
    # If no polygon centroids are in view, don not plot
    #if (nrow(dataInBounds()) == 0)
     # return(NULL)
    
    #print(xyplot(ACT_comp ~ per_pupil_expend,
                   #input$x, 
                 #data = dataInBounds(), 
                 #data = unsd_2016_tn@data,
                 #xlim = range(unsd_2016_tn@data$per_pupil_expend), 
                 #ylim = range(unsd_2016_tn@data$ACT_comp)))
  #})
  
  output$trendPlot <- renderPlotly({
    
    #build graph with ggplot syntax
    p <- ggplot(unsd_2016_tn@data, aes_string(x = input$x, 
                                                color = input$color,
                                                y = 'ACT_comp')) +
                                                #y = input$y)) +
      geom_point()
    
    ggplotly(p) %>% 
      layout(autosize = TRUE, legend = list(x = 100, y = 0.5))
    
    })
  
  #This observer should maintain the polygons
  observe({
    
    leafletProxy("mylflt", data = unsd_2016_tn) %>% 
      clearShapes() %>% 
      addPolygons(weight = 2, color = "navy", opacity = 1,
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
      )
  })
}