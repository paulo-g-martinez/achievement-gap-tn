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
      #addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo"
                       ) %>% 
      # Layers control
      addLayersControl(
        baseGroups = c(
          #"OSM (default)",
          "Positron", "Nat Geo"),
        overlayGroups = c("County Outline", "Unified School-Districts", "Secondary School-Districts (Exclusively High Sch.)"),
        options = layersControlOptions(collapsed = F)
      ) %>% 
      hideGroup(c("Unified School-Districts", "Secondary School-Districts (Exclusively High Sch.)")) %>% 
      
      setView(lng = -89.50, lat = 35.45, zoom = 7)
  })
  # A reactive expression that should return the set of data in the map bounds

  dataInBounds <- reactive({
    #if (is.null(input$mylflt_bounds))
     # return(counties_tn[FALSE,])
    if (is.null(input$mylflt_bounds))
      return(unsd_2016_tn@data[FALSE,])
    if(is.null(input$mylflt_bounds))
      return(scsd_2016_tn@data[FALSE,])
    bounds <- input$mylflt_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    #attempting to merge two subsetted data frames for interactive scatterplot
    dplyr::full_join(unsd_2016_tn@data, scsd_2016_tn@data) %>% 
      subset(latitude >= latRng[1] & latitude <= latRng[2] &
                      longitude >= lngRng[1] & longitude <= lngRng[2])
    
    #subseting shape files
    #unsd_2016_tn[unsd_2016_tn$NAME == 'Dyer County School District',]
    #subset(counties_tn,
     #      y >= latRng[1] & y <= latRng[2] &
      #       x >= lngRng[1] & x <= lngRng[2])
    #subset(unsd_2016_tn, 
     #      latitude >= latRng[1] & latitude <= latRng[2] &
      #       longitude >= lngRng[1] & longitude <= lngRng[2])
    #subset(scsd_2016_tn,
     #      latitude >= latRng[1] & latitude <= latRng[2] &
      #       longitude >= lngRng[1] & longitude <= lngRng[2])
    
  })
  
  #output$scatterACTcomp <- renderPlot({
    # If no polygon centroids are in view, don not plot
    #if (nrow(dataInBounds()) == 0)
     # return(NULL)
    
    #print(xyplot(ACT_comp ~ Dollars_Per_Pupil_Expend,
                   #input$x, 
                 #data = dataInBounds(), 
                 #data = unsd_2016_tn@data,
                 #xlim = range(unsd_2016_tn@data$Dollars_Per_Pupil_Expend), 
                 #ylim = range(unsd_2016_tn@data$ACT_comp)))
  #})
  
  output$trendPlot <- renderPlotly({
    
    #build graph with ggplot syntax
    p <- ggplot(dataInBounds(),#@data,
                aes_string(x = input$x, 
                           y = input$y,
                           a = 'NAME',
                           b = 'Pct_Econ_Disadv',
                           color = input$color)) +
      geom_point(alpha = 0.7) +
      geom_smooth(aes_string(x = input$x, y = input$y, color = input$color),
                  show.legend = TRUE, method = "lm", 
                  formula = y ~ x, inherit.aes = F)
            #geom_smooth(aes(x = input$x, y = input$y), method = "lm")
    
    ggplotly(p) %>% 
      layout(autosize = TRUE,
             legend = list(y = 0.7, x = 100, orientation = 'v')
             )
    
    })
  
  "output$trendPlot <- renderPlotly({
    #p <- 
    plot_ly(data = dataInBounds()@data, x = ~input$x, y = ~input$y, type = 'scatter',
                mode = 'markers', text = ~paste(NAME, '<\br>ACT compostie:', ACT_comp), 
                legend = list(y = 0.7, x = 100, orientation = 'v'))
    
  })"
  
  #This observer should maintain the polygons
  observe({
    
    leafletProxy("mylflt", 
                 data = unsd_2016_tn) %>% 
                 #data = dataInBounds()) %>% #DIDNT WORK
      clearShapes() %>% 
      addPolygons(
        #data = dataInBounds(),
        weight = 2, color = "navy", opacity = 1,
                  fillColor = "blue", fillOpacity = 0.5,
                  group = "Unified School-Districts", 
                  label = ~str_to_title(as.character(NAME)),
                  highlightOptions = highlightOptions(color = "orange", weight = 3,
                                                      #sendToBack = T,
                                                      bringToFront = T),
                  popup = ~paste("<b>Name: </b>", NAME,
                                 "<br><b>ACT Composite: </b>", ACT_Composite_Score,
                                 "<br><b>Pct Econ Dsadv: </b>", Pct_Econ_Disadv, 
                                 "<br><b>Per Pupil Expenditure: $<b/>", Dollars_Per_Pupil_Expend)
      ) %>% 
      addPolygons(data = scsd_2016_tn,
                  weight = 2, color = "navy", opacity = 1,
                  fillColor = "blue", fillOpacity = 0.5,
                  group = "Secondary School-Districts (Exclusively High Sch.)",
                  label = ~str_to_title(as.character(NAME)),
                  highlightOptions = highlightOptions(color = "orange", weight = 3,
                                                      bringToFront = T),
                  popup = ~paste("<b>Name: </b>", NAME,
                                 "<br><b>ACT Composite: </b>", ACT_Composite_Score,
                                 "<br><b>Pct Econ Dsadv: </b>", Pct_Econ_Disadv, 
                                 "<br><b>Per Pupil Expenditure: $<b/>", Dollars_Per_Pupil_Expend)
      ) %>% 
      addPolygons(data = counties_tn,
                  weight = 1, color = "black", opacity = 1,
                  fillColor = "grey", fillOpacity = 0.25,
                  group = "County Outline",
                  label = ~str_to_title(as.character(county)),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = T, sendToBack = T),
                  popup = ~str_to_title(as.character(county))
      )
  })
  
  output$table <- DT::renderDataTable({
    df <- dplyr::full_join(unsd_2016_tn@data, scsd_2016_tn@data) %>% 
      dplyr::filter(
        is.null(input$counties) | County.Name %in% input$counties,
        is.null(input$districts)| NAME %in% input$districts
      )
    #action <- DT::dataTableAjax(session, df)
    
    #DT::datatable(df, options = list(ajax = list(url = action)), escape = F)
  })
}