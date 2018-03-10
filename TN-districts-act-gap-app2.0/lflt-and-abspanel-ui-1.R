# This script is based off of the 063-superzip-example on https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example, consulted Wed. Mar. 6th, 2018
library(leaflet)
library(plotly)

# Choices for drop-downs
vars <- c(
  "ACT Composite Score" = "ACT_comp",
  #"Income Bracket" = bracket
  "Per Pupil Expenditure" = "per_pupil_expend",
  "Percent Economically Disadvantaged" = "pct_ED"
  )
nms <- names(unsd_2016_tn)

navbarPage("ACT Gap TN", id = "nav",
           
           tabPanel("Interactive Districts Map",
                    div(class = "outer",
                        
                        tags$head(
                          #Include a custom CSS
                          includeCSS("www/styles.css"),
                          includeScript("www/gomap.js")
                        ),
                        
                        #If not using custom CSS, set height of leafletOutpu to a number instead of percent
                        leafletOutput("mylflt", width = "100%", height = "100%"),
                        #leafletOutput("mylflt", width = "100%", height = 600),
                        
                        #Shiny version prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
                                      draggable = TRUE, top = 150, left = 10, right = "auto", bottom = "auto",
                                      width = 800, height = "auto",
                                      
                                      h2("School District Explorer"),
                                      p("You can hover over the scatter plot tools to see what they do."),
                                      
                                      plotlyOutput('trendPlot'),
                                                   #height = "auto", width = "auto",
                                                   #height = 450, width = 550),
                                      
                                      p(""),
                                      p("Notice that as you zoom in and out of the map the scatter plot adjusts. (It only plots districts which are visible on the screen.) Below, you can select which two values you would like to explore the relationship of."),
                                      
                                      selectInput("y", "Vertical Axis", choices = c(nms, ""), selected = "ACT_comp"),
                                      p("Hint: Try comparing ACT scores on the vertical axis with Percent Economically Disadvantaged (pct_ED) on the horizontal one."),
                                      selectInput("x", "Horizontal Axis", choices = c(nms, ""), selected = "per_pupil_expend"),
                                      #selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "clarity"),
                                      #selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
                                      #sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                       #           min = 100, max = 2000, value = 1000),
                                      selectInput('color', "Color Coded By", choices = c(nms,""), selected = "STATEFP"),
                                      p("Here you can color-group the points by your input. For super granular filtering control, you can color-group them by district name and clikc-off particular points on the legend. (Although, with 146 districts, you might have to do some scrolling through the legend.)")
                                      
                                      #plotOutput("scatterACTcomp", height = 250)
                                      
                                    ),
                    
                        tags$div(id = "cite",
                                 'Data compiled from ', tags$em('title of piece'), 'by author of piece (publisher, daate).'
                        )
                      )
                    )
)
