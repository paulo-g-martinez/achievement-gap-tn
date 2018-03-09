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
           
           tabPanel("Interactive Districts",
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
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 600, height = "auto",
                                      
                                      h2("School District Explorer"),
                                      
                                      plotlyOutput('trendPlot', height = 450, width = 550),
                                      
                                      selectInput("x", "X axis", choices = nms, selected = "per_pupil_expend"),
                                      #selectInput("y", "Y axis", choices = vars, selected = "ACT Composite Score"),
                                      selectInput('color', "Color Coding", choices = nms, selected = "bracket")
                                      #selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "clarity"),
                                      #selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
                                      #sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                       #           min = 100, max = 2000, value = 1000),
                                      
                                      #plotOutput("scatterACTcomp", height = 250)
                                      
                                    ),
                    
                        tags$div(id = "cite",
                                 'Data compiled from ', tags$em('title of piece'), 'by author of piece (publisher, daate).'
                        )
                      )
                    )
)
