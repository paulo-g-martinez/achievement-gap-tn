# This script is based off of the 063-superzip-example on https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example, consulted Wed. Mar. 6th, 2018
library(leaflet)

# Choices for drop-downs
vars <- c(
  "ACT Composite SCore" = "ACT_comp",
  "Per Pupil Expenditure" = "per_pupil_expend",
  "Percent Economically Disadvantaged" = "pcd_ED"
)

navbarPage("ACT Gap TN", id = "nav",
           
           tabPanel("Interactive Districts",
                    div(class = "outer",
                        
                        tags$head(
                          #Include a custom CSS
                          #includeCSS("styles.css"),
                          #includeScript("geomap.js")
                        ),
                        
                        #If not using custom CSS, set height of leafletOutpu to a number instead of percent
                        #leafletOutput("mylflt", width = "100%", height = "100%"),
                        leafletOutput("mylflt", width = "100%", height = 600),
                        
                        #Shiny version prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, TOP = 60, lefft = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("School District explorer"),
                                      
                                      #selectInput(""),
                                      #selectInput(),
                                      #conditionalPanel(
                                      #),
                                      
                                      plotOutput("scatterACTcomp", height = 250)
                                    ),
                        tags$div(id = "cite",
                                 'Data compiled from ', tags$em('title of piece'), 'by author of piece (publisher, daate).'
                        )
                      )
                    )
           )
