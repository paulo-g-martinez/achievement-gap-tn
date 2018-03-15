# This script is based off of the 063-superzip-example on https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example, consulted Wed. Mar. 6th, 2018
library(leaflet)
library(plotly)
library(DT)

# Choices for drop-downs
vars <- c(
  "ACT Composite Score" = "ACT_Composite_Score",
  "District Number" = "dst_num",
  "Alg 1" = "alg1", 
  "Alg 2" = "alg2",
  "Bio 1" = "bio1", 
  "Chemistry" = "chem",
  "Eng. Lang. Arts" = "ELA",
  "Enrollment" = "enrollment",
  "% Black" = "pct_black",
  "% Hispanic" = "pct_hispanic",
  "% Native American" = "pct_ntv_am",
  "% Black, Hisp. or Ntv. Am." = "pct_BHN",
  "% Eng. Lang. Learners" = "pct_EL",
  "% Students With Disability" = "pct_SWD",
  "% Economically Disadvantaged" = "pct_ED",
  "% Chronically Absent" = "pct_chron_absent",
  "% Suspended" = "pct_susp",
  "% Expelled" = "pct_expel",
  "% Dropouts" = "pct_dropout",
  "% Graduated" = "pct_grad",
  "CORE Region" = "CORE_region",
  "Bracket of PPE" = 'bracket',
  "Per Pupil Expenditure" = "Dollars_Per_Pupil_Expend",
  "County Number" = "County.Number",
  "County Name" = "County.Name",
  "District Name" = "NAME",
  "TN District" = "STATEFP"
  )
nms <- dplyr::full_join(unsd_2016_tn@data, scsd_2016_tn@data) %>% 
  dplyr::select(-UNSDLEA, -AFFGEOID, -GEOID, -LSAD, -ALAND, -AWATER, -longitude, -latitude, -SCSDLEA, -LEA_NCES, -Distr_Name, -LEA_ACCOUNTS, -STATEFP) %>% 
  names()

navbarPage("The Achievement Gap in TN", id = "nav",
           
           tabPanel("Interactive Publich School Districts Map",
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
                                      draggable = TRUE, top = 60, left = 37, right = "auto", bottom = "auto",
                                      width = 560, height = "auto",
                                      
                                      h4("Welcome to the Shiny Data Explorer!"), 
                                      h5("You can explore, organize, and filter data from TN's public school system."),
                                      h6("You can hover over the scatter plot tools to see what they do."),
                                      p("As you zoom in and out of the map the scatter plot adjusts. (It only plots districts which are visible on the screen.) Below, you can select which two values you would like to explore the relationship of."),
                                      h3("School District Data Explorer 2014-2015"),
                                      
                                      plotlyOutput('trendPlot'),
                                                   #height = "auto", width = "auto",
                                                   #height = 450, width = 550),
                                      
                                      h6(""),
                                      selectInput("y", "Vertical Axis", choices = c(nms, ""), selected = "ACT_Composite_Score"),
                                      p("Hint: Try comparing ACT scores on the vertical axis with Percent Economically Disadvantaged on the horizontal one."),
                                      selectInput("x", "Horizontal Axis", choices = c(nms, ""), selected = "Dollars_Per_Pupil_Expend"),
                                      #selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "clarity"),
                                      #selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
                                      #sliderInput('plotHeight', 'Height of plot (in pixels)', 
                                       #           min = 100, max = 2000, value = 1000),
                                      selectInput('color', "Color Coded By", choices = c(nms,""), selected = "State"),
                                      p("Here you can color-group the points by your input."), 
                                      p("Hint: Color-code the districkts by Expenditure-Bracket, to see how the districts that spend the least on their students compare to districts that spend the most on their students."),
                                      h6("For super granular filtering control, you can color-group them by district number and click-off particular points on the legend. (Although, with 146 districts, you might have to do some scrolling through the legend.)")
                                      
                                      #plotOutput("scatterACTcomp", height = 250)
                                      
                                    ),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
                                      draggable = TRUE, top = "auto", left = "auto", right = 2, bottom = -700,
                                      width = 650, height = "auto",
                                      
                                      h4("Level up the stats!"),
                                      p("This is a correlation matrix, It shows the correlative strength between all variables all at once."),
                                      p("(If it's too overwhelming you can just slide it out of the way, it won't hurt anything.)"),
                                      plotOutput("matrix", height = 600)
                                      
                        ),
                    
                        tags$div(id = "cite",
                                 'Data compiled from ', tags$em('tn.gov/education/data'), 'and from the U.S. Census Bureau (https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html, consulted, 02-27-2018).'
                        )
                      )
                    ),
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("counties", "Counties", c("All counties" = "", as.character(dplyr::full_join(unsd_2016_tn@data, scsd_2016_tn@data)$County.Name), "Anderson County" = "Anderson County"), multiple = TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.counties",
                                              selectInput("districts", "Districts", as.character(dplyr::full_join(unsd_2016_tn@data, scsd_2016_tn@data)$NAME), multiple = TRUE)
                             )
                      )#,
                      #column(3,
                       #      conditionalPanel("input.states",
                        #                      selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                         #    )
                      #)
                    #),
                    #fluidRow(
                    #  column(1,
                     #        numericInput("minScore", "Min score", min=0, max=100, value=0)
                      #),
                      #column(1,
                       #      numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      #)
                    ),
                    hr(),
                    DT::dataTableOutput('table')
           )#,
           
           #conditionalPanel("false", icon("crosshair"))           
           
)
