# Load Packages ---------------------------------
library("tidyverse")
library("magrittr")
library("zipcode")
library("leaflet")
library("maps") # using it to learn leaflet
library("rgdal") # for reading shape files .shp
library("gdata") # read xls

# Load Data, Clean, and Merge ---------------------------------

# load TN counties (95 counties)
counties_tn <- map("county", 'tennessee', fill = T, plot = F)
# clean up name column
counties_tn$county <- str_replace(counties_tn$names, "tennessee,", "")


#load TN unified-school districts for 2016. (districts with both elementary and secondary schools)
  # (2016 is the first year these are available for from the U.S. Census Bureaug https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html, consulted, 02-27-2018
    # note only 126 out of 145 districts are available
unsd_2016_tn <- readOGR("data/cb_2016_47_unsd_500k/cb_2016_47_unsd_500k.shp",
                               verbose = T,
                               GDAL1_integer64_policy = T)
#load TN unified-school districts for 2016. (districts with both elementary and secondary schools)
# (2016 is the first year these are available for from the U.S. Census Bureaug https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html, consulted, 02-27-2018
# note only 16 available (combining for 142 out of 145 districts)
scsd_2016_tn <- readOGR("data/cb_2016_47_scsd_500k/cb_2016_47_scsd_500k.shp",
                        verbose = T,
                        GDAL1_integer64_policy = T)


# load district academic and socioeconomic variables 2014-2015 school year
# This csv is aggregated from the data available for download at tn.gov/education/data
ach_profile_14_15 <- read.csv(file = "data/achievement_profile_data_with_CORE.csv", header = TRUE)
names(ach_profile_14_15) <- c("district", "dst_name", "alg1","alg2","bio1","chem","ELA","eng1","eng2","eng3","math","science","enrollment","pct_black","pct_hispanic","pct_ntv_am","pct_EL","pct_SWD","pct_ED","per_pupil_expend","pct_BHN","ACT_comp","pct_chron_absent","pct_susp","pct_expel","pct_grad","pct_dropout","CORE_region")
# Read in crosswalk to match districts to counties
# grade ) schoool ) district ) county ) state
crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)
# Merge Data
ach_profile_14_15 <- merge(ach_profile_14_15, crosswalk, by.x = "district", by.y = "District.Number", type = "left", all.x = TRUE)
rm(crosswalk)

#ach_profile_14_15 %<>% 
#  filter(!is.na(County.Name))

#define string cleaning function
str_cleaner <- function(s) {
  y <- str_trim(s, side = "both") %>% 
    str_to_lower() %>% 
    str_replace_all(" ", "-")
  return(y)
}

#ach_profile_14_15$County.Name <- map_chr(ach_profile_14_15$County.Name, str_cleaner)
#unsd_df$NAME %<>% map_chr(str_cleaner) 
  
# attempting merge of ach and scsd
scsd_df <- scsd_2016_tn@data
#scsd_df$NAME %<>% map_chr(str_cleaner) 
#scsd_df$UNSDLEA <- scsd_df$SCSDLEA # add variable for row bind

unsd_df <- unsd_2016_tn@data
#unsd_df$NAME %<>% map_chr(str_cleaner) 
#unsd_df$SCSDLEA <- unsd_df$UNSDLEA # add variable for row bind
#bind
#sd.df <- bind_rows(unsd_df, scsd_df)
#sd.df$County.Name <- str_extract(sd.df$NAME, ".*-county")

#read in data_school_directory_2015 as crosswalk between tn identifiers and national identifiers
school.directory.tn.2015 <- read.xls("data/data_schl_directory_2015.xlsx", header = T, na.strings = "--")
#filter and select down to the district level granularity
county.directory.tn.2015 <- school.directory.tn.2015 %>% 
  select(LEA_ID, LEA_NCES, LEA_ACCOUNTS) %>% 
  dplyr::group_by(LEA_ID) %>% 
  dplyr::distinct()

#join into ach_profile on district identifier
ach_profile_14_15 <- full_join(ach_profile_14_15, 
                               county.directory.tn.2015, 
                               by = c("district" = "LEA_ID"))
ach_profile_14_15$LEA_NCES %<>% as.factor() 

#join matching ach rows into unsd_df
unsd_df <- left_join(unsd_df, ach_profile_14_15, 
                     by = c("GEOID" = "LEA_NCES"))
#pump back into the shape file
unsd_2016_tn@data <- unsd_df

#join matching ach rows into scsd_df
# prep join variable
scsd_df$LEA_ACCOUNTS <- str_replace(scsd_df$NAME, ".* in ", "") %>% 
  str_trim() %>% 
  str_replace(" .*", "") %>% 
  str_replace("Franklin", "Franklin SSD") %>% 
  str_trim()
#join
scsd_df <- left_join(scsd_df, ach_profile_14_15, 
                      by = c("LEA_ACCOUNTS" = "LEA_ACCOUNTS"))
#pump back into the shape file
scsd_2016_tn@data <- scsd_df

# load IRS tax return data, zip code granularity
load("data/irs.Rda")
# For now, I only need IRS 2015 tax return info, because 2013 income funded 2014-2015 education
rm(irs11, irs12, irs14, irs15)

# Map data for introductory analysis -------------------
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
              ) %>% 
  addPolygons(data = unsd_2016_tn, weight = 2, color = "navy", opacity = 1,
              fillColor = "blue", fillOpacity = 0.5,
              group = "Unified School-Districts", 
              label = ~~str_to_title(as.character(NAME)),
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
              label = ~~str_to_title(as.character(NAME)),
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
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Unified School-Districts", "Secondary School-Districts"))
