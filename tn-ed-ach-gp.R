# Load Packages
#---------------------------------
library("tidyverse")
library("plotly") #haven't used yet
library("zipcode")
library("leaflet")
library("maps") # using it to learn leaflet
library("rgdal") # for reading shape files .shp

# Load Data
#---------------------------------

  # load IRS tax return data, zip code granularity
  load("data/irs.Rda")
   # For now, I only need IRS 2013 tax return info.
    rm(irs11, irs12, irs14, irs15)

  # load zip code df
  data(zipcode)
  zipcodes04 <- zipcode %>% 
    dplyr::filter(state == "TN")
  rm(zipcode)

  # Table of school districts and thier corresponding academic and socioeconomic variables
    # This csv is aggregated from the data available for download at tn.gov/education/data
  ach_profile <- read.csv(file = "data/achievement_profile_data_with_CORE.csv", header = TRUE)
  names(ach_profile) <- c("district", 
                        "dst_name", 
                        "alg1",
                        "alg2",
                        "bio1",
                        "chem",
                        "ELA",
                        "eng1",
                        "eng2",
                        "eng3",
                        "math",
                        "science",
                        "enrollment",
                        "pct_black",
                        "pct_hispanic",
                        "pct_ntv_am",
                        "pct_EL",
                        "pct_SWD",
                        "pct_ED",
                        "per_pupil_expend",
                        "pct_BHN",
                        "ACT_comp",
                        "pct_chron_absent",
                        "pct_susp",
                        "pct_expel",
                        "pct_grad",
                        "pct_dropout",
                        "CORE_region")

# Read in crosswalk to match districts to counties
# grade ) schoool ) district ) county ) state
crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)

# Merge Data
#---------------------------------
ach_profile <- merge(ach_profile, crosswalk, by.x = "district", by.y = "District.Number", type="left", all.x = TRUE)
rm(crosswalk)

# GRANULARITY MATCHING
#---------------------------------

# Prepping TN counties shape file for polygons
tn_counties <- map_data("county") %>% 
  dplyr::filter(region == "tennessee") %>% 
  dplyr::rename(state = region, county = subregion)

# attempting a leaflet with polygons
mapCounties <- map("county", fill = T, plot = F)
m <- leaflet(mapCounties) %>% 
  addTiles() %>% 
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = F)

# attempting leaflet with school district polygons from US census
# https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html
shape_districts2016 <- readOGR("cb_2016_47_unsd_500k/cb_2016_47_unsd_500k.shp", GDAL1_integer64_policy = T)
leaflet(shape_districts2016) %>% 
  addPolygons(color = "blue", weight = 1, smoothFactor = 0.5, opacity = 1.0,
              fillOpacity = 0.5, 
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
             highlightOptions = 
                highlightOptions(color = "white", weight = 2, bringToFront = T))
#something seems off, I got few blanks but mostly county outlines

#attempting with secondarty education districts
sec_ed_dists <- readOGR("cb_2016_47_scsd_500k/cb_2016_47_scsd_500k.shp", GDAL1_integer64_policy = T)
leaflet(sec_ed_dists) %>% 
  addPolygons(color = "blue", weight = 1, smoothFactor = 0.5, opacity = 1.0,
              fillOpacity = 0.5, 
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = 
                highlightOptions(color = "white", weight = 2, bringToFront = T))
# having same issue, it seems the data is itself incomplete or something.
# or maybe I have somehow lost some data.
