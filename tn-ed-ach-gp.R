# Load Packages ---------------------------------
library("tidyverse")
library("zipcode")
library("leaflet")
library("maps") # using it to learn leaflet
library("rgdal") # for reading shape files .shp
library("gdata") # read xls

# Load Data and Merge ---------------------------------

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
names(ach_profile) <- c("district", "dst_name", "alg1","alg2","bio1","chem","ELA","eng1","eng2","eng3","math","science","enrollment","pct_black","pct_hispanic","pct_ntv_am","pct_EL","pct_SWD","pct_ED","per_pupil_expend","pct_BHN","ACT_comp","pct_chron_absent","pct_susp","pct_expel","pct_grad","pct_dropout","CORE_region")

# Read in crosswalk to match districts to counties
# grade ) schoool ) district ) county ) state
crosswalk <- read.xls("./data/data_district_to_county_crosswalk.xls", header = TRUE)

# Merge Data
ach_profile <- merge(ach_profile, crosswalk, by.x = "district", by.y = "District.Number", type="left", all.x = TRUE)
rm(crosswalk)

# read in school attendance boundary shapes
#SABS <- readOGR("data/SABS_1314_SchoolLevels/SABS_1314_High.shp", verbose = T, GDAL1_integer64_policy = T)
# filter down to TN
#SABS_TN <- SABS[SABS$stAbbrev == "TN", ]
#rm(SABS)
#save(SABS_TN, file = "data/SABS_TN.Rda")
load("data/SABS_TN.Rda")

# transform projection for compatibiliyt
SABS_TN <- spTransform(SABS_TN, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# a leaflet with polygon data from an unplotted map-object
mapCounties <- map("county", 'tennessee', fill = T, plot = F)
mapCounties$county <- str_replace(mapCounties$names, "tennessee,", "")

# leaflet with school district polygons from US census bureau
# https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html
shape_districts2016 <- readOGR("cb_2016_47_unsd_500k/cb_2016_47_unsd_500k.shp",
                               verbose = T,
                               GDAL1_integer64_policy = T)

#There are two problems:
# 1) leaflet is making holes where there should be districts within districts (I could work around that)
# 2) the data is unhelpful since most school districts = county

# GRANULARITY MATCHING ---------------------------------

# combine shp files, and tiles into one leaflet
leaflet(mapCounties) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo") %>%
  # Overlay groups
  addPolygons(weight = 3, color = "navy", opacity = 1,
              fillColor = "red", fillOpacity = 0.5,
              group = "County Outline",
              label = ~as.character(county),
              highlightOptions = highlightOptions(color = "white", weight = 3,
                                                  bringToFront = T)) %>%
  #addPolygons(data = shape_districts2016,
   #           weight = 2, color = "white", opacity = 1,
    #          fillColor = "navy", fillOpacity = 0.5,
     #         group = "District Outline",
      #        label = ~as.character(NAME),
       #       highlightOptions = highlightOptions(color = "red", weight = 2,
        #                                          bringToFront = T)) %>%
  addPolygons(data = SABS_TN,
              weight = 1, color = "black", opacity = 1, 
              fillColor = "white", fillOpacity = 0.2,
              #fill = F,
              group = "Attendance Zone",
              label = ~as.character(schnam),
              highlightOptions = highlightOptions(color = "orange", weight = 4,
                                                  bringToFront = T, fillColor = "blue", 
                                                  sendToBack = T, fillOpacity = 0.5,
                                                  #pathOptions(clickable = T),
                                                  ), 
              popup = ~paste(schnam, "openEnroll: ", openEnroll)
              ) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Positron", "Nat Geo"),
    overlayGroups = c("County Outline", "District Outline", "Attendance Zone"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("District Outline", "Attendance Zone")) 

# example of selectable shapes
leaflet(mapCounties) %>% 
  addPolygons(layerId = ~county, group = ~county) %>% 
  addLayersControl(overlayGroups = ~county)

# EXPLORATORY ANALYSIS --------------------------------------
"ach_profile <- ach_profile %>%  #could move the ach_grouping up into Data Maniplation in Load and Merge
dplyr::mutate(bracket = case_when(
per_pupil_expend/max(per_pupil_expend, na.rm = T) < 0.25 ~ '1st quarter',
per_pupil_expend/max(per_pupil_expend, na.rm = T) >= 0.25 & per_pupil_expend/max(per_pupil_expend, na.rm = T) < 0.50 ~ '2nd quarter',
per_pupil_expend/max(per_pupil_expend, na.rm = T) >= 0.50 & per_pupil_expend/max(per_pupil_expend, na.rm = T) < 0.75 ~ '3nd quarter',
per_pupil_expend/max(per_pupil_expend, na.rm = T) >= 0.75 & per_pupil_expend/max(per_pupil_expend, na.rm = T) <= 1.0 ~ '4th quarter')
)"

ach_profile <- ach_profile %>%
  dplyr::mutate(bracket = case_when(
    per_pupil_expend > 11000 ~ '1st third',
    per_pupil_expend > 9000 ~ '2nd third',
    per_pupil_expend < 9000 ~ '3rd third')
  )
ggplot(ach_profile, aes(x = per_pupil_expend, y = ACT_comp)) +
  geom_point(aes(color = bracket)) +
  #geom_quantile() +
  #geom_smooth(method = lm) +
  geom_smooth(data = (ach_profile %>% filter(bracket == "1st third")), method = lm) +
  geom_smooth(data = (ach_profile %>% filter(bracket == "2nd third")), method = lm) +
  geom_smooth(data = (ach_profile %>% filter(bracket == "3rd third")), method = lm)

corr_ach <- ach_profile %>%
  dplyr::select(-c(district, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr_ach, label = T)

corr1 <- ach_profile %>%
  dplyr::filter(bracket == "1st third") %>%
  dplyr::select(-c(district, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr1, label = T)

corr2 <- ach_profile %>%
  dplyr::filter(bracket == "2nd third") %>%
  dplyr::select(-c(district, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr2, label = T)

corr3 <- ach_profile %>%
  dplyr::filter(bracket == "3rd third") %>%
  dplyr::select(-c(district, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr3, label = T)
