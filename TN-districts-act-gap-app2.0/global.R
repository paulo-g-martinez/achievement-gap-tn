# Load Packages ---------------------------------
library("tidyverse")
library("magrittr")
library("zipcode")
library("leaflet")
library("maps")
library("rgdal")
library(sp) #for working with S4 classes
library("gdata") # read xls

# Load Data, Clean, and Merge ---------------------------------

# load TN counties (95 counties)
counties_tn <- map("county", 'tennessee', fill = T, plot = F)
# clean up name column
counties_tn$county <- str_replace(counties_tn$names, "tennessee,", "")
#save(counties_tn, file = "act-ach-gap/counties_tn.Rda")


#load TN unified-school districts for 2016. (districts with both elementary and secondary schools)
# (2016 is the first year these are available for from the U.S. Census Bureaug https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html, consulted, 02-27-2018
# note only 126 out of 145 districts are available
unsd_2016_tn <- readOGR("../data/cb_2016_47_unsd_500k/cb_2016_47_unsd_500k.shp",
                        verbose = T,
                        GDAL1_integer64_policy = T)
#load TN unified-school districts for 2016. (districts with both elementary and secondary schools)
# (2016 is the first year these are available for from the U.S. Census Bureaug https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html, consulted, 02-27-2018
# note only 16 available (combining for 142 out of 145 districts)
scsd_2016_tn <- readOGR("../data/cb_2016_47_scsd_500k/cb_2016_47_scsd_500k.shp",
                        verbose = T,
                        GDAL1_integer64_policy = T)


# load district academic and socioeconomic variables 2014-2015 school year
# This csv is aggregated from the data available for download at tn.gov/education/data
ach_profile_14_15 <- read.csv(file = "../data/achievement_profile_data_with_CORE.csv", header = TRUE)
names(ach_profile_14_15) <- c("dst_num", "dst_name", "alg1","alg2","bio1","chem","ELA","eng1","eng2","eng3","math","science","enrollment","pct_black","pct_hispanic","pct_ntv_am","pct_EL","pct_SWD","pct_ED","per_pupil_expend","pct_BHN","ACT_comp","pct_chron_absent","pct_susp","pct_expel","pct_grad","pct_dropout","CORE_region")
# manually group per pupil expenditure
ach_profile_14_15 %<>%
  dplyr::mutate(bracket = case_when(
    per_pupil_expend > 11000 ~ '1st third',
    per_pupil_expend > 9000 ~ '2nd third',
    per_pupil_expend < 9000 ~ '3rd third')
  )
# Read in crosswalk to match districts to counties
# grade ) schoool ) district ) county ) state
crosswalk <- read.xls("../data/data_district_to_county_crosswalk.xls", header = TRUE)
# Merge Data
ach_profile_14_15 <- merge(ach_profile_14_15, crosswalk, by.x = "dst_num", by.y = "District.Number", type = "left", all.x = TRUE)
rm(crosswalk)

#read in data_school_directory_2015 as crosswalk between tn identifiers and national identifiers
school.directory.tn.2015 <- read.xls("../data/data_schl_directory_2015.xlsx", header = T, na.strings = "--")
#filter and select down to the district level granularity
district.directory.tn.2015 <- school.directory.tn.2015 %>% 
  select(LEA_ID, LEA_NCES, LEA_ACCOUNTS) %>% 
  dplyr::group_by(LEA_ID) %>% 
  dplyr::distinct()

#join into ach_profile on district identifier
ach_profile_14_15 <- full_join(ach_profile_14_15, 
                               district.directory.tn.2015, 
                               by = c("dst_num" = "LEA_ID"))
ach_profile_14_15$LEA_NCES %<>% as.factor() 

#join matching ach rows into unsd_df
unsd_2016_tn@data <- left_join(unsd_2016_tn@data, ach_profile_14_15, 
                     by = c("GEOID" = "LEA_NCES"))
# copying polygon centroid coordinates into data for interactive filtering
unsd_2016_tn@data$longitude <- coordinates(unsd_2016_tn)[,1]
unsd_2016_tn@data$latitude <- coordinates(unsd_2016_tn)[,2]

# prep join variable
scsd_2016_tn@data$LEA_ACCOUNTS <- str_replace(scsd_2016_tn@data$NAME, ".* in ", "") %>% 
  str_trim() %>% 
  str_replace(" .*", "") %>% 
  str_replace("Franklin", "Franklin SSD") %>% 
  str_trim()
#join matching ach rows into scsd_df
scsd_2016_tn@data <- left_join(scsd_2016_tn@data, ach_profile_14_15, 
                     by = c("LEA_ACCOUNTS" = "LEA_ACCOUNTS"))
# copying polygon centroid coordinates into attribute table for interactive filtering
scsd_2016_tn@data$longitude <- coordinates(scsd_2016_tn)[,1]
scsd_2016_tn@data$latitude <- coordinates(scsd_2016_tn)[,2]

# Map data for introductory analysis -------------------
'mylflt <- leaflet(counties_tn) %>% 
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
  hideGroup(c("Unified School-Districts", "Secondary School-Districts"))'

'# Scatter Plot for interaction---------------
ach_profile_14_15 %<>%
  dplyr::mutate(bracket = case_when(
    per_pupil_expend > 11000 ~ "1st third",
    per_pupil_expend > 9000 ~ "2nd third",
    per_pupil_expend < 9000 ~ "3rd third")
  )
sctplt <- ggplot(ach_profile_14_15, aes(x = per_pupil_expend, y = ACT_comp)) +
  geom_point(aes(color = bracket)) +
  #geom_quantile() +
  #geom_smooth(method = lm) +
  geom_smooth(data = (ach_profile_14_15 %>% filter(bracket == "1st third")), method = lm) +
  geom_smooth(data = (ach_profile_14_15 %>% filter(bracket == "2nd third")), method = lm) +
  geom_smooth(data = (ach_profile_14_15 %>% filter(bracket == "3rd third")), method = lm)
save(sctplt, file = "act-ach-gap/sctplt.Rda")

# Correlation plots----------------
corr_ach <- ach_profile_14_15 %>%
  dplyr::select(-c(dst_num, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
matrix.ach <- GGally::ggcorr(corr_ach, label = T)

corr1 <- ach_profile_14_15 %>%
  dplyr::filter(bracket == "1st third") %>%
  dplyr::select(-c(dst_num, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr1, label = T)

corr2 <- ach_profile_14_15 %>%
  dplyr::filter(bracket == "2nd third") %>%
  dplyr::select(-c(dst_num, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr2, label = T)

corr3 <- ach_profile_14_15 %>%
  dplyr::filter(bracket == "3rd third") %>%
  dplyr::select(-c(dst_num, dst_name, CORE_region, County.Name, County.Number, bracket))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
GGally::ggcorr(corr3, label = T)'
