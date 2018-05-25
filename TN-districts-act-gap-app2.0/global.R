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
# (2016 is the first year these are available for from the U.S. Census Bureau https://www.census.gov/geo/maps-data/data/cbf/cbf_sd.html, consulted, 02-27-2018
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
ach_profile_14_15 <- read.csv(file = "data/dist_achievement_profile_data_with_CORE.csv", header = TRUE)
names(ach_profile_14_15) <- c("Distr_Num", "Distr_Name", "Alg1","Alg2","Bio1","Chem","Eng_Lang_Arts","Eng1","Eng2","Eng3","Math","Science","Enrollment","Pct_Black","Pct_Hispanic","Pct_Native_Amer","Pct_Eng_Language_Learner","Pct_Stud_w_Disability","Pct_Econ_Disadv","Dollars_Per_Pupil_Expend","Pct_Blk_Hisp_Ntv","ACT_Composite_Score","Pct_Chron_Absent","Pct_Suspended","Pct_Expelled","Pct_Graduated","Pct_Dropout","CORE_Region")
# manually group per pupil expenditure
ach_profile_14_15 %<>%
  dplyr::mutate(Dollars_Per_Pup_Exp_Bracket = case_when(
    Dollars_Per_Pupil_Expend > 11000 ~ 'Highest PPE',
    Dollars_Per_Pupil_Expend > 9000 ~ 'Middle PPE',
    Dollars_Per_Pupil_Expend < 9000 ~ 'Lowest PPE')
  )
# Read in crosswalk to match districts to counties
# grade ) schoool ) district ) county ) state
crosswalk <- read.xls("data/data_district_to_county_crosswalk.xls", header = TRUE)
# Merge Data
ach_profile_14_15 <- merge(ach_profile_14_15, crosswalk, by.x = "Distr_Num", by.y = "District.Number", type = "left", all.x = TRUE)
rm(crosswalk)

#read in data_school_directory_2015 as crosswalk between tn identifiers and national identifiers
school.directory.tn.2015 <- read.xls("data/data_schl_directory_2015.xlsx", header = T, na.strings = "--")
#filter and select down to the district level granularity
district.directory.tn.2015 <- school.directory.tn.2015 %>% 
  select(LEA_ID, LEA_NCES, LEA_ACCOUNTS) %>% 
  dplyr::group_by(LEA_ID) %>% 
  dplyr::distinct()

#join into ach_profile on district identifier
ach_profile_14_15 <- full_join(ach_profile_14_15, 
                               district.directory.tn.2015, 
                               by = c("Distr_Num" = "LEA_ID"))
ach_profile_14_15$LEA_NCES %<>% as.factor() 

#join matching ach rows into unsd_df
unsd_2016_tn@data <- left_join(unsd_2016_tn@data, ach_profile_14_15, 
                     by = c("GEOID" = "LEA_NCES")) %>% 
                      add_column(State = "TN")
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
                     by = c("LEA_ACCOUNTS" = "LEA_ACCOUNTS")) %>% 
  add_column(State = "TN")
# copying polygon centroid coordinates into attribute table for interactive filtering
scsd_2016_tn@data$longitude <- coordinates(scsd_2016_tn)[,1]
scsd_2016_tn@data$latitude <- coordinates(scsd_2016_tn)[,2]

# Compiling data at the school level granularity ------------------

# Read in school directory 2013-2014 as a crosswalk between SABS and TN identifiers
    sc.dir.13_14 <- read.xls("data/data_schl_directory_2014.xlsx") %>% 
      dplyr::select(-c(ST.Agency, Mailing.Add.3, Zip...4, Location.Add.3, City.1, State.1, Zip.1))
    # 0 pad SCH.NCES for paste with LEA.NCES
    sc.dir.13_14$SCH.NCES %<>% 
      as.character() %>% 
      str_pad(5, pad = "0")
    # prep for paste
    sc.dir.13_14$LEA.NCES %<>%
      as.character()
    # paste and trim
    sc.dir.13_14$ncessch <- paste(sc.dir.13_14$LEA.NCES, sc.dir.13_14$SCH.NCES) %>% 
      str_replace_all(" ", "")

# Read in school-level attendance boundary shapes for 2013 - 2014 school year
    load("data/SABS_TN_13_14.Rda") 
    # transform projection for compatibiliyt
    SABS_TN.13_14 <- spTransform(SABS_TN.13_14, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    
    # prep column for join
    SABS_TN.13_14@data$ncessch %<>% 
      as.character() %>% 
      str_replace_all(" ", "")
    # Left join into SABS 13_14 shapefile
    SABS_TN.13_14@data <- left_join(SABS_TN.13_14@data, sc.dir.13_14) %>% 
      rename(Tn.Id = X.Record.Id)
  
# Read in 2013-2014 school achievement for ACT scores
    sc.ach.13_14 <- read.xls("data/data_2014_school_achievement.xlsx") %>% 
      dplyr::select(-c(SCHOOL.YEAR, Math.Grade, Math.Trend, Reading.Grade, Reading.Trend, 
                       Social.Studies.Grade, Social.Studies.Trend, Science.Grade, Science.Trend, 
                       ACT.3yr.english, ACT.3yr.math, ACT.3yr.Reading, ACT.3yr.science, 
                       ACT.1yr.composite, ACT.1yr.english, ACT.1yr.math, ACT.1yr.Reading, 
                       ACT.1yr.science))

    # Left join it into SABS13_14
    SABS_TN.13_14@data <- left_join(SABS_TN.13_14@data, sc.ach.13_14, 
                              by = c("School.Name" = "school.name",
                                     "LEA.Id" = "DISTRICT.ID")) %>% 
                                select(-c(Math.3yr.Average.NCE, Social.Studies.3yr.Average.NCE, Science.3yravg.cur)) %>% 
                                rename(DISTRICT.ID = LEA.Id)

# Read in school level demographic profile data
    sc.prfl.13_14 <- read.xls("data/data_2014_school_profile.xlsx")

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
                             "ACT Composite: ", ACT_Composite_Score, "<br>",
                             "Pct Econ Dsadv: ", pct_ED, "<br>",
                             "Per Pupil Expenditure: $", Dollars_Per_Pupil_Expend)
  ) %>% 
  addPolygons(data = scsd_2016_tn, weight = 2, color = "navy", opacity = 1,
              fillColor = "blue", fillOpacity = 0.5,
              group = "Secondary School-Districts", 
              label = ~str_to_title(as.character(NAME)),
              highlightOptions = highlightOptions(color = "orange", weight = 3,
                                                  #sendToBack = T,
                                                  bringToFront = T),
              popup = ~paste(NAME, "<br>",
                             "ACT Composite: ", ACT_Composite_Score, "<br>",
                             "Pct Econ Dsadv: ", pct_ED, "<br>",
                             "Per Pupil Expenditure: $", Dollars_Per_Pupil_Expend)
  ) %>% 
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Positron", "Nat Geo"),
    overlayGroups = c("County Outline", "Unified School-Districts", "Secondary School-Districts"),
    options = layersControlOptions(collapsed = T)
  ) %>% 
  hideGroup(c("Unified School-Districts", "Secondary School-Districts"))'

# Scatter Plot for interaction---------------
'ach_profile_14_15 %<>%
  dplyr::mutate(bracket = case_when(
    Dollars_Per_Pupil_Expend > 11000 ~ "1st third",
    Dollars_Per_Pupil_Expend > 9000 ~ "2nd third",
    Dollars_Per_Pupil_Expend < 9000 ~ "3rd third")
  )
sctplt <- ggplot(ach_profile_14_15, aes(x = Dollars_Per_Pupil_Expend, y = ACT_Composite_Score)) +
  geom_point(aes(color = bracket)) +
  #geom_quantile() +
  #geom_smooth(method = lm) +
  geom_smooth(data = (ach_profile_14_15 %>% filter(bracket == "1st third")), method = lm) +
  geom_smooth(data = (ach_profile_14_15 %>% filter(bracket == "2nd third")), method = lm) +
  geom_smooth(data = (ach_profile_14_15 %>% filter(bracket == "3rd third")), method = lm)
save(sctplt, file = "act-ach-gap/sctplt.Rda")'

# Correlation plots----------------
'corr_ach <- ach_profile_14_15 %>%
  dplyr::select(-c(Distr_Num, Distr_Name, CORE_Region, County.Name, County.Number, Dollars_Per_Pup_Exp_Bracket, LEA_ACCOUNTS, LEA_NCES))
#ggcorrplot::ggcorrplot(corr)
#ggcorrplot::cor_pmat(corr)
matrix.ach <- GGally::ggcorr(corr_ach, label = T, layout.exp = 2, check_overlap = T, hjust = 1)'

