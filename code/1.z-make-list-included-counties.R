##############
#
# Make a list of counties that should be considered for our analysis
#we should only use counties where EITHER ONE of the two requirements are met:
#a) they are within the eastern copperhead range (which includes counties where they could be present but have never been observed)
#b) a copperhead has been observed there (which includes counties just outside the range map)
#
##############

library(sf)
library(tigris) #countyFP codes
options(tigris_use_cache = TRUE)
library(dplyr)

#read in data
snakes <- read.csv("data/cleaned_snake_obs_with_county+cicada.csv")

#read in county map
counties_sf <- tigris::counties() %>%
  dplyr::select(-CSAFP, -CBSAFP, -METDIVFP, -FUNCSTAT)

#read in range map
copperhead_range <- sf::read_sf("data/snakes/eastern_copperhead_range_inat.kml") %>%
  st_transform(crs = st_crs(counties_sf))

#extract counties that match my two requirements
a_counties <- counties_sf %>% 
  st_filter(copperhead_range, .predicate = st_intersects) %>%
  mutate(ST_CNTY_CODE = paste0(STATEFP, COUNTYFP)) %>%
  st_drop_geometry() %>%
  dplyr::select(ST_CNTY_CODE) %>%
  mutate(included_bc = "within copperhead range")

b_counties <- snakes %>%
  filter(common_name == "Eastern Copperhead") %>%
  distinct(ST_CNTY_CODE) %>%
  mutate(ST_CNTY_CODE = as.character(ST_CNTY_CODE),
         included_bc = "copperhead observations")


counties_to_include = dplyr::add_row(b_counties, a_counties) %>%
  distinct(ST_CNTY_CODE, .keep_all = TRUE)

write.csv(counties_to_include, "data/counties_meeting_copperhead_requirements.csv")
