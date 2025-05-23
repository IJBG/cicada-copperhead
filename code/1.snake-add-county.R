#############################
#
# Add county and brood data to each snake and copperhead observation
#
#############################

library(sf)
library(dplyr)
library(stringr)

#---------
# load in data
#---------

#load in the cleaned data
clean_snakes <- read.csv("data/snakes/cleaned_snakes.csv") %>%
  #add in year information
  mutate(year = substr(observed_on, start = 1, stop = 4))

copper <- read.csv("data/snakes/cleaned_copperheads.csv") %>%
  mutate(year = substr(observed_on, start = 1, stop = 4))

  #let's combine both the copper and snakes df into one for this task now. I know there are some copperheads that are NOT in the snakes df for probably data geoprivacy reasons, or perhaps bc they are out of the bounds of the snakes download, which I limited by cicada area a bit vs the copperhead download where I downloaded ALL observations without lat/lon limit. 

  snakes <- clean_snakes %>%
    #rm download-native copperheads
    filter(!common_name == "Eastern Copperhead") %>%
    #add in seperate-download copperheads, see the 0. examination of this problem for more details
    dplyr::add_row(copper)
  
  rm(clean_snakes);rm(copper)
    
#load in the cicada and county data
cicada <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb")

emergence_years <- read.csv("data/cicada/cicada_emergence_years.csv")

#---------
# add geometry
#---------

#add geom
snakes_geom <- st_as_sf(snakes, coords = c('longitude', 'latitude'), crs = st_crs(cicada))

#overlay the snake points on the cicada counties, and extract which ST_CNTY_CODE each snake observation is part of. Not all snakes will have a ST_CNTY_CODE and these represent snake points that lie outside the boundaries of cicada broods
snakes_county <- 
  st_join(snakes_geom, cicada, join = st_within) %>%
  filter(!is.na(STATEFP))
  #okay, now we can get rid of the geometry again so these are easier to work with. 

snakes_fin <- snakes_county %>%
  #drop geometry
  st_drop_geometry() %>%
  #okay, and now we've got duplicates to deal with. 
  mutate(present = 1) %>%
  tidyr::pivot_wider(id_cols = c(id, observed_on, user_login, quality_grade, description, num_identification_agreements, num_identification_disagreements, captive_cultivated, scientific_name, common_name, taxon_id, round_lat, round_lon, year, ST_CNTY_CODE, STATEFP, COUNTYFP, NAME),
                     names_from = BROOD_NAME,
                     values_from = present,
                     values_fill = 0)
  #sweet! now every observation has a record of county it's in and what broods are associated with that county. 
  #In the past here is where I summarized. However, I am going to avoid that and instead summarize data as late as possible, in case I end up changing some things around. So, we will end and save this here.

    write.csv(snakes_fin, "data/cleaned_snake_obs_with_county+cicada.csv")

