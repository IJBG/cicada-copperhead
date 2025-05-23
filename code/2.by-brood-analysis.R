###################
#
# OK! Time to analyse this copperhead pattern :3
#
#
##################

library(dplyr)
library(stringr)

minimum_year = 2019
maximum_year = 2024

#read in data
snakes <- read.csv("data/cleaned_snake_obs_with_county+cicada.csv")

#--------------------
#add county/brood information
#--------------------
#filter all the snake observations (which have been already filtered to be within counties that have cicada broods emerge) to the counties that also meet the minimum copperhead requirements (within copperhead range OR copperhead observed there)
copperhead_counties <- read.csv("data/counties_meeting_copperhead_requirements.csv")

#now, let's also get brood information
emergence_years <- read.csv("data/cicada/cicada_emergence_years.csv") %>%
  #filter only to broods with emergences within our study years, 2019-2024
  filter(!is.na(emergence_2019_through_2024)) 

emergence_years$BROOD_NAME = str_replace(emergence_years$BROOD_NAME, " ", ".")

snakes <- snakes %>%
  filter(ST_CNTY_CODE %in% copperhead_counties$ST_CNTY_CODE) %>%
  #awesome. so now all these snake observations are ONLY in places where copperheads could reasonably be expected to be seen.
  #filter snakes to only counties with broods that had emergences within our study years
  filter(if_any(any_of(emergence_years$BROOD_NAME), ~ .x == 1)) %>%
  #now we can also remove those extraneous columns for the broods that didn't emerge 2019-2024
  select(id, observed_on, user_login, quality_grade, description, num_identification_agreements, num_identification_disagreements, captive_cultivated, scientific_name, common_name, taxon_id, round_lat, round_lon, year, ST_CNTY_CODE, any_of(emergence_years$BROOD_NAME)) %>%
  group_by(id) %>%
  mutate(n_broods = sum(Brood.VIII, Brood.IX, Brood.X, Brood.XIII, Brood.XIX)) %>%
  ungroup()
  #yay! beautiful!! The max number of broods is 2!!

#filter to year of, yr before
#add column for during emergence window

#summarize % copperhead by brood
#be sure to first double check the number of copperheads in the snakes df is correct, if not, rm all eastern copperheads and add_rows copper