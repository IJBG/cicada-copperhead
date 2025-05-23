####################
#
# Script to clean the snake data downloaded from inat and
# prep it for use
#
# Remove duplicate observations where it seems possible the same individual snake was seen by multiple observers
# We'll do this by rounding lat/lons to 4 digits (roughly 30-40 meters)
# and any snakes within the same lat/lon + species + date but with different observers
# will be considered duplicates
# We will keep only the first user's entry for these duplicates
#
# Remove observations with NA lat/lon
#
###################

library(dplyr)
library(stringr)

#load in all the inat data.
#if you have downloaded this from github fresh, you may need to unzip the inat-snakes file.
inat_snakes <- read.csv("data/snakes/inat-snakes.csv")
inat_copper <- read.csv("data/snakes/inat-copperheads.csv")

    #testing
    df = inat_copper

rm_inat_duplicates <- function(df) {
  
  #add round lat and lon
  df <- df %>%
    mutate(round_lat = round(latitude, digits = 4),
           round_lon = round(longitude, digits = 4)) 
  
  potential_duplicates <- df %>%
    group_by(taxon_id, observed_on, round_lat, round_lon) %>%
    dplyr::summarize(n = n(),
                     unique_observers = length(unique(user_login)),
                     first_username = first(unique(user_login)))%>%
    filter(n > 1) %>%
  #okay, so there's my table with for the lat/lon/dates with multiple identifications of the same species, how MANY of that species and how many unique observers
  #when there's 1 unique observer and two snakes, we can assume this represents two distinct snakes. So we can get rid of those. That leaves us only with locations where we have potential duplicate observations from 2+ observers. 
  filter(unique_observers > 1) %>%
  ungroup()
  #now, our table has a list of dates with duplicate species in the same small area, and one of the usernames of the observers. We can remove these observations from the starting df so we keep only 1 snake observation for the area. 

  #ah, I can't just anti-join, I do have to filter out and keep just the first, because of cases where eg. 10 snakes of the same taxon are seen by 10 different observers.
  #lmao, keep for posterity the first entry, anti_join the list, then add back in only the first entry for that species. #done
  keep_posterity = data.frame()
  for(i in 1:nrow(potential_duplicates)) {
    temp <- df %>%
      filter(user_login == potential_duplicates$first_username[i],
             observed_on == potential_duplicates$observed_on[i],
             taxon_id == potential_duplicates$taxon_id[i],
             round_lat == potential_duplicates$round_lat[i],
             round_lon == potential_duplicates$round_lon[i])
    
    keep_posterity = add_row(temp, keep_posterity)
  }
  
  df_no_duplicates <- df %>%
    #remove all potentially duplicated entries
    anti_join(potential_duplicates, 
              by = c("taxon_id", "observed_on", "round_lat", "round_lon")) 
    #check
    assertthat::assert_that(sum(potential_duplicates$n) == nrow(df) - nrow(df_no_duplicates))
    #continue, add back in just the first entry of the potentially duplicated ones, so we keep ONE record of that snake being there.
    df_no_duplicates <- add_row(df_no_duplicates, keep_posterity)
  
  #done! return df_no_duplicates
    df_no_duplicates
}

#function to remove NA lat/lon
rm_missing_coordinates <- function(df) {
  
  df <- df %>%
    filter(!is.na(latitude),
           !is.na(longitude))
  
}


copper <- inat_copper %>%
  rm_missing_coordinates() %>%
  rm_inat_duplicates() %>%
  dplyr::select(-created_at, -updated_at, -license, -url, -tag_list, -private_place_guess, -private_latitude, -private_longitude, -positioning_method, -positioning_device, -oauth_application_id)

snakes <- inat_snakes %>%
  rm_missing_coordinates() %>%
  rm_inat_duplicates() %>%
  dplyr::select(-user_id, -created_at, -updated_at, -license, -url, -tag_list, -private_place_guess, -private_latitude, -private_longitude, -positioning_method, -positioning_device, -image_url)


#Write cleaned datasets.
write.csv(copper, "data/snakes/cleaned_copperheads.csv")
write.csv(snakes, "data/snakes/cleaned_snakes.csv")
