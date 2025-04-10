####################
#
# Remove duplicate observations where it seems like the same individual snake was seen by multiple observers
# We'll do this by rounding lat/lons a bit 
# and any snakes within the same lat/lon + species + date but with different observers
# will be considered duplicates
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
  
  potential_duplicates <- df %>%
    mutate(round_lat = round(latitude, digits = 4),
           round_lon = round(longitude, digits = 4)) %>%
    group_by(taxon_id, observed_on, round_lat, round_lon) %>%
    dplyr::summarize(n = n(),
                     unique_observers = length(unique(user_name)))%>%
    filter(n > 1)
  #okay, so there's my table with for the lat/lon/dates with multiple copperheads, how MANY copperheads and how many unique observers
  
  #I could see an argument for rming any dates+lat+lon with any duplicates, bc we can't tell the difference between two copperheads and two unique observations. BUT, that's the conservative (removing all potential duplicates) rather than like, capturing how many coperheads were around. The answer could be multiple copperheads.
  
  #for the lat/lon/dates with more than one observer, we'll keep only one of the two rows. Probably, what, can use some fancy dplyr filter() work to do this?
  
}
