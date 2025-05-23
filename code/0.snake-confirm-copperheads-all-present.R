#-------------------------
# Script to check that all the copperheads
# are in the inat snake data
# and there's nothing missing/wrong
#------------------------

#load in snake data
snakes <- read.csv("data/snakes/inat-snakes.csv") %>%
  mutate(year = substr(observed_on, start = 1, stop = 4)) #add in year information

#load in copperheads
copperheads <- read.csv("data/snakes/inat-copperheads.csv")

#confirm all copperheads in snake data
miss_copper <- anti_join(copperheads, snakes, by = "id") #about 3,801 copperhead observations that are NOT included.... where are they I wonder? let's exclude Texas because no cicadas in Texas, similarly we can exclude Florida
miss_copper <- miss_copper %>%
  mutate(txfl = str_detect(string = place_guess, pattern = "Texas|Florida|TX|FL")) %>%
  filter(txfl == FALSE) #great, that cuts it down by to 2522 obs

table(miss_copper$geoprivacy)
#ah, these are all records with obscured geoprivacy. Despite that do we have a lat/lon. The 41 observations with blank geoprivary (eg. not obscured or private), are below 30 latitude and not within cicada area anyway.
#for the other ones, we want them. NOT all the copperheads we want are within snakes. we'll see if this isn't fixed once we filter to cicada areas, I think I did download a bigger stretch of copperhead observations than I did snakes