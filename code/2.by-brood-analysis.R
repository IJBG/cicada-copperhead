###################
#
# OK! Time to analyse this copperhead pattern :3
#
#
##################

library(dplyr)
library(stringr)
library(lubridate)

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

snakes <- read.csv("data/cleaned_snake_obs_with_county+cicada.csv") %>%
  filter(ST_CNTY_CODE %in% copperhead_counties$ST_CNTY_CODE) %>%
  #awesome. so now all these snake observations are ONLY in places where copperheads could reasonably be expected to be seen.
  #filter snakes to only counties with broods that had emergences within our study years
  filter(if_any(any_of(emergence_years$BROOD_NAME), ~ .x == 1)) %>%
  #now we can also remove those extraneous columns for the broods that didn't emerge 2019-2024
  select(id, observed_on, user_login, quality_grade, description, num_identification_agreements, num_identification_disagreements, captive_cultivated, scientific_name, common_name, taxon_id, round_lat, round_lon, year, ST_CNTY_CODE, any_of(emergence_years$BROOD_NAME)) %>%
  group_by(id) %>%
  mutate(n_broods = sum(Brood.VIII, Brood.IX, Brood.X, Brood.XIII, Brood.XIX)) %>%
  ungroup() %>%
  #yay! beautiful!! The max number of broods is 2!!
  #107k observations with just 1 brood, 3070 observations with 2 broods, and confirmed there's no overlap problem where the overlap broods are only a year apart. See 'confirm_no_brood_timing_issues.csv' So we can keep those 3k two brood observations.
#add column for during emergence window, start of April through end of June [going based on iNat timing, https://www.inaturalist.org/taxa/83854-Magicicada]
  mutate(yday = yday(observed_on)) %>%
  mutate(apr_thru_jun = case_when(
    yday >= 91 & yday <= 182 ~ TRUE,
    TRUE ~ FALSE)) %>%
  #our measure of if copperheads increase in the yr of a cicada emergence or not is not going to be a window of the calendar year from Jan-Dec. This window should be from April 1 one year to May 31 the next year. That's the real measure of if copperheads experience like a fecundity benefit. So, we are going to assign a window_year for what April that observation is associated with. 
  mutate(window_year = case_when(
    #it's in or after the april window, gets the same year
    yday >= 91 ~ year,
    #it's before the start of april, associated with the potential emergence the yr before. This just affects JAN, FEB, and MAR
    yday < 91 ~ year-1
  )) %>%
  #okay yay. now let's add if this observation was actually during an emergence year.
  mutate(emergence_year = case_when(
    Brood.VIII == 1 & window_year == 2019 ~ 1,
    Brood.VIII == 1 & window_year == 2018 ~ -1,
    Brood.IX == 1 & window_year == 2020 ~ 1,
    Brood.IX == 1 & window_year == 2019 ~ -1,
    Brood.X == 1 & window_year == 2021 ~ 1,
    Brood.X == 1 & window_year == 2020 ~ -1,
    Brood.XIII == 1 & window_year == 2024 ~ 1, 
    Brood.XIII == 1 & window_year == 2023 ~ -1,
    Brood.XIX == 1 & window_year == 2024 ~ 1, 
    Brood.XIX == 1 & window_year == 2023 ~ -1,
    #catch all other cases
    TRUE ~ 0
  )) %>%
#could also use eg. (emergence_years$emergence_2019_through_2024[emergence_years$BROOD_NAME == "Brood.VIII"]-1) but that gets like. so complicated. so I've hard coded the brood years.
  #add associated brood
  mutate(associated_brood = case_when(
    Brood.VIII == 1 & window_year == 2019 ~ "Brood.VIII",
    Brood.VIII == 1 & window_year == 2018 ~ "Brood.VIII",
    Brood.IX == 1 & window_year == 2020 ~ "Brood.IX",
    Brood.IX == 1 & window_year == 2019 ~ "Brood.IX",
    Brood.X == 1 & window_year == 2021 ~ "Brood.X",
    Brood.X == 1 & window_year == 2020 ~ "Brood.X",
    Brood.XIII == 1 & Brood.XIX == 1 & window_year == 2024 ~ "Brood.XIII & Brood.XIX",
    Brood.XIII == 1 & Brood.XIX == 1 & window_year == 2023 ~ "Brood.XIII & Brood.XIX",
    Brood.XIII == 1 & window_year == 2024 ~ "Brood.XIII", 
    Brood.XIII == 1 & window_year == 2023 ~ "Brood.XIII",
    Brood.XIX == 1 & window_year == 2024 ~ "Brood.XIX", 
    Brood.XIX == 1 & window_year == 2023 ~ "Brood.XIX",
  ))

#summarize % copperhead by brood
#be sure to first double check the number of copperheads in the snakes df is correct, if not, rm all eastern copperheads and add_rows copper [we've already done this]

#---------------
# Analysis of during emergence window year of year before
#---------------
#summarize to % copperhead by brood.
aprjun_analysis <- snakes %>%
  filter(apr_thru_jun == TRUE,
         emergence_year != 0) %>%
  group_by(associated_brood, emergence_year) %>%
  summarize(total_observations = n(),
            n_copperheads = sum(scientific_name == "Agkistrodon contortrix"),
            perc_copper = (n_copperheads/total_observations)) %>%
  ungroup()
  
boxplot(perc_copper ~ emergence_year,
        data = aprjun_analysis,
        main = "During Emergence Window, 
        % Copperheads in Emergence vs 
        Non-emergence Years")

library(ggplot2)
ggplot(data = aprjun_analysis,
       aes(x = emergence_year, y = perc_copper,
           color = associated_brood)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

#summarize to % copperhead by brood only in counties where copperheads have EVER been seen on iNat.
aprjun_limcounties_analysis <- snakes %>%
  filter(apr_thru_jun == TRUE,
         emergence_year != 0,
         ST_CNTY_CODE %in% copperhead_counties$ST_CNTY_CODE[copperhead_counties$included_bc == "copperhead observations"]) %>%
  group_by(associated_brood, emergence_year) %>%
  summarize(total_observations = n(),
            n_copperheads = sum(scientific_name == "Agkistrodon contortrix"),
            perc_copper = (n_copperheads/total_observations)) %>%
  ungroup()


boxplot(perc_copper ~ emergence_year,
        data = aprjun_limcounties_analysis,
        main = "During Emergence Window, 
        % Copperheads in Emergence vs 
        Non-emergence Years
        CopperOBS counties")
ggplot(data = aprjun_limcounties_analysis,
       aes(x = emergence_year, y = perc_copper,
           color = associated_brood)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

#------------------
# Analysis of whole-year effect of cicada year of year before
#------------------

year_analysis <- snakes %>%
  filter(emergence_year != 0) %>%
  group_by(associated_brood, emergence_year) %>%
  summarize(total_observations = n(),
            n_copperheads = sum(scientific_name == "Agkistrodon contortrix"),
            perc_copper = (n_copperheads/total_observations)) %>%
  ungroup()

boxplot(perc_copper ~ emergence_year,
        data = year_analysis,
        main = "Year, 
        % Copperheads in Emergence vs 
        Non-emergence Years")
ggplot(data = year_analysis,
       aes(x = emergence_year, y = perc_copper,
           color = associated_brood)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

#if we limit the counties..
year_limcounties_analysis <- snakes %>%
  filter(emergence_year != 0,
         ST_CNTY_CODE %in% copperhead_counties$ST_CNTY_CODE[copperhead_counties$included_bc == "copperhead observations"]) %>%
  group_by(associated_brood, emergence_year) %>%
  summarize(total_observations = n(),
            n_copperheads = sum(scientific_name == "Agkistrodon contortrix"),
            perc_copper = (n_copperheads/total_observations)) %>%
  ungroup()

boxplot(perc_copper ~ emergence_year,
        data = year_limcounties_analysis,
        main = "Year, 
        % Copperheads in Emergence vs 
        Non-emergence Years
        CopperOBS counties")
ggplot(data = year_limcounties_analysis,
       aes(x = emergence_year, y = perc_copper,
           color = associated_brood)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

#Alright, with all that, I feel pretty confident saying that there's no effect of cicadas on how frequently people encounter copperheads / copperheads are not more active while hunting cicadas in cicada-years than the year before / no fecundity benefit boosting the number of copperheads seen in the year following a cicada emergence