library(DemoKin)
library(tidyverse)
options(tibble.print_min=20)

# 1. Function to get UNWPP data -----

# A function to get DemoKin inputs from UNWPP using their API
# Inspired by and based on https://github.com/schmert/UN-API

get_UNWPP_inputs <- function(countries, my_startyr, my_endyr){
  
  
  print("Getting API ready...")
  # Get data from UN using API
  
  base_url <- 'https://population.un.org/dataportalapi/api/v1'
  
  # First, identify which indicator codes we want to use
  
  target <- paste0(base_url,'/indicators/?format=csv')
  codes <- read.csv(target, sep='|', skip=1) 
  
  qx_code <- codes$Id[codes$ShortName == "qx1"]
  asfr_code <- codes$Id[codes$ShortName == "ASFR1"]
  
  # Get location codes
  
  target <- paste0(base_url, '/locations?sort=id&format=csv')
  df_locations <- read.csv(target, sep='|', skip=1)
  
  # find the codes for countries
  
  my_location <- 
    df_locations %>% 
    filter( Name %in% countries) %>% 
    pull(Id) %>% 
    paste(collapse = ",")
  
  # Get px values
  
  print(paste0("Getting mortality data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- qx_code
  my_location  <- my_location
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  px <- 
    read.csv(target, sep='|', skip=1) %>% 
    filter(Sex == "Female") %>% 
    mutate(px = 1- Value) %>% 
    select(Location, Time = TimeLabel, age = AgeStart, px)
  
  # ASFR
  
  print(paste0("Getting fertility data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- asfr_code
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  asfr <- 
    read.csv(target, sep='|', skip=1) %>% 
    select(Location, Time = TimeLabel, age = AgeStart, ASFR = Value)
  
  data <- 
    left_join(px, asfr, by = c("Location", "Time", "age")) %>% 
    mutate(ASFR = replace(ASFR,is.na(ASFR),0)) 
  
  data
}

# 2. Get data ---------------

# pick countries
countries <- c("Argentina", "Guatemala", "Colombia")

# Year range

my_startyr   <- 1950
my_endyr     <- 2020

data <- get_UNWPP_inputs(
  countries = countries
  , my_startyr = my_startyr
  , my_endyr = my_endyr
  )

# 3. Analysis ----------

# period data for decennial years
period_kin <- 
  data %>%
  filter(Time %in% seq(1950, 2020, 10)) %>%
  # filter(Time %in% seq(1950, 2020, 20)) %>% 
  split(list(.$Location, .$Time)) %>%
  map_df(function(X){
    print(paste(unique(X$Location), unique(X$Time)))
    kin(X$px, X$ASFR/1000)$kin_summary %>%
      mutate(Location = unique(X$Location), 
             year = unique(X$Time), .before = 1)
  })

# comparison
period_kin %>% 
  filter(kin %in% c("m","d"), age_focal %in% c(15, 30, 60)) %>% 
  select(Location, kin, year, age_focal, count_living) %>% 
  ggplot(aes(year,count_living,color=Location)) +
  geom_line(size = 2) + 
  facet_grid(rows = vars(kin), cols = vars(age_focal),scales = "free_y") + theme_bw()

# 1950 cohort
cohort_kin <- data %>%
  split(list(.$Location)) %>%
  map_df(function(X){
    print(unique(X$Location))
    U <-
      X %>%
      select(Time, age, px) %>%
      pivot_wider(names_from = Time, values_from = px) %>%
      select(-age) %>% as.matrix()
    f <- X %>%
      select(Time, age, ASFR) %>%
      mutate(ASFR = ASFR/1000) %>% 
      pivot_wider(names_from = Time, values_from = ASFR) %>%
      select(-age) %>% as.matrix()
    kin(U, f, time_invariant = FALSE, output_kin = c("m","d"), output_cohort = 1950)$kin_summary %>%
      mutate(Location = unique(X$Location),  .before = 1)
  })

# comparison
cohort_kin %>% 
  select(Location, kin, cohort, age_focal, count_living) %>% 
  ggplot(aes(age_focal,count_living,color=Location)) +
  geom_line(size = 2) + 
  facet_grid(rows = vars(kin),scales = "free_y") + theme_bw()
