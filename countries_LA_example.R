library(DemoKin)
library(tidyverse)
options(tibble.print_min=20)

# data fromwpp2022: https://population.un.org/wpp/Download/Standard/CSV/

# life table: https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Complete_Medium_Both_1950-2021.zip
lt_data <- data.table::fread("../DemoKin_tests/WPP2022_Life_Table_Complete_Medium_Female_1950-2021.csv")

# fertility: https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Fertility_by_Age1.zip
fert_data <- data.table::fread("../DemoKin_tests/WPP2022_Fertility_by_Age1.csv")

# pick countries
countries <- c("Argentina", "Guatemala", "Colombia")

# join lt and fert
data <- lt_data %>% 
  filter(Location %in% countries) %>% 
  select(Location, Time, age = AgeGrpStart, px) %>% 
  left_join(fert_data %>% 
              filter(Location %in% countries) %>% 
              select(Location, Time, age = AgeGrpStart, ASFR)) %>% 
  mutate(ASFR = replace(ASFR,is.na(ASFR),0))

# period data for decennial years
period_kin <- data %>%
  filter(Time %in% seq(1950, 2020, 10)) %>% 
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
    U <- X %>%
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
