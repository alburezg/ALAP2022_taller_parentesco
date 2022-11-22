X <- data %>% 
  filter(Location == "Argentina")

X              

data %>% filter(Location == "Argentina", Time == 1950, age == 0) 



px %>% filter(Location == "Argentina", TimeLabel == 1950, AgeStart == 15) %>% View
