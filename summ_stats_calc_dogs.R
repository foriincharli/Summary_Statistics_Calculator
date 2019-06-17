library(tidyverse)


df2 <- df1 %>% 
  filter(Breed == "French Bulldog") %>% 
  select(Coat_Colour, Weight, Height, Country_Origin, Size) %>% 
  group_by(Country_Origin, Size) %>% 
  summarise_each(funs(mean(., na.rm = T), 
                      n = sum(!is.na(.)), 
                      se = sd(., na.rm=T)/sqrt(sum(!is.na(.)))), 
                 1:5) %>% 
  mutate_if(is.numeric, round, 2)
