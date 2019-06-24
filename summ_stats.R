#### Generate some summary statistics ####
df2 <- df1 %>% 
  group_by(Year, State, Location) %>% 
  summarise_each(funs(mean(., na.rm = T), 
                      n = sum(!is.na(.)), 
                      se = sd(., na.rm = T)/sqrt(sum(!is.na(.)))), 
                 MinTemp:Rainfall)