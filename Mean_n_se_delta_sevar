# DELTA CHANGE T - C #
df2 <- df1 %>% 
  group_by(Genotype, Treatment) %>% 
  summarise_each(funs(mean(., na.rm = T), 
                      n = sum(!is.na(.)), 
                      se = sd(., na.rm = T)/sqrt(sum(!is.na(.))),
                      var = var(.)), 
                 4:7) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate_at(vars(matches("_mean")), funs(.-.[Treatment == "Control"])) 

# calculate standard error of delta change
se <- geno %>% 
  group_by(Genotype, Treatment) %>% 
  summarise_each(funs(mean(., na.rm = T), 
                      n = sum(!is.na(.)), 
                      se = sd(., na.rm = T)/sqrt(sum(!is.na(.))),
                      var = var(.)), 
                 4:7) %>% 
  mutate_at(vars(matches("SFW_se")), funs(./(10))) %>% 
  mutate_if(is.numeric, round, 3)
  
se_var <- se %>%
  group_by(Genotype, Treatment) %>%
  summarise(SFW_sevar = SFW_var / SFW_n,
            PRL_sevar = PRL_var / PRL_n,
            LRN_sevar = LRN_var / LRN_n,
            LRD_sevar = LRD_var / LRD_n) %>% 
  mutate_at(vars(matches("_sevar")), funs(sqrt(.+ . [Treatment == "Control"]))) %>% 
  mutate_if(is.numeric, round, 4)

# stick se_var and delta_c together
final <- inner_join(delta_c, se_var, by = c("Genotype", "Treatment")) %>% 
  filter(Treatment == "Treatment1" | Treatment == "Treatment2") 
