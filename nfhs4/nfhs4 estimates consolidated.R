
# district ----------
nfhs4_district_estimates <- readRDS(paste0(path_ecological_analysis,"/working/n4 district_female_indicators.RDS")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 district_male_indicators.RDS")) %>% 
              dplyr::select(-state),
            by=c("sdistri"="smdistri")) %>% 
  
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 district_child_indicators.RDS")),
            by=c("sdistri"="shdistri")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 district_population_indicators hmembers.RDS")) ,
            by=c("sdistri"="shdistri")) %>%
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 district_household_indicators.RDS")),
            by=c("sdistri"="shdistri")) 


nfhs4_district_estimates %>% 
  dplyr::select(-matches("(se|state)")) %>% 
  pivot_longer(cols=-sdistri,names_to="id",values_to="value") %>% 
  mutate(variable = "nfhs4d_total",
         value = value*100,
         est = case_when(str_detect(id,"_low") ~ "lci",
                         str_detect(id,"_upp") ~ "uci",
                         TRUE ~ "mean"),
         id = str_replace(id,"(_low|_upp)","")
         ) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  write_csv(.,"nfhs4 estimates/district long.csv")

# state ---------
nfhs4_state_estimates <- readRDS(paste0(path_ecological_analysis,"/working/n4 state_female_indicators.RDS")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 state_male_indicators.RDS")),
            by=c("state")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 state_child_indicators.RDS")),
            by=c("state")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 state_population_indicators hmembers.RDS")),
            by=c("state")) %>%
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 state_household_indicators.RDS")),
            by=c("state")) 

nfhs4_state_estimates %>% 
  pivot_longer(cols=-state,names_to="id",values_to="value") %>% 
  mutate(variable = "nfhs4s_total",
         value = value*100,
         est = case_when(str_detect(id,"_low") ~ "lci",
                         str_detect(id,"_upp") ~ "uci",
                         TRUE ~ "mean"),
         id = str_replace(id,"(_low|_upp)","")
  ) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  write_csv(.,"nfhs4 estimates/state long.csv")

# region ---------
nfhs4_region_estimates <- readRDS(paste0(path_ecological_analysis,"/working/n4 region_female_indicators.RDS")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 region_male_indicators.RDS")),
            by=c("state","v025"="mv025")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 region_child_indicators.RDS")),
            by=c("state","v025"="hv025")) %>% 
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 region_population_indicators hmembers.RDS")),
            by=c("state","v025"="hv025")) %>%
  left_join(readRDS(paste0(path_ecological_analysis,"/working/n4 region_household_indicators.RDS")),
            by=c("state","v025"="hv025")) %>% 
  dplyr::select(-matches("(low|upp|se|distri)")) 

nfhs4_region_estimates %>% 
  pivot_longer(cols=-one_of("state","v025"),names_to="id",values_to="value") %>% 
  mutate(variable = case_when(v025 == 1 ~ "nfhs4s_urban",
                              v025 == 2 ~ "nfhs4s_rural",
                              TRUE ~ NA_character_),
         value = value*100,
         est = case_when(str_detect(id,"_low") ~ "lci",
                         str_detect(id,"_upp") ~ "uci",
                         TRUE ~ "mean"),
         id = str_replace(id,"(_low|_upp)","")
  ) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  write_csv(.,"nfhs4 estimates/region long.csv")

# india -------
nfhs4_india_estimates <- readRDS(paste0(path_ecological_analysis,"/working/n4 india_female_indicators.RDS")) %>% 
  bind_cols(readRDS(paste0(path_ecological_analysis,"/working/n4 india_male_indicators.RDS")),
            readRDS(paste0(path_ecological_analysis,"/working/n4 india_child_indicators.RDS")),
            readRDS(paste0(path_ecological_analysis,"/working/n4 india_population_indicators hmembers.RDS")),
            readRDS(paste0(path_ecological_analysis,"/working/n4 india_household_indicators.RDS"))
            )
nfhs4_india_estimates %>% 
  pivot_longer(cols=everything(),names_to="id",values_to="value") %>% 
  mutate(variable = "nfhs4s_total",
         value = value*100,
         est = case_when(str_detect(id,"_low") ~ "lci",
                         str_detect(id,"_upp") ~ "uci",
                         TRUE ~ "mean"),
         id = str_replace(id,"(_low|_upp)","")
  ) %>% 
  pivot_wider(names_from=est,values_from=value) %>% 
  write_csv(.,"nfhs4 estimates/india long.csv")