# S07	Population living in households with electricity (%)
# S08	Population living in households with an improved drinking-water source1 (%)
# S09	Population living in households that use an improved sanitation facility2 (%)


population_df <- readRDS(paste0(path_ecological_analysis,"/working/iahr52_population.RDS")) %>% 
  mutate(hv024 = factor(hv024,labels=attr(hv024,"labels") %>% 
                          attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s",""))) %>% 
  mutate(weight = hmembers*hv005/(10^6),
         state = as.character(hv024)) 

# INDAI ----------
india_population_indicators <- population_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09"))  %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(india_population_indicators,paste0(path_ecological_analysis,"/working/n3 india_population_indicators hmembers.RDS"))


# STATE ----------
population_indicators <- population_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09","state"))  %>%
  group_by(state) %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(population_indicators,paste0(path_ecological_analysis,"/working/n3 state_population_indicators hmembers.RDS"))

# REGION ----------

region_population_indicators <- population_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09","state","hv025"))  %>%
  group_by(state,hv025) %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(region_population_indicators,paste0(path_ecological_analysis,"/working/n3 region_population_indicators hmembers.RDS"))

