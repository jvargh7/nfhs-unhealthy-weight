# S07	Population living in households with electricity (%)
# S08	Population living in households with an improved drinking-water source1 (%)
# S09	Population living in households that use an improved sanitation facility2 (%)


population_df <- readRDS(paste0(path_ecological_analysis,"/working/iahr74_population.RDS")) %>% 
  mutate(hv024 = factor(hv024,levels=c(1:36),labels=attr(hv024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = hmembers*hv005/(10^6),
         state = case_when(shdistri %in% c(494,495,496) ~ "daman and diu",
                           shdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(hv024))) 
india_population_indicators <- population_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09"))  %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

saveRDS(india_population_indicators,paste0(path_ecological_analysis,"/working/n4 india_population_indicators hmembers.RDS"))


# STATE ----------
population_indicators <- population_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09","state"))  %>%
  group_by(state) %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(population_indicators,paste0(path_ecological_analysis,"/working/n4 state_population_indicators hmembers.RDS"))

# REGION ----------

region_population_indicators <- population_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09","state","hv025"))  %>%
  group_by(state,hv025) %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(region_population_indicators,paste0(path_ecological_analysis,"/working/n4 region_population_indicators hmembers.RDS"))

# DISTRICT ------------
# https://r-survey.r-forge.r-project.org/survey/html/surveyoptions.html
options(survey.lonely.psu="adjust")
d_population_indicators <- population_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S07","S08","S09","shdistri")) %>% 
  group_by(shdistri) %>% 
  summarize_all(~survey_mean(.,vartype = "ci",na.rm=TRUE))

saveRDS(d_population_indicators,paste0(path_ecological_analysis,"/working/n4 district_population_indicators hmembers.RDS"))
