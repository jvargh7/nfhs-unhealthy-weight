# S10	Households using clean fuel for cooking3 (%)
# S12	Households with any usual member covered under a health insurance/financing scheme (%)

household <- readRDS(paste0(path_ecological_analysis,"/working/iahr74_extract.RDS"))


household_df <- household %>% 
  mutate(hv024 = factor(hv024,levels=c(1:36),labels=attr(household$hv024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = hv005/(10^6),
         state = case_when(shdistri %in% c(494,495,496) ~ "daman and diu",
                           shdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(hv024))) 

# NATIONAL --------
india_household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12")) %>% 
 summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

saveRDS(india_household_indicators,paste0(path_ecological_analysis,"/working/n4 india_household_indicators.RDS"))


# STATE --------
household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12","state")) %>% 
  group_by(state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(household_indicators,paste0(path_ecological_analysis,"/working/n4 state_household_indicators.RDS"))

# REGION -----------

region_household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12","state","hv025")) %>% 
  group_by(state,hv025) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(region_household_indicators,paste0(path_ecological_analysis,"/working/n4 region_household_indicators.RDS"))


# DISTRICT -----------
district_household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12","shdistri")) %>% 
  group_by(shdistri) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

saveRDS(district_household_indicators,paste0(path_ecological_analysis,"/working/n4 district_household_indicators.RDS"))