# S10	Households using clean fuel for cooking3 (%)
# S12	Households with any usual member covered under a health insurance/financing scheme (%)

household <- readRDS(paste0(path_ecological_analysis,"/working/iahr52_extract.RDS"))


household_df <- household %>% 
  mutate(hv024 = factor(hv024,levels=unique(hv024),labels=attr(hv024,"labels") %>% 
                          attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s",""))) %>% 
  mutate(weight = hv005/(10^6),
         state = as.character(hv024))

# INDIA --------
india_household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12")) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(india_household_indicators,paste0(path_ecological_analysis,"/working/n3 india_household_indicators.RDS"))


# STATE --------
household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12","state")) %>% 
  group_by(state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(household_indicators,paste0(path_ecological_analysis,"/working/n3 state_household_indicators.RDS"))

# REGION -----------

region_household_indicators <- household_df %>% 
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S10","S12","state","hv025")) %>% 
  group_by(state,hv025) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(region_household_indicators,paste0(path_ecological_analysis,"/working/n3 region_household_indicators.RDS"))


