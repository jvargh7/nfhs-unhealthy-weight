# S81	Children under 5 years who are stunted (height-for-age)18 (%)
# S82	Children under 5 years who are wasted (weight-for-height)18 (%)
# S83	Children under 5 years who are severely wasted (weight-for-height)19 (%)
# S84	Children under 5 years who are underweight (weight-for-age)18 (%)
# S85	Children under 5 years who are overweight (weight-for-height)20 (%)
# S92	Children age 6-59 months who are anaemic (<11.0 g/dl)22 (%)


child_df <- readRDS(paste0(path_ecological_analysis,"/working/iapr52_child.RDS")) %>% 
  mutate(hv024 = factor(hv024,labels=attr(hv024,"labels") %>% 
                          attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s",""))) %>% 
  mutate(weight = hv005/(10^6),
         state = as.character(hv024))

india_child_indicators <- child_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S81","S82","S83","S84","S85","S92"))  %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(india_child_indicators,paste0(path_ecological_analysis,"/working/n3 india_child_indicators.RDS"))

# STATE ---------
child_indicators <- child_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S81","S82","S83","S84","S85","S92","state"))  %>%
  group_by(state) %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(child_indicators,paste0(path_ecological_analysis,"/working/n3 state_child_indicators.RDS"))

# REGION -----------
region_child_indicators <- child_df %>%
  as_survey_design(ids = hv001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S81","S82","S83","S84","S85","S92","state","hv025"))  %>%
  group_by(state,hv025) %>%
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(region_child_indicators,paste0(path_ecological_analysis,"/working/n3 region_child_indicators.RDS"))



