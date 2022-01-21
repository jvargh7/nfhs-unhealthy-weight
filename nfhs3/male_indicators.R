# S102	Men (age 15 and above) Blood sugar level - high (141-160 mg/dl)23 (%)
# S103	Men (age 15 and above) Blood sugar level - very high (>160 mg/dl)23 (%)
# S104	Men (age 15 and above) Blood sugar level - high or very high (>140 mg/dl) or taking medicine to control blood sugar level23 (%)
# S108	Men (age 15 and above) Mildly elevated blood pressure (Systolic 140-159 mm of Hg and/or Diastolic 90-99 mm of Hg) (%)
# S109	Men (age 15 and above) Moderately or severely elevated blood pressure (Systolic >= 160mm of Hg and/or Diastolic >= 100mm of Hg) (%)
# S110	Men (age 15 and above) Elevated blood pressure (Systolic >= 140 mm of Hg and/or Diastolic >= 90 mm of Hg) or taking medicine to control blood pressure (%)

male <- readRDS(paste0(path_ecological_analysis,"/working/iamr52_clean.RDS"))

male_df <- male %>% 
  mutate(
    S15 = case_when(mv133 %in% c(9:20) | mv155 %in% c(1,2)~ 1,
                    TRUE ~ 0),
    S17 = case_when(mv133 %in% c(10:20) ~ 1,
                    mv133 %in% c(0:9) ~ 0,
                    mv133 >= 97 ~ NA_real_,
                    TRUE ~ NA_real_),
    S87 = case_when(mv445 > 6000 ~ NA_real_,
                    mv445 < 1850 ~ 1,
                    mv445 >= 1850 ~ 0,
                    TRUE ~ NA_real_),
    
    
    S89 = case_when(mv445 > 6000 ~ NA_real_,
                    mv445 >= 2500 ~ 1,
                    mv445 < 2500 ~ 0,
                    TRUE ~ NA_real_),
    
    overweight = case_when(mv445 > 6000 ~ NA_real_,
                           mv445 >= 2500 & mv445 < 3000 ~ 1,
                           mv445 < 2500 | mv445 >= 3000 ~ 0,
                           TRUE ~ NA_real_),
    
    obese = case_when(mv445 > 6000 ~ NA_real_,
                      mv445 >= 3000 ~ 1,
                      mv445 < 3000 ~ 0,
                      TRUE ~ NA_real_),
    S129 = case_when(mv463z == 1 ~ 0,
                     mv463z == 0 ~ 1,
                     TRUE ~ NA_real_),
    S131 = case_when(sm612 == 0 ~ 0,
                     sm612 == 1 ~ 1,
                     TRUE ~ NA_real_)
  ) %>% 
  mutate(mv024 = factor(mv024,levels=unique(mv024),
                        labels=attr(mv024,"labels") %>% 
                          attr(.,"names") %>% str_replace(.,"\\[[a-z]+\\]\\s",""))) %>% 
  mutate(weight = mv005/(10^6),
         state = mv024) 


# INDIA ------------
india_male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131"
                                 # "overweight","obese",
                                 # "S102","S103","S104","S108","S109","S110",
                                 )) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))


saveRDS(india_male_indicators,paste0(path_ecological_analysis,"/working/n3 india_male_indicators.RDS"))


# STATE ------------
male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131",
                                 # "overweight","obese",
                                 # "S102","S103","S104","S108","S109","S110",
                                 "state")) %>% 
  group_by(state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
  mutate(state = str_replace(state,"&","and"))


saveRDS(male_indicators,paste0(path_ecological_analysis,"/working/n3 state_male_indicators.RDS"))

# REGION -----------

region_male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131",
                                 # "overweight","obese",
                                 # "S102","S103","S104","S108","S109","S110",
                                 "state","mv025")) %>% 
  group_by(state,mv025) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
  mutate(state = str_replace(state,"&","and"))


saveRDS(region_male_indicators,paste0(path_ecological_analysis,"/working/n3 region_male_indicators.RDS"))

