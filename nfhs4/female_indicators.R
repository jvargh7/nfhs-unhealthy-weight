# S99	Women (age 15 and above) Blood sugar level - high (141-160 mg/dl)23 (%)
# S100	Women (age 15 and above) Blood sugar level - very high (>160 mg/dl)23 (%)
# S101	Women (age 15 and above) Blood sugar level - high or very high (>140 mg/dl) or taking medicine to control blood sugar level23 (%)
# S105	Women (age 15 and above) Mildly elevated blood pressure (Systolic 140-159 mm of Hg and/or Diastolic 90-99 mm of Hg) (%)
# S106	Women (age 15 and above) Moderately or severely elevated blood pressure (Systolic >= 160mm of Hg and/or Diastolic >= 100mm of Hg) (%)
# S107	Women (age 15 and above) Elevated blood pressure (Systolic >= 140 mm of Hg and/or Diastolic >= 90 mm of Hg) or taking medicine to control blood pressure (%)

require(srvyr)
female <- readRDS(paste0(path_ecological_analysis,"/working/iair74_clean.RDS"))

female_df <- female %>% 
  mutate(
    S14 = case_when(v133 %in% c(9:20) | v155 %in% c(1,2) ~ 1,
                    TRUE ~ 0),
    S16 = case_when(v133 %in% c(10:20) ~ 1,
                    v133 %in% c(0:9) ~ 0,
                    v133 >= 97 ~ NA_real_,
                    TRUE ~ NA_real_),
    
    S20 = case_when(
                    v511 == 99 ~ NA_real_,
                    v012 < 20 | v012 > 24 ~ NA_real_,
                    v012 %in% c(20:24) & is.na(v511) ~ 0,
                    v012 %in% c(20:24) & v511 %in% c(0:18) ~ 0,
                    v012 %in% c(20:24) & v511 %in% c(19:24) ~ 1,
                    TRUE ~ NA_real_
    ),

    S20_rev = case_when(
      v511 == 99 ~ NA_real_,
      v012 < 20 | v012 > 24 ~ NA_real_,
      v012 %in% c(20:24) & is.na(v511) ~ 1,
      v012 %in% c(20:24) & v511 %in% c(0:18) ~ 1,
      v012 %in% c(20:24) & v511 %in% c(19:24) ~ 0,
      TRUE ~ NA_real_
    ),
    
    S86 = case_when(v445 > 6000 ~ NA_real_,
                                  v445 < 1850 ~ 1,
                                  v445 >= 1850 ~ 0,
                                  TRUE ~ NA_real_),
    
    S88 = case_when(v445 > 6000 ~ NA_real_,
                           v445 >= 2500  ~ 1,
                           v445 < 2500 ~ 0,
                           TRUE ~ NA_real_),
    
    overweight = case_when(v445 > 6000 ~ NA_real_,
                                 v445 >= 2500 & v445 < 3000 ~ 1,
                                 v445 < 2500 | v445 >= 3000 ~ 0,
                                 TRUE ~ NA_real_),
    
    
    obese = case_when(v445 > 6000 ~ NA_real_,
                            v445 >= 3000 ~ 1,
                            v445 < 3000 ~ 0,
                            TRUE ~ NA_real_),

    S99 = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                         sb70 >= 140 & sb70 <= 160 ~ 1,
                         sb70 < 140 | sb70 > 160 ~ 0,
                         TRUE ~ NA_real_
  ),
  S100 = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                   sb70 > 160 ~ 1,
                   sb70 <= 160 ~ 0,
                   TRUE ~ NA_real_),
  
  S101 = case_when(s723ab == 1 ~ 1,
                   is.na(sb70) | sb70 > 498 ~ NA_real_,
                   sb70 >= 140 ~ 1,
                   sb70 < 140 ~ 0,
                   TRUE ~ NA_real_),
  
  S105 = case_when(is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                   f_sbp >= 140 & f_sbp < 160 ~ 1,
                   f_dbp >= 90 & f_dbp < 100 ~ 1,
                   f_sbp < 140 | f_sbp >= 160 ~ 0,
                   f_dbp < 90 | f_dbp >= 100 ~ 0,
                   TRUE ~ NA_real_
  ),
  S106 = case_when(is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                   f_sbp >= 160 ~ 1,
                   f_dbp >= 100  ~ 1,
                   f_sbp < 160  ~ 0,
                   f_dbp < 100 ~ 0,
                   TRUE ~ NA_real_
  ),
  S107 = case_when(sb19 == 1 ~ 1,
                   is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                   f_sbp >= 140 ~ 1,
                   f_dbp >= 90  ~ 1,
                   f_sbp < 140  ~ 0,
                   f_dbp < 90 ~ 0,
                   TRUE ~ NA_real_
  ),
  
  S128 = case_when(v463z == 1 ~ 0,
                   v463z == 0 ~ 1,
                   TRUE ~ NA_real_),
  S130 = case_when(s716 == 0 ~ 0,
                   s716 == 1 ~ 1,
                   TRUE ~ NA_real_)
  
  
  ) %>% 
  mutate(v024 = factor(v024,levels=c(1:36),labels=attr(female$v024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = v005/(10^6),
         state = case_when(sdistri %in% c(494,495,496) ~ "daman and diu",
                           sdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(v024))) 


india_female_cvd_indicators <- female_df %>% 
  dplyr::filter(v454 == 0) %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S86","S88","S99","S100","S101","S105","S106","S107")) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))


india_female_ses_indicators <- female_df %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S14","S16","S20","S20_rev","S128","S130")) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

bind_cols(india_female_cvd_indicators,
          india_female_ses_indicators) %>% 
  saveRDS(.,paste0(path_ecological_analysis,"/working/n4 india_female_indicators.RDS"))

# STATE SUMMARY ----------------
female_cvd_indicators <- female_df %>% 
  dplyr::filter(v454 == 0) %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S86","S88","S99","S100","S101","S105","S106","S107","state")) %>% 
  group_by(state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))


female_ses_indicators <- female_df %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S14","S16","S20","S20_rev","S128","S130","state")) %>% 
  group_by(state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

left_join(female_cvd_indicators,
          female_ses_indicators,
          by = "state") %>% 
saveRDS(.,paste0(path_ecological_analysis,"/working/n4 state_female_indicators.RDS"))


# REGION SUMMARY ----------------
region_female_cvd_indicators <- female_df %>% 
  dplyr::filter(v454 == 0) %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S86","S88","S99","S100","S101","S105","S106","S107","state","v025")) %>% 
  group_by(v025,state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))


region_female_ses_indicators <- female_df %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S14","S16","S20","S20_rev","S128","S130","state","v025")) %>% 
  group_by(v025,state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

left_join(region_female_cvd_indicators,
          region_female_ses_indicators,
          by = c("state","v025")) %>% 
  saveRDS(.,paste0(path_ecological_analysis,"/working/n4 region_female_indicators.RDS"))


# DISTRICT SUMMARY -------------

d_female_cvd_indicators <- female_df %>% 
  dplyr::filter(v454 == 0) %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S86","S88","S99","S100","S101","S105","S106","S107","sdistri")) %>% 
  group_by(sdistri) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))


d_female_ses_indicators <- female_df %>% 
  as_survey_design(ids = v001,strata = state,
                   weight = weight,
                   nest = FALSE,
                   variance = "YG",pps = "brewer",
                   variables = c("S14","S16","S20","S20_rev","S128","S130","sdistri")) %>% 
  group_by(sdistri) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))

left_join(d_female_cvd_indicators,
          d_female_ses_indicators,
          by = "sdistri") %>% 
  saveRDS(.,paste0(path_ecological_analysis,"/working/n4 district_female_indicators.RDS"))