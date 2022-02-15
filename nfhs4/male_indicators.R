# S102	Men (age 15 and above) Blood sugar level - high (141-160 mg/dl)23 (%)
# S103	Men (age 15 and above) Blood sugar level - very high (>160 mg/dl)23 (%)
# S104	Men (age 15 and above) Blood sugar level - high or very high (>140 mg/dl) or taking medicine to control blood sugar level23 (%)
# S108	Men (age 15 and above) Mildly elevated blood pressure (Systolic 140-159 mm of Hg and/or Diastolic 90-99 mm of Hg) (%)
# S109	Men (age 15 and above) Moderately or severely elevated blood pressure (Systolic >= 160mm of Hg and/or Diastolic >= 100mm of Hg) (%)
# S110	Men (age 15 and above) Elevated blood pressure (Systolic >= 140 mm of Hg and/or Diastolic >= 90 mm of Hg) or taking medicine to control blood pressure (%)
require(srvyr)
male <- readRDS(paste0(path_ecological_analysis,"/working/iamr74_clean.RDS"))

male_df <- male %>% 
  dplyr::filter(mv012 %in% c(15:49)) %>% 
  mutate(
    S15 = case_when(mv133 %in% c(9:20) | mv155 %in% c(1,2)~ 1,
                    TRUE ~ 0),
    S17 = case_when(mv133 %in% c(10:20) ~ 1,
                    mv133 %in% c(0:9) ~ 0,
                    mv133 >= 97 ~ NA_real_,
                    TRUE ~ NA_real_),
    S87 = case_when(hb40 > 6000 ~ NA_real_,
                    hb40 < 1850 ~ 1,
                    hb40 >= 1850 ~ 0,
                    TRUE ~ NA_real_),
    
    
    S89 = case_when(hb40 > 6000 ~ NA_real_,
                    hb40 >= 2500 ~ 1,
                    hb40 < 2500 ~ 0,
                    TRUE ~ NA_real_),
    
    overweight = case_when(hb40 > 6000 ~ NA_real_,
                           hb40 >= 2500 & hb40 < 3000 ~ 1,
                           hb40 < 2500 | hb40 >= 3000 ~ 0,
                           TRUE ~ NA_real_),
    
    obese = case_when(hb40 > 6000 ~ NA_real_,
                      hb40 >= 3000 ~ 1,
                      hb40 < 3000 ~ 0,
                      TRUE ~ NA_real_),
    
    unhealthy_m = case_when(hb40 > 6000 ~ NA_real_,
                          hb40 < 1850 | hb40 >= 2500 ~ 1,
                          hb40 >= 1850 & hb40 < 2500 ~ 0,
                          TRUE ~ NA_real_),
    
    S102 = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                     smb70 >= 140 & smb70 <= 160 ~ 1,
                          smb70 < 140 | smb70 > 160 ~ 0,
                          TRUE ~ NA_real_ ),
         
         S103 = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                          smb70 > 160 ~ 1,
                          smb70 <= 160 ~ 0,
                          TRUE ~ NA_real_),
         
         S104 = case_when(sm622ab == 1 ~ 1,
                          is.na(smb70) | smb70 > 498 ~ NA_real_,
                          smb70 >= 140 ~ 1,
                          smb70 < 140 ~ 0,
                          TRUE ~ NA_real_),
         
         S108 = case_when(is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                          m_sbp >= 140 & m_sbp < 160 ~ 1,
                          m_dbp >= 90 & m_dbp < 100 ~ 1,
                          m_sbp < 140 | m_sbp >= 160 ~ 0,
                          m_dbp < 90 | m_dbp >= 100 ~ 0,
                          TRUE ~ NA_real_
         ),
         S109 = case_when(is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                          m_sbp >= 160 ~ 1,
                          m_dbp >= 100  ~ 1,
                          m_sbp < 160  ~ 0,
                          m_dbp < 100 ~ 0,
                          TRUE ~ NA_real_
         ),
         S110 = case_when(smb19 == 1 ~ 1,
                          is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                          m_sbp >= 140 ~ 1,
                          m_dbp >= 90  ~ 1,
                          m_sbp < 140  ~ 0,
                          m_dbp < 90 ~ 0,
                          TRUE ~ NA_real_
         ),
    S129 = case_when(mv463z == 1 ~ 0,
                     mv463z == 0 ~ 1,
                     TRUE ~ NA_real_),
    S131 = case_when(sm615 == 0 ~ 0,
                     sm615 == 1 ~ 1,
                     TRUE ~ NA_real_)
  ) %>% 
  mutate(mv024 = factor(mv024,levels=c(1:36),labels=attr(mv024,"labels") %>% attr(.,"names"))) %>% 
  mutate(weight = mv005/(10^6),
         state = case_when(smdistri %in% c(494,495,496) ~ "daman and diu",
                           smdistri %in% c(3,4) ~ "ladakh",
                           TRUE ~ as.character(mv024)),
         smdistri = as.numeric(smdistri)) 


india_male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131","unhealthy_m",
                                 # "overweight","obese",
                                 "S102","S103","S104","S108","S109","S110")) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(india_male_indicators,paste0(path_ecological_analysis,"/working/n4 india_male_indicators.RDS"))

# STATE ------------
male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131","unhealthy_m",
                                 # "overweight","obese",
                                 "S102","S103","S104","S108","S109","S110","state")) %>% 
  group_by(state) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
  mutate(state = str_replace(state,"&","and"))


saveRDS(male_indicators,paste0(path_ecological_analysis,"/working/n4 state_male_indicators.RDS"))

# REGION -----------

region_male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131","unhealthy_m",
                                 # "overweight","obese",
                                 "S102","S103","S104","S108","S109","S110","state","mv025")) %>% 
  group_by(state,mv025) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE)) %>% 
  mutate(state = str_replace(state,"&","and"))


saveRDS(region_male_indicators,paste0(path_ecological_analysis,"/working/n4 region_male_indicators.RDS"))

# DISTRICT ----------
district_male_indicators <- male_df %>% 
  as_survey_design(ids = mv001,strata = state,
                   weight = weight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer",
                   variables = c("S15","S17","S87","S89","S129","S131",
                                 # "overweight","obese",
                                 "S102","S103","S104","S108","S109","S110","state","smdistri")) %>% 
  group_by(state,smdistri) %>% 
  summarize_all(~survey_mean(.,vartype="ci",na.rm=TRUE))
saveRDS(district_male_indicators,paste0(path_ecological_analysis,"/working/n4 district_male_indicators.RDS"))
