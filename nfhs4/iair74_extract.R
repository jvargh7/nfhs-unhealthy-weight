
# S14	Women who are literate4 (%)

path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"


couples_variables <- readxl::read_excel(paste0(path_couples_folder,"/working/NFHS4 Couples Variable List.xlsx"),
                                        sheet = "iacr74 variables")

iair_extract <- read_dta(paste0(path_india_raw_data,"/IAIR74DT/IAIR74FL.dta"),
                         col_select = c(na.omit(couples_variables$female),"v106","v155","v511","v133"))

iair_clean <- iair_extract  %>% 
  # Blood pressure cleaning -------
mutate_at(vars(sb16s,sb16d,
               sb23s,sb23d,
               sb27s,sb27d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                                  TRUE ~ as.numeric(x))) %>% 
  mutate(f_hemo_anemia = case_when(v456 >= 250 | v456 < 3 ~ NA_real_,
                                   v454 == 1 & v456 < 110 ~ 1,
                                   v454 == 1 & v456 >= 110 ~ 0,
                                   v456 < 120 ~ 1,
                                   v456 >= 120 ~ 0,
                                   TRUE ~ NA_real_),
         f_bmi_underweight = case_when(v445 > 6000 ~ NA_real_,
                                       v445 < 1850 ~ 1,
                                       v445 >= 1850 ~ 0,
                                       TRUE ~ NA_real_),
         f_bmi_overweight = case_when(v445 > 6000 ~ NA_real_,
                                      v445 >= 2500 & v445 < 3000 ~ 1,
                                      v445 < 2500 | v445 >= 3000 ~ 0,
                                      TRUE ~ NA_real_),
         f_bmi_obese = case_when(v445 > 6000 ~ NA_real_,
                                 v445 >= 3000 ~ 1,
                                 v445 < 3000 ~ 0,
                                 TRUE ~ NA_real_),
         f_fasting = case_when(sb51 > 94 | sb52 > 94 ~ NA_real_,
                               sb51 > 8 & sb52 > 8 ~ 1,
                               sb51 <=8 | sb52 <= 8 ~ 0,
                               TRUE ~ NA_real_),
         f_ifg = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                           s723a == 1 ~ NA_real_, # Exclude those who have diabetes
                           f_fasting == 1 & sb70 >= 100 & sb70 < 126 ~ 1,
                           f_fasting == 1 & (sb70 < 100 | sb70 >= 126) ~ 0,
                           f_fasting == 0 ~ NA_real_,
                           TRUE ~ NA_real_),
         f_igt = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                           s723a == 1 ~ NA_real_, # Exclude those who have diabetes
                           f_fasting == 0 & sb70 >= 140 & sb70 < 200 ~ 1,
                           f_fasting == 0 & (sb70 < 140 | sb70 >= 200) ~ 0,
                           f_fasting == 1 ~ NA_real_,
                           TRUE ~ NA_real_),
         
         f_diagifg = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                               s723a == 1 & f_fasting == 1 & sb70 >= 100 & sb70 < 126 ~ 1,
                               s723a == 1 & f_fasting == 1 & (sb70 < 100 | sb70 >= 126) ~ 0,
                               s723a == 1 & f_fasting == 0 ~ NA_real_,
                               TRUE ~ NA_real_),
         f_diagigt = case_when(is.na(sb70) | sb70 > 498 ~ NA_real_,
                               s723a == 1 & f_fasting == 0 & sb70 >= 140 & sb70 < 200 ~ 1,
                               s723a == 1 & f_fasting == 0 & (sb70 < 140 | sb70 >= 200) ~ 0,
                               s723a == 1 & f_fasting == 1 ~ NA_real_,
                               TRUE ~ NA_real_),
         f_dm = case_when(s723a == 1 ~ 1,
                          is.na(sb70) | sb70 > 498 ~ NA_real_,
                          f_fasting == 1 & sb70 >= 126 ~ 1,
                          f_fasting == 0 & sb70 >= 200 ~ 1,
                          is.na(f_fasting) & sb70 >= 200 ~ 1,
                          f_fasting == 1 & sb70 < 126 ~ 0,
                          f_fasting == 0 & sb70 < 200 ~ 0,
                          is.na(f_fasting) & sb70 < 200 ~ 0,
                          TRUE  ~ NA_real_
         ),
         
         f_diagdm = case_when(
           is.na(sb70) | sb70 > 498 ~ NA_real_,
           s723a == 1 & f_fasting == 1 & sb70 >= 126 ~ 1,
           s723a == 1 & f_fasting == 0 & sb70 >= 200 ~ 1,
           s723a == 1 & is.na(f_fasting) & sb70 >= 200 ~ 1,
           s723a == 1 & f_fasting == 1 & sb70 < 126 ~ 0,
           s723a == 1 & f_fasting == 0 & sb70 < 200 ~ 0,
           s723a == 1 & is.na(f_fasting) & sb70 < 200 ~ 0,
           TRUE  ~ NA_real_
         ),
         
         f_sbp = rowMeans(.[,c("sb16s","sb23s","sb27s")],na.rm=TRUE),
         
         f_dbp = rowMeans(.[,c("sb16d","sb23d","sb27d")],na.rm=TRUE),
         
         f_prehtn = case_when(sb18 == 1 ~ NA_real_,
                              is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                              f_sbp >= 120 & f_sbp < 140 ~ 1,
                              f_dbp >= 80 & f_dbp < 90 ~ 1,
                              f_sbp < 120 | f_sbp >= 140 ~ 0,
                              f_dbp < 80 | f_dbp >= 90 ~ 0,
                              TRUE ~ NA_real_),
         f_htn = case_when(sb18 == 1 ~ 1,
                           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
                           f_sbp >= 140 ~ 1,
                           f_dbp >= 90 ~ 1,
                           f_sbp < 140 ~ 0,
                           f_dbp < 90 ~ 0,
                           TRUE ~ NA_real_),
         
         f_diagprehtn = case_when(
           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
           sb18 == 1 & f_sbp >= 120 & f_sbp < 140 ~ 1,
           sb18 == 1 & (f_dbp >= 80 & f_dbp < 90) ~ 1,
           sb18 == 1 & (f_sbp < 120 | f_sbp >= 140) ~ 0,
           sb18 == 1 & (f_dbp < 80 | f_dbp >= 90) ~ 0,
           TRUE ~ NA_real_),
         
         
         f_diaghtn = case_when(
           is.na(f_sbp) | is.na(f_dbp) ~ NA_real_,
           sb18 == 1 & f_sbp >= 140 ~ 1,
           sb18 == 1 & f_dbp >= 90 ~ 1,
           sb18 == 1 & f_sbp < 140 ~ 0,
           sb18 == 1 & f_dbp < 90 ~ 0,
           TRUE ~ NA_real_)
         
  )

saveRDS(iair_clean,paste0(path_ecological_analysis,"/working/iair74_clean.RDS"))
rm(iair_extract)