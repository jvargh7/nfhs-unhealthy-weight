
# S14	Women who are literate4 (%)

path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"


couples_variables <- readxl::read_excel(paste0(path_couples_folder,"/working/NFHS4 Couples Variable List.xlsx"),
                                        sheet = "iacr52 variables")

iair_extract <- read_dta(paste0(path_india_raw_data,"/IAIR52DT/IAIR52FL.dta"),
                         col_select = c(na.omit(couples_variables$female),"v106","v155","v511","v133"))

iair_clean <- iair_extract  %>% 
  # Blood pressure cleaning -------
  mutate(f_hemo_anemia = case_when(v456 >= 250 | v456 < 3 ~ NA_real_,
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
                                 TRUE ~ NA_real_)
         
  )

saveRDS(iair_clean,paste0(path_ecological_analysis,"/working/iair52_clean.RDS"))
rm(iair_extract)
