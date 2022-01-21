path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"


couples_variables <- readxl::read_excel(paste0(path_couples_folder,"/working/NFHS4 Couples Variable List.xlsx"),
                                        sheet = "iacr52 variables")

iamr_extract <- read_dta(paste0(path_india_raw_data,"/IAMR52DT/IAMR52FL.dta"),
                         col_select = c(na.omit(couples_variables$male),"mv106","mv155","mv511","mv133")) 

iamr_clean <- iamr_extract %>% 
  mutate(
    m_hemo_anemia = case_when(mv456 >= 250 | mv456 < 3 ~ NA_real_,
                              mv456 < 130 ~ 1,
                              mv456 >= 130 ~ 0,
                              TRUE ~ NA_real_),
    
    
    m_bmi_underweight = case_when(mv445 > 6000 ~ NA_real_,
                                  mv445 < 1850 ~ 1,
                                  mv445 >= 1850 ~ 0,
                                  TRUE ~ NA_real_),
    
    
    m_bmi_overweight = case_when(mv445 > 6000 ~ NA_real_,
                                 mv445 >= 2500 & mv445 < 3000 ~ 1,
                                 mv445 < 2500 | mv445 >= 3000 ~ 0,
                                 TRUE ~ NA_real_),
    
    
    m_bmi_obese = case_when(mv445 > 6000 ~ NA_real_,
                            mv445 >= 3000 ~ 1,
                            mv445 < 3000 ~ 0,
                            TRUE ~ NA_real_)
    
  )  

saveRDS(iamr_clean,paste0(path_ecological_analysis,"/working/iamr52_clean.RDS"))
rm(iamr_extract)
