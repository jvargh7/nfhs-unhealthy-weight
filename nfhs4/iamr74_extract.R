path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"


couples_variables <- readxl::read_excel(paste0(path_couples_folder,"/working/NFHS4 Couples Variable List.xlsx"),
                                        sheet = "iacr variables")

iamr_extract <- read_dta(paste0(path_india_raw_data,"/IAMR74DT/IAMR74FL.dta"),
                         col_select = c(na.omit(couples_variables$male),"mv106","mv155","mv511","mv133")) 

iapr_male <- read_dta(paste0(path_india_raw_data,"/IAPR74DT/IAPR74FL.dta"),
                      col_select = na.omit(couples_variables$iapr74)) %>% 
  dplyr::filter(!is.na(hb1))

iamr_clean <- iamr_extract %>% 
  left_join(iapr_male,
            by=c("mv003"="hvidx",
                 "mv002" = "hv002",
                 "mv001" = "hv001")) %>% 
  # Blood pressure cleaning -------
mutate_at(vars(
  smb16s,smb16d,
  smb23s,smb23d,
  smb27s,smb27d),function(x) case_when(as.numeric(x) %in% c(994,995,996,999) ~ NA_real_,
                                       TRUE ~ as.numeric(x))) %>% 
  mutate(
    m_hemo_anemia = case_when(hb56 >= 250 | hb56 < 3 ~ NA_real_,
                              hb56 < 130 ~ 1,
                              hb56 >= 130 ~ 0,
                              TRUE ~ NA_real_),
    
    
    m_bmi_underweight = case_when(hb40 > 6000 ~ NA_real_,
                                  hb40 < 1850 ~ 1,
                                  hb40 >= 1850 ~ 0,
                                  TRUE ~ NA_real_),
    
    
    m_bmi_overweight = case_when(hb40 > 6000 ~ NA_real_,
                                 hb40 >= 2500 & hb40 < 3000 ~ 1,
                                 hb40 < 2500 | hb40 >= 3000 ~ 0,
                                 TRUE ~ NA_real_),
    
    
    m_bmi_obese = case_when(hb40 > 6000 ~ NA_real_,
                            hb40 >= 3000 ~ 1,
                            hb40 < 3000 ~ 0,
                            TRUE ~ NA_real_),
    
    
    m_fasting = case_when(smb51 > 94 | smb52 > 94 ~ NA_real_,
                          smb51 > 8 & smb52 > 8 ~ 1,
                          smb51 <=8 | smb52 <= 8 ~ 0,
                          TRUE ~ NA_real_),
    
    
    m_ifg = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                      sm622a == 1 ~ NA_real_, # Exclude those who have diabetes
                      m_fasting == 1 & smb70 >= 100 & smb70 < 126 ~ 1,
                      m_fasting == 1 & (smb70 < 100 | smb70 >= 126) ~ 0,
                      m_fasting == 0 ~ NA_real_,
                      TRUE ~ NA_real_),
    
    
    m_igt = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                      sm622a == 1 ~ NA_real_, # Exclude those who have diabetes
                      m_fasting == 0 & smb70 >= 140 & smb70 < 200 ~ 1,
                      m_fasting == 0 & (smb70 < 140 | smb70 >= 200) ~ 0,
                      m_fasting == 1 ~ NA_real_,
                      TRUE ~ NA_real_),
    
    
    m_diagifg = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                          sm622a == 1 & m_fasting == 1 & smb70 >= 100 & smb70 < 126 ~ 1,
                          sm622a == 1 & m_fasting == 1 & (smb70 < 100 | smb70 >= 126) ~ 0,
                          sm622a == 1 & m_fasting == 0 ~ NA_real_,
                          TRUE ~ NA_real_),
    
    
    m_diagigt = case_when(is.na(smb70) | smb70 > 498 ~ NA_real_,
                          sm622a == 1 & m_fasting == 0 & smb70 >= 140 & smb70 < 200 ~ 1,
                          sm622a == 1 & m_fasting == 0 & (smb70 < 140 | smb70 >= 200) ~ 0,
                          sm622a == 1 & m_fasting == 1 ~ NA_real_,
                          TRUE ~ NA_real_),
    
    
    m_dm = case_when(sm622a == 1 ~ 1,
                     is.na(smb70) | smb70 > 498 ~ NA_real_,
                     m_fasting == 1 & smb70 >= 126 ~ 1,
                     m_fasting == 0 & smb70 >= 200 ~ 1,
                     is.na(m_fasting) & smb70 >= 200 ~ 1,
                     m_fasting == 1 & smb70 < 126 ~ 0,
                     m_fasting == 0 & smb70 < 200 ~ 0,
                     is.na(m_fasting) & smb70 < 200 ~ 0,
                     TRUE  ~ NA_real_
    ),
    
    
    m_diagdm = case_when(
      is.na(smb70) | smb70 > 498 ~ NA_real_,
      sm622a == 1 & m_fasting == 1 & smb70 >= 126 ~ 1,
      sm622a == 1 & m_fasting == 0 & smb70 >= 200 ~ 1,
      sm622a == 1 & is.na(m_fasting) & smb70 >= 200 ~ 1,
      sm622a == 1 & m_fasting == 1 & smb70 < 126 ~ 0,
      sm622a == 1 & m_fasting == 0 & smb70 < 200 ~ 0,
      sm622a == 1 & is.na(m_fasting) & smb70 < 200 ~ 0,
      TRUE  ~ NA_real_
    ),
    
    
    
    m_sbp = rowMeans(.[,c("smb16s","smb23s","smb27s")],na.rm=TRUE),
    
    m_dbp = rowMeans(.[,c("smb16d","smb23d","smb27d")],na.rm=TRUE),
    
    
    m_prehtn = case_when(smb18 == 1 ~ NA_real_,
                         is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                         m_sbp >= 120 & m_sbp < 140 ~ 1,
                         m_dbp >= 80 & m_dbp < 90 ~ 1,
                         m_dbp < 120 | m_dbp >= 140 ~ 0,
                         m_dbp < 80 | m_dbp >= 90 ~ 0,
                         TRUE ~ NA_real_),
    
    
    m_htn = case_when(smb18 == 1 ~ 1,
                      is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
                      m_sbp >= 140 ~ 1,
                      m_dbp >= 90 ~ 1,
                      m_sbp < 140 ~ 0,
                      m_dbp < 90 ~ 0,
                      TRUE ~ NA_real_),
    
    
    m_diagprehtn = case_when(
      is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
      smb18 == 1 & (m_sbp >= 120 & m_sbp < 140) ~ 1,
      smb18 == 1 & (m_dbp >= 80 & m_dbp < 90) ~ 1,
      smb18 == 1 & (m_dbp < 120 | m_dbp >= 140) ~ 0,
      smb18 == 1 & (m_dbp < 80 | m_dbp >= 90) ~ 0,
      TRUE ~ NA_real_),
    
    
    
    
    
    
    m_diaghtn = case_when(
      is.na(m_sbp) | is.na(m_dbp) ~ NA_real_,
      smb18 == 1 & m_sbp >= 140 ~ 1,
      smb18 == 1 & m_dbp >= 90 ~ 1,
      smb18 == 1 & m_sbp < 140 ~ 0,
      smb18 == 1 & m_dbp < 90 ~ 0,
      TRUE ~ NA_real_)
    
  )  

saveRDS(iamr_clean,paste0(path_ecological_analysis,"/working/iamr74_clean.RDS"))
rm(iamr_extract,iapr_male)