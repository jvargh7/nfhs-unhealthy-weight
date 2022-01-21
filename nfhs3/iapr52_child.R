path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"

child_variables = c("hv021","hv023","hv005","hv001",
                    "hc1","hv103","hc70","hc72",
                    "hc71","hc72","hc1","hc56",
                    "hc27","hv270","hv025","hv024")

# Replicating nfhs-child-malnutrition\sensitivity\table1 nfhs4_raw.do ---------
iapr_child <- read_dta(paste0(path_india_raw_data,"/IAPR52DT/IAPR52FL.dta"),
                      col_select = child_variables) %>% 
  dplyr::filter(!is.na(hc1)) %>% 

  dplyr::filter(hc1 < 60) %>% 
  mutate(child_age = cut(hc1,breaks=c(0,6,9,12,18,24,36,48,60),right=FALSE,include.lowest=TRUE),
         S81 = case_when(hc70 >= 9996 ~ NA_real_,
                              is.na(hc70) ~ NA_real_,
                              hc70 < -200 ~ 1,
                              TRUE ~ 0),
         S82 = case_when(hc72 >= 9996 ~ NA_real_,
                              is.na(hc72) ~ NA_real_,
                              hc72 < -200 ~ 1,
                              TRUE ~ 0),
         S83 = case_when(hc72 >= 9996 ~ NA_real_,
                             is.na(hc72) ~ NA_real_,
                             hc70 < -300 ~ 1,
                             TRUE ~ 0),
         S84 = case_when(hc71 >= 9996 ~ NA_real_,
                              is.na(hc71) ~ NA_real_,
                              hc71 < -200 ~ 1,
                              TRUE ~ 0),
         S85 = case_when(hc72 >= 9996 ~ NA_real_,
                              is.na(hc72) ~ NA_real_,
                              hc72 > 200 ~ 1,
                              TRUE ~ 0),
         S92 = case_when(hc56 >= 250 | hc56 < 30 ~ NA_real_,
                            hc1 < 6 ~ NA_real_,
                            is.na(hc56) ~ NA_real_,
                            hc56 < 110 ~ 1,
                            TRUE ~ 0)
         )

saveRDS(iapr_child,paste0(path_ecological_analysis,"/working/iapr52_child.RDS"))
