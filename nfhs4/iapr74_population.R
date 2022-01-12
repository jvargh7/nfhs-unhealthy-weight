path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"

population_variables <- c("hv001","hv002","hv005",
                          "hv009",
                          "hv012",
                          "hv024","hv025","hv201",
                          "hv023","shdistri",
                          "hv205","hv225","hv206",
                          "sh54","hv226")


# Replicating nfhs-child-malnutrition\sensitivity\02_table2.do ---------
iahr_population <- read_dta(paste0(path_india_raw_data,"/IAHR74DT/IAHR74FL.dta"),
                       col_select = population_variables) %>% 
  # dplyr::filter(hv102 == 1) %>% 
  mutate(S08 = case_when(hv201 == 99 ~ NA_real_,
                           hv201 %in% c(11:15,21,31,41,51,61:73) ~ 1,
                           hv201 %in% c(30,32,40,42,43,96) ~ 0,
                           TRUE ~ NA_real_),
         S09 = case_when(hv205 == 99 ~ NA_real_,
                                hv225 == 1 ~ 0, # Shared toilet = unimproved
                                hv205 %in% c(11:13,15,21,22,41,51) ~ 1, # improved
                                hv205 %in% c(14,23,42,43,96) ~ 0, # Unimproved
                                hv205 == 31 ~ 0, # Open defecation
                                TRUE ~ NA_real_),
         
         S07 = case_when(hv206 == 9 ~ NA_real_,
                                 TRUE ~ as.numeric(hv206))
         
         ) %>% 
  # group_by(hv024,shdistri,hv001,hv002) %>% 
  # mutate(hmembers = n()) %>% 
  # ungroup() %>% 
  mutate(hmembers = hv012) %>% 
  distinct(hv024,shdistri,hv001,hv002,.keep_all=TRUE)

saveRDS(iahr_population,paste0(path_ecological_analysis,"/working/iahr74_population.RDS"))
rm(iahr_population)
