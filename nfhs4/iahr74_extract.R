path_couples_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS4 Couples"
path_india_raw_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program/IA"

iahr_extract <- read_dta(paste0(path_india_raw_data,"/IAHR74DT/IAHR74FL.dta"),
                         col_select = c("hv001","hv005","hv024","hv025","hv021","hv022",
                                        "sh54","hv226","shdistri")) %>% 
  mutate(S12 = case_when(sh54 %in% c(8,9) ~ NA_real_,
                               TRUE ~ as.numeric(sh54)),
         S10 = case_when(hv226 %in% c(95,96,99) ~ NA_real_,
                             hv226 %in% c(1:4) ~ 1,
                             hv226 %in% c(5:11) ~ 0,
                             TRUE ~ NA_real_)
         )


saveRDS(iahr_extract,paste0(path_ecological_analysis,"/working/iahr74_extract.RDS"))

