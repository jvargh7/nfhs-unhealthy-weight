library(tidyverse)

path_ecological_analysis <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS5 Ecological Analysis"
dhs_path <- "C:/Cloud/OneDrive - Emory University/data/DHS"

p_val_format = function(p_val){
  case_when(
    # p_val < 0.001 ~ paste0("<0.001"),
    p_val < 0.01 ~ paste0("<0.01"),
    TRUE ~ paste0("= ",p_val %>% round(.,2)))}
