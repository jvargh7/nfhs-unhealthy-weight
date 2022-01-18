nfhs4_mapping <- readxl::read_excel("data/uw_mapping.xlsx",sheet="nfhs4 to nfhs5") %>% 
  dplyr::select(v024_nfhs4,sdistri,nfhs4_state,nfhs4_statecode,nfhs4_district) %>%
  dplyr::filter(!is.na(nfhs4_district)) %>% 
  distinct(sdistri,.keep_all=TRUE)

nfhs4_states <- nfhs4_mapping %>% 
  distinct(nfhs4_statecode,nfhs4_state)


n4_district <-  readxl::read_excel(paste0(path_ecological_analysis,"/working/NFHS 4_District and State.xlsx"),
                                   sheet = "District") %>%
  rename(nfhs4d_total = Value.num,
         nfhs4d_item = Variable,
         nfhs4_statecode = State,
         nfhs4_district = District) %>% 
  dplyr::select(nfhs4d_item,nfhs4d_total,nfhs4_statecode,nfhs4_district)%>% 
  mutate(nfhs4d_total = as.numeric(nfhs4d_total),
         nfhs4_statecode = case_when(nfhs4_statecode == "GO" ~ "GA",
                                     TRUE ~ nfhs4_statecode)) %>% 
  left_join(readxl::read_excel("data/uw_mapping.xlsx",sheet="nfhs4_district") %>% 
              dplyr::select(ID,nfhs4d_item),
            by = c("nfhs4d_item")) %>% 
  dplyr::select(-nfhs4d_item) %>% 
  left_join(nfhs4_states,
            by="nfhs4_statecode")  %>% 
  mutate(nfhs4_district = str_replace(nfhs4_district,nfhs4_state,"") %>% str_trim()) %>% 
  mutate(nfhs4_district = str_replace_all(nfhs4_district,"\\s\\s"," ")) %>% 
  mutate(nfhs4_district = case_when(nfhs4_district == "Korea" & nfhs4_statecode == "CT" ~ "Korea (Koriya)",
                                    nfhs4_district == "Darjiling" & nfhs4_statecode == "WB" ~ "Darjeeling",
                                    str_detect(nfhs4_district,"Tripura") ~ str_replace(nfhs4_district,"\\sTripura",""),
                                    TRUE ~ nfhs4_district))


n4_uts <- readxl::read_excel(paste0(path_ecological_analysis,"/working/NFHS 4_District and State.xlsx"),
                             sheet = "States2") %>%
  rename(nfhs4s_item = ID,
         nfhs4_statecode = NFHS4_S,
         nfhs4d_total = Value) %>% 
  mutate(nfhs4d_total = as.numeric(nfhs4d_total)) %>% 
  dplyr::filter(nfhs4_statecode %in% c("LK","CH","DN"),Area == "Total") %>% 
  dplyr::select(nfhs4s_item,nfhs4_statecode,nfhs4d_total)  %>% 
  left_join(readxl::read_excel("data/uw_mapping.xlsx",sheet="nfhs4_state") %>% 
              dplyr::select(ID,nfhs4s_item),
            by = c("nfhs4s_item")) %>% 
  dplyr::select(-nfhs4s_item) %>% 
  mutate(nfhs4_district = case_when(nfhs4_statecode == "LK" ~ "Lakshadweep",
                                    nfhs4_statecode == "CH" ~ "Chandigarh",
                                    nfhs4_statecode == "DN" ~ "Dadra & Nagar Haveli",
                                    TRUE ~ NA_character_)) %>% 
  left_join(nfhs4_states,
            by="nfhs4_statecode")


nfhs4_factsheets <- bind_rows(n4_district,
                              n4_uts) %>% 
  left_join(nfhs4_mapping,
            by=c("nfhs4_statecode","nfhs4_district","nfhs4_state")) %>% 
  dplyr::select(ID,v024_nfhs4,sdistri,nfhs4_statecode,nfhs4_state,nfhs4_district,nfhs4d_total)


write_csv(nfhs4_factsheets,"data/nfhs4 district factsheets long.csv")
