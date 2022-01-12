library(sp)
library(rgdal)
library(tmap)
path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
shape_df <-  readOGR(paste0(path_shape_files,"/sdr_subnational_boundaries_2020-12-28/shps"),"sdr_subnational_boundaries2")
bound_df <- readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2")

districts <- read_csv("data/districts_plus_uts.csv") %>% 
  pivot_wider(names_from=id,values_from=c(nfhs5d_total,nfhs4d_total)) %>% 
  mutate_at(vars(-sdistri),~as.numeric(.)) %>% 
  mutate(
    nfhs5d4_total_S86 = nfhs5d_total_S86 - nfhs4d_total_S86,
    nfhs5d4_total_S87 = nfhs5d_total_S87 - nfhs4d_total_S87,
    nfhs5d4_total_S88 = nfhs5d_total_S88 - nfhs4d_total_S88,
    nfhs5d4_total_S89 = nfhs5d_total_S89 - nfhs4d_total_S89
  ) %>% 
  mutate(nfhs4d_f_unhealthyweight = nfhs4d_total_S86 + nfhs4d_total_S88,
         # nfhs4d_m_unhealthyweight= nfhs4d_total_S87 + nfhs4d_total_S89,
         nfhs5d_f_unhealthyweight = nfhs5d_total_S86 + nfhs5d_total_S88
         # nfhs5d_m_unhealthyweight= nfhs5d_total_S87 + nfhs5d_total_S89
         )


# states <- read_csv("data/states.csv") %>% 
states <- read_csv("data/states_plus_nfhs3.csv") %>% 
  dplyr::select(-nfhs5s_urban,-nfhs5s_rural,-nfhs4s_rural,-nfhs4s_urban,-nfhs3s_rural,-nfhs3s_urban) %>% 
  pivot_wider(names_from=id,values_from=c(nfhs5s_total,nfhs4s_total,nfhs3s_total)) %>% 
  mutate(
    nfhs5s4_total_S86 = nfhs5s_total_S86 - nfhs4s_total_S86,
    nfhs5s4_total_S87 = nfhs5s_total_S87 - nfhs4s_total_S87,
    nfhs5s4_total_S88 = nfhs5s_total_S88 - nfhs4s_total_S88,
    nfhs5s4_total_S89 = nfhs5s_total_S89 - nfhs4s_total_S89,
    
    nfhs4s3_total_S86 = nfhs4s_total_S86 - nfhs3s_total_S86,
    nfhs4s3_total_S87 = nfhs4s_total_S87 - nfhs3s_total_S87,
    nfhs4s3_total_S88 = nfhs4s_total_S88 - nfhs3s_total_S88,
    nfhs4s3_total_S89 = nfhs4s_total_S89 - nfhs3s_total_S89,
    
    
    nfhs5s4_f_unhealthyweight = nfhs5s_total_S86 + nfhs5s_total_S88 - nfhs4s_total_S86 - nfhs4s_total_S88,
    nfhs5s4_m_unhealthyweight = nfhs5s_total_S87 + nfhs5s_total_S89 - nfhs4s_total_S87 - nfhs4s_total_S89,
    nfhs4s3_f_unhealthyweight = nfhs4s_total_S86 + nfhs4s_total_S88 - nfhs3s_total_S86 - nfhs3s_total_S88,
    nfhs4s3_m_unhealthyweight = nfhs4s_total_S87 + nfhs4s_total_S89 - nfhs3s_total_S87 - nfhs3s_total_S89
    
  ) %>% 
  mutate(nfhs3s_f_unhealthyweight = nfhs3s_total_S86 + nfhs3s_total_S88,
         nfhs3s_m_unhealthyweight= nfhs3s_total_S87 + nfhs3s_total_S89,
         nfhs4s_f_unhealthyweight = nfhs4s_total_S86 + nfhs4s_total_S88,
         nfhs4s_m_unhealthyweight= nfhs4s_total_S87 + nfhs4s_total_S89,
         nfhs5s_f_unhealthyweight = nfhs5s_total_S86 + nfhs5s_total_S88,
         nfhs5s_m_unhealthyweight= nfhs5s_total_S87 + nfhs5s_total_S89
  )


shape_df2 <- sp::merge(shape_df,districts,
                       by.x="REGCODE",by.y="sdistri",all.x=TRUE)

bound_df2 <- sp::merge(bound_df,states %>% 
                         mutate(ST_NM = case_when(state == "Andaman & Nicobar Islands" ~ "Andaman & Nicobar",
                                                  state == "Dadra & Nagar Haveli and Daman & Diu" ~ "Dadra and Nagar Haveli and Daman and Diu",
                                                  state == "NCT of Delhi" ~ "Delhi",
                                                  TRUE ~ state)),
                       by.x="ST_NM",by.y="ST_NM",all.x=TRUE)
