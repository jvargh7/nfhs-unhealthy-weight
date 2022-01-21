# 2005-06 to 2015-16--------------

ds_ids = c("S86","S87","S88","S89",
           
           "S14","S16","S20_rev",
           "S09","S08",
           "S07","S10","S12")

state_pc <- read_csv("analysis/state_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  dplyr::select(-nfhs5s_pc1) %>% 
  rename(nfhs3s_total = nfhs3s_pc1,
         nfhs4s_total = nfhs4s_pc1)

# INDIA ----------
india <- read_csv("nfhs4/india long.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  mutate(india_nfhs4 = paste0(round(mean,1)," (",
                              round(lci,1),", ",
                              round(uci,1),")"),
         mean_nfhs4 = mean,
         se_nfhs4 = (mean - lci)/1.96
         ) %>% 
  dplyr::select(id,india_nfhs4,mean_nfhs4,se_nfhs4) %>% 
  left_join(read_csv("nfhs3/india long.csv") %>% 
              mutate(india_nfhs3 = paste0(round(mean,1)," (",
                                          round(lci,1),", ",
                                          round(uci,1),")"),
                     mean_nfhs3 = mean,
                     se_nfhs3 = (mean - lci)/1.96
                     ) %>% 
              dplyr::select(id,india_nfhs3,mean_nfhs3,se_nfhs3),
            by = "id")  %>% 
  mutate(india_change = mean_nfhs4 - mean_nfhs3)  %>% 
  mutate(z = (india_change)/sqrt(se_nfhs3^2 + se_nfhs4^2),
         p_val = pnorm(abs(z),lower.tail = FALSE),
         india_change = paste0(india_change %>% round(.,1)," (",
                               (india_change - 1.96*sqrt(se_nfhs3^2 + se_nfhs4^2)) %>% round(.,1),", ",
                               (india_change + 1.96*sqrt(se_nfhs3^2 + se_nfhs4^2)) %>% round(.,1),")"
         )
  )


# STATES --------------
nfhs5_factsheet_map <- readxl::read_excel("data/uw_mapping.xlsx",sheet="nfhs5_factsheet_map")

states <- read_csv("nfhs4/state long.csv") %>% 
  dplyr::filter(id %in% ds_ids)  %>% 
  mutate(state_nfhs4 = paste0(round(mean,1)," (",
                              round(lci,1),", ",
                              round(uci,1),")"),
         nfhs4s_total = mean,
         se_nfhs4 = (mean - lci)/1.96
  ) %>% 
  dplyr::select(id,state,state_nfhs4,nfhs4s_total,se_nfhs4) %>%
  left_join(nfhs5_factsheet_map,
            by = c("state"="v024_label")) %>% 
  
  left_join(read_csv("nfhs3/state long.csv") %>% 
              mutate(state_nfhs3 = paste0(round(mean,1)," (",
                                          round(lci,1),", ",
                                          round(uci,1),")"),
                     nfhs3s_total = mean,
                     se_nfhs3 = (mean - lci)/1.96
              ) %>% 
              dplyr::select(id,state,state_nfhs3,nfhs3s_total,se_nfhs3),
            by = c("v024_nfhs3"="state","id")
    
  ) %>% 
  dplyr::select(-v024_nfhs3,-state) %>% 
  rename(state = factsheet_state) %>% 
  
  # bind_rows(state_pc) %>% 
  dplyr::filter(!is.na(nfhs3s_total)) %>% 
  
  mutate(state_change = nfhs4s_total - nfhs3s_total) %>% 
  mutate(z = (state_change)/sqrt(se_nfhs3^2 + se_nfhs4^2),
         p_val = pnorm(abs(z),lower.tail = FALSE)) %>% 
  group_by(id) %>% 
  summarize(state_nfhs4 = paste0(median(nfhs4s_total) %>% round(.,1)," [",
                                 quantile(nfhs4s_total,0.25) %>% round(.,1),", ",
                                 quantile(nfhs4s_total,0.75) %>% round(.,1),"]"),
            state_nfhs3 = paste0(median(nfhs3s_total) %>% round(.,1)," [",
                                 quantile(nfhs3s_total,0.25) %>% round(.,1),", ",
                                 quantile(nfhs3s_total,0.75) %>% round(.,1),"]"),
            state_change = paste0(median(state_change) %>% round(.,1)," [",
                                  quantile(state_change,0.25) %>% round(.,1),", ",
                                  quantile(state_change,0.75) %>% round(.,1),"]",
                                  ", ",sum(abs(z)> 2.58)))
            
      

left_join(states,
          india,
          by="id") %>% 
  dplyr::select(id,india_nfhs3,india_nfhs4,india_change,
                state_nfhs3,state_nfhs4,state_change) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/table 2005-15 human development summary.csv"))


