ds_ids = c("S86","S87","S88","S89",
           
           "S14","S16","S20_rev",
           "S09","S08",
           "S07","S10","S12")
state_pc <- read_csv("analysis/state_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  rename(nfhs5s_total = nfhs5s_pc1,
         nfhs4s_total = nfhs4s_pc1)

india <- read_csv("nfhs4/india long.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  mutate(india_nfhs4 = paste0(round(mean,1)," (",
                              round(lci,1),", ",
                              round(uci,1),")")) %>% 
  left_join(read_csv("data/india.csv") %>% 
              dplyr::select(id,nfhs5s_total),
            by = "id") %>% 
  dplyr::select(id,nfhs5s_total,india_nfhs4) %>% 
  rename(india_nfhs5 = nfhs5s_total)



states <- read_csv("data/states.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  dplyr::select(id,state,nfhs5s_total,nfhs4s_total)  %>%
  bind_rows(state_pc) %>% 
  group_by(id) %>% 
  summarize(state_nfhs5 = paste0(median(nfhs5s_total) %>% round(.,1)," [",
                                 quantile(nfhs5s_total,0.25) %>% round(.,1),", ",
                                 quantile(nfhs5s_total,0.75) %>% round(.,1),"]"),
            state_nfhs4 = paste0(median(nfhs4s_total) %>% round(.,1)," [",
                                 quantile(nfhs4s_total,0.25) %>% round(.,1),", ",
                                 quantile(nfhs4s_total,0.75) %>% round(.,1),"]")
            )


change_india <- read_csv("nfhs4/india long.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  left_join(read_csv("data/india.csv") %>% 
              dplyr::select(id,nfhs5s_total),
            by = "id") %>% 
  mutate(se = (mean - lci)/1.96,
         india_change = nfhs5s_total - mean ) %>% 
  mutate(z = (mean - nfhs5s_total)/sqrt(se^2 + se^2),
         p_val = pnorm(abs(z),lower.tail = FALSE),
         india_change = paste0(india_change %>% round(.,1)," (",
                               (india_change - 1.96*sqrt(se^2 + se^2)) %>% round(.,1),", ",
                               (india_change + 1.96*sqrt(se^2 + se^2)) %>% round(.,1),")"
                               )
         ) %>% 
  dplyr::select(id,india_change)

change_states <- read_csv("nfhs4/state long.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  left_join(read_csv("data/states.csv") %>% 
              dplyr::select(id,v024_label,nfhs5s_total),
            by=c("id","state"="v024_label")) %>% 
  mutate(se = (mean - lci)/1.96,
         state_change = nfhs5s_total - mean %>% round(.,1),
         z = (mean - nfhs5s_total)/sqrt(se^2 + se^2),
         p_val = pnorm(abs(z),lower.tail = FALSE)) %>% 
  group_by(id) %>% 
  summarize(state_change = paste0(median(state_change) %>% round(.,1)," [",
                                  quantile(state_change,0.25) %>% round(.,1),", ",
                                  quantile(state_change,0.75) %>% round(.,1),"]",
                                  ", ",sum(abs(z)> 1.96))
  )


left_join(states,
          india,
          by="id") %>% 
  left_join(change_india,
            by="id") %>% 
  left_join(change_states,
            by="id") %>% 
  dplyr::select(id,india_nfhs4,india_nfhs5,india_change,
                state_nfhs4,state_nfhs5,state_change) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/table human development summary.csv"))


