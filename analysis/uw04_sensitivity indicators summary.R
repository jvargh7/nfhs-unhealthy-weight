ds_sens_ids = c("S15","S17","S119",
           "S120","S121","S122",
           "S123","S124")


india <- read_csv("data/india.csv") %>% 
  dplyr::filter(id %in% ds_sens_ids) %>% 
  dplyr::select(id,nfhs5s_total,nfhs4s_total) %>% 
  rename(india_nfhs5 = nfhs5s_total,
         india_nfhs4 = nfhs4s_total)

states <- read_csv("data/states_plus_nfhs3.csv") %>% 
  dplyr::filter(id %in% ds_sens_ids) %>% 
  dplyr::select(id,state,nfhs5s_total,nfhs4s_total)  %>%
  group_by(id) %>% 
  summarize(state_nfhs5 = paste0(median(nfhs5s_total) %>% round(.,1)," (",
                                 quantile(nfhs5s_total,0.25) %>% round(.,1),", ",
                                 quantile(nfhs5s_total,0.75) %>% round(.,1),")"),
            state_nfhs4 = paste0(median(nfhs4s_total) %>% round(.,1)," (",
                                 quantile(nfhs4s_total,0.25) %>% round(.,1),", ",
                                 quantile(nfhs4s_total,0.75) %>% round(.,1),")")
  )

left_join(states,
          india,
          by="id") %>% 
  dplyr::select(id,contains("nfhs4"),contains("nfhs5")) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/sensitivity additional human development summary.csv"))


