
# India ----------

india_nfhs5 <- read_csv("data/india.csv") %>% 
  dplyr::select(id,nfhs5s_total)

india_nfhs5_unhealthy <- india_nfhs5 %>% 
    dplyr::filter(id %in% c("S86","S88","S87","S89")) %>% 
  pivot_wider(names_from=id,values_from=nfhs5s_total) %>% 
  mutate(unhealthy_f = S86 + S88,
         unhealthy_m = S87 + S89) %>% 
  dplyr::select(unhealthy_f,unhealthy_m) %>% 
  pivot_longer(cols=everything(),names_to="id",values_to="nfhs5s_total")

india_nfhs5 <- bind_rows(india_nfhs5,
                         india_nfhs5_unhealthy)

india <- read_csv("nfhs4/india long.csv") %>% 
  mutate(id = case_when(id == "unhealthy_female" ~ "unhealthy_f",
                        id == "unhealthy_male" ~ "unhealthy_m",
                        TRUE ~ id)) %>% 
  mutate(india_nfhs4 = paste0(round(mean,1)," (",
                              round(lci,1),", ",
                              round(uci,1),")")) %>% 
  left_join(india_nfhs5,
            by = "id") %>% 
  dplyr::select(id,nfhs5s_total,india_nfhs4,mean,lci,uci) %>% 
  rename(india_nfhs5 = nfhs5s_total,
         india_nfhs4_mean = mean,
         india_nfhs4_lci = lci,
         india_nfhs4_uci = uci)
rm(india_nfhs5,india_nfhs5_unhealthy)

# States -----------------
states <- read_csv("data/states.csv")

states_unhealthy <- bind_rows(
  states %>% 
    dplyr::filter(id %in% c("S86","S88")) %>% 
    pivot_longer(cols=matches("nfhs(4|5)s"),names_to="indicator",values_to="values") %>% 
    pivot_wider(names_from=id,values_from = values) %>% 
    mutate(unhealthy_f = S86 + S88,
           propf_overweight = S88*100/(S86 + S88)) %>% 
    dplyr::select(-S86,-S88) %>% 
    pivot_longer(cols=one_of(c("unhealthy_f","propf_overweight")),names_to="id",values_to="values") %>% 
    pivot_wider(names_from=indicator,values_from="values"),
  states %>% 
    dplyr::filter(id %in% c("S87","S89")) %>% 
    pivot_longer(cols=matches("nfhs(4|5)s"),names_to="indicator",values_to="values") %>% 
    pivot_wider(names_from=id,values_from = values) %>% 
    mutate(unhealthy_m = S87 + S89,
           propm_overweight = S89*100/(S87 + S89)) %>% 
    dplyr::select(-S87,-S89) %>% 
    pivot_longer(cols=one_of(c("unhealthy_m","propm_overweight")),names_to="id",values_to="values") %>% 
    pivot_wider(names_from=indicator,values_from="values")
  
)

states_unhealthy %>% 
  dplyr::filter(id %in% c("propf_overweight","propm_overweight")) %>% 
  group_by(id) %>% 
  summarize(nfhs4 = sum(nfhs4s_total > 50),
            nfhs5 = sum(nfhs5s_total > 50))

states <- states %>% 
  bind_rows(states_unhealthy)

rm(states_unhealthy)

# Districts ---------------
districts <- read_csv("data/districts_plus_uts.csv")
districts_unhealthy <-   districts %>% 
    dplyr::filter(id %in% c("S86","S88")) %>% 
    pivot_longer(cols=matches("nfhs(4|5)d"),names_to="indicator",values_to="values") %>% 
    pivot_wider(names_from=id,values_from = values) %>% 
    mutate(unhealthy_f = S86 + S88,
           propf_overweight = S88*100/(S86 + S88)) %>% 
    dplyr::select(-S86,-S88) %>% 
    pivot_longer(cols=one_of(c("unhealthy_f","propf_overweight")),names_to="id",values_to="values") %>% 
    pivot_wider(names_from=indicator,values_from="values")

districts_unhealthy %>% 
  dplyr::filter(id %in% c("propf_overweight")) %>% 
  group_by(id) %>% 
  summarize(nfhs4 = sum(nfhs4d_total > 50),
            nfhs5 = sum(nfhs5d_total > 50))

districts <- districts %>% 
  bind_rows(districts_unhealthy)

rm(districts_unhealthy)
