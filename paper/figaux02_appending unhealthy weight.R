
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
districts <- districts %>% 
  bind_rows(districts_unhealthy)

rm(districts_unhealthy)
