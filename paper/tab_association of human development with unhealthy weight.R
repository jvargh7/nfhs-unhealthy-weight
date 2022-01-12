
ds_ids = c("S14","S16","S20",
           "S09","S08",
           "S07","S10","S12","INDEX")
outcome_ids <- c("S86","S87","S88","S89","Unhealthy Women","Unhealthy Men")

state_pc <- read_csv("unhealthy weight/state_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  rename(nfhs5s_total = nfhs5s_pc1,
         nfhs4s_total = nfhs4s_pc1)


indicators_states <- read_csv("chronic disease/states.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  dplyr::select(id,state,nfhs5s_total,nfhs4s_total)  %>%
  bind_rows(state_pc)

outcomes_states <- read_csv("chronic disease/states.csv") %>% 
  dplyr::filter(id %in% outcome_ids) %>% 
  dplyr::select(id,state,nfhs5s_total,nfhs4s_total)

unhealthy_weight = outcomes_states %>% 
  mutate(id = case_when(id %in% c("S86","S88") ~ "Unhealthy Women",
                        id %in% c("S87","S89") ~ "Unhealthy Men",
                        TRUE ~ NA_character_)) %>% 
  group_by(id,state) %>% 
  dplyr::summarise_at(vars(contains("_total")),~sum(.))

outcomes_states2 <- bind_rows(outcomes_states,
                              unhealthy_weight)

nfhs4_summary <- map2_df(.x=expand.grid(ds_ids,outcome_ids)$Var1,
                         .y=expand.grid(ds_ids,outcome_ids)$Var2,
                         .f=function(x,y){
                           
                           x_vec = indicators_states %>% 
                             dplyr::filter(id == x) %>% 
                             dplyr::select(nfhs4s_total) %>% 
                             pull();
                           
                           y_vec = outcomes_states2 %>% 
                             dplyr::filter(id == y) %>% 
                             dplyr::select(nfhs4s_total) %>% 
                             pull();
                           
                           data.frame(indicator = x,
                                      outcome = y,
                                      r = cor.test(x_vec,y_vec)$estimate %>% round(.,2),
                                      p_val = cor.test(x_vec,y_vec)$p.value %>% p_val_format(.)) %>% 
              return(.)
            
          }
  ) %>% 
  mutate(r_pval = paste0(r,", p",p_val)) %>% 
  dplyr::select(-r,-p_val) %>% 
  pivot_wider(names_from=outcome,values_from=r_pval)



nfhs5_summary <- map2_df(.x=expand.grid(ds_ids,outcome_ids)$Var1,
                         .y=expand.grid(ds_ids,outcome_ids)$Var2,
                         .f=function(x,y){
                           
                           x_vec = indicators_states %>% 
                             dplyr::filter(id == x) %>% 
                             dplyr::select(nfhs5s_total) %>% 
                             pull();
                           
                           y_vec = outcomes_states2 %>% 
                             dplyr::filter(id == y) %>% 
                             dplyr::select(nfhs5s_total) %>% 
                             pull();
                           
                           data.frame(indicator = x,
                                      outcome = y,
                                      r = cor.test(x_vec,y_vec)$estimate %>% round(.,2),
                                      p_val = cor.test(x_vec,y_vec)$p.value %>% p_val_format(.)) %>% 
                             return(.)
                           
                         }
) %>% 
  mutate(r_pval = paste0(r,", p",p_val)) %>% 
  dplyr::select(-r,-p_val) %>% 
  pivot_wider(names_from=outcome,values_from=r_pval)

left_join(nfhs4_summary %>% rename_at(vars(starts_with("S")), ~ paste0(.,"_N4")),
          nfhs5_summary%>% rename_at(vars(starts_with("S")), ~ paste0(.,"_N5")),
          by="indicator") %>%
  mutate(indicator = as.character(indicator)) %>% 
  dplyr::arrange(indicator) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/association of human development.csv"))
