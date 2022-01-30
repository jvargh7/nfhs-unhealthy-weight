
source("analysis/uw03_sensitivity index of human development.R")

ds_ids = c("S15","S17","S119",
           "S120","S121","S122",
           "S123","S124","INDEX2")
outcome_ids <- c("S86","S87","S88","S89")


indicators_states <- read_csv("data/states_plus_nfhs3.csv") %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  dplyr::select(id,state,nfhs5s_total,nfhs4s_total)  %>%
  bind_rows(state_pc_sens %>% 
              mutate(id = "INDEX2") %>% 
              dplyr::select(-nfhs5s_pc1,-nfhs4s_pc1) %>% 
              rename(nfhs5s_total = nfhs5s_pcsens1,
                     nfhs4s_total = nfhs4s_pcsens1) 
              )

outcomes_states <- read_csv("data/states_plus_nfhs3.csv") %>% 
  dplyr::filter(id %in% outcome_ids) %>% 
  dplyr::select(id,state,nfhs5s_total,nfhs4s_total)



nfhs4_summary <- map2_df(.x=expand.grid(ds_ids,outcome_ids)$Var1,
                         .y=expand.grid(ds_ids,outcome_ids)$Var2,
                         .f=function(x,y){
                           
                           x_vec = indicators_states %>% 
                             dplyr::filter(id == x) %>% 
                             dplyr::select(nfhs4s_total) %>% 
                             pull();
                           
                           y_vec = outcomes_states %>% 
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
  mutate(r_pval = paste0(r,", p-val",p_val)) %>% 
  dplyr::select(-r,-p_val) %>% 
  pivot_wider(names_from=outcome,values_from=r_pval)



nfhs5_summary <- map2_df(.x=expand.grid(ds_ids,outcome_ids)$Var1,
                         .y=expand.grid(ds_ids,outcome_ids)$Var2,
                         .f=function(x,y){
                           
                           x_vec = indicators_states %>% 
                             dplyr::filter(id == x) %>% 
                             dplyr::select(nfhs5s_total) %>% 
                             pull();
                           
                           y_vec = outcomes_states %>% 
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
  write_csv(.,paste0(path_ecological_analysis,"/working/sensitivity indicators association.csv"))
