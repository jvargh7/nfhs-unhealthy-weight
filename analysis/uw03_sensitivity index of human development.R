ds_sens_ids = c("S14","S16","S20_rev",
           "S09","S08",
           "S07","S10","S12",
           "S15","S17","S119",
           "S120","S121","S122",
           "S123","S124"
           )

# "S15","S17"
# S119	Currently married women who usually participate in three household decisions25 (%)	119
# S120	Women who worked in the last 12 months and were paid in cash (%)	120
# S121	Women owning a house and/or land (alone or jointly with others) (%)
# S122	Women having a bank or savings account that they themselves use (%)
# S123	Women having a mobile phone that they themselves use (%)


states <- read_csv("data/states_plus_nfhs3.csv")

pca_sens_obj <- states %>% 
  dplyr::select(id,state,nfhs4s_total) %>% 
  dplyr::filter(id %in% ds_sens_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4s_total) %>% 
  dplyr::select(one_of(ds_sens_ids)) %>%
  prcomp(.,scale. = TRUE)

# https://stats.stackexchange.com/questions/72839/how-to-use-r-prcomp-results-for-prediction
nfhs4_s <- states %>% 
  dplyr::select(id,state,nfhs4s_total) %>% 
  dplyr::filter(id %in% ds_sens_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4s_total) %>% 
  dplyr::select(one_of(ds_sens_ids)) %>% 
  predict(pca_sens_obj,.)

nfhs5_s <- states %>% 
  dplyr::select(id,state,nfhs5s_total) %>% 
  dplyr::filter(id %in% ds_sens_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs5s_total) %>% 
  dplyr::select(one_of(ds_sens_ids)) %>% 
  predict(pca_sens_obj,.)

state_pc_sens <- read_csv("analysis/state_pc.csv") %>% 
  mutate(nfhs4s_pcsens1 = nfhs4_s[,1]*-1,
         nfhs5s_pcsens1 = nfhs5_s[,1]*-1
  )

with(state_pc_sens,cor.test(nfhs4s_pc1,nfhs4s_pcsens1,method="spearman"))
with(state_pc_sens,cor.test(nfhs5s_pc1,nfhs5s_pcsens1,method="spearman"))


pca_obj$rotation %>% 
  data.frame() %>% 
  mutate(id = rownames(.)) %>% 
  rename_at(vars(starts_with("PC")),~paste0(.,"_v1")) %>% 
  full_join(pca_sens_obj$rotation %>% 
              data.frame() %>% 
              mutate(id = rownames(.)) %>% 
              rename_at(vars(starts_with("PC")),~paste0(.,"_v2")),
            by="id") %>% 
  
  dplyr::select(id,PC1_v1,PC2_v1,PC3_v1,
                PC1_v2,PC2_v2,PC3_v2) %>% 
  mutate_at(vars(starts_with("PC")),
            function(x) case_when(is.na(x) ~ "",
                       TRUE ~ round(x,2) %>% as.character(.))) %>% 
  arrange(id) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/pca rotations compared.csv"))
