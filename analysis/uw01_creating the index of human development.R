ds_ids = c("S14","S16","S20_rev",
           "S09","S08",
           "S07","S10","S12")
           
# "S15","S17"
# S119	Currently married women who usually participate in three household decisions25 (%)	119
# S120	Women who worked in the last 12 months and were paid in cash (%)	120
# S121	Women owning a house and/or land (alone or jointly with others) (%)
# S122	Women having a bank or savings account that they themselves use (%)
# S123	Women having a mobile phone that they themselves use (%)


districts <- read_csv("data/districts_plus_uts.csv")
states <- read_csv("data/states_plus_nfhs3.csv")

pca_obj <- districts %>% 
  dplyr::select(id,sdistri,nfhs4d_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4d_total) %>% 
  dplyr::select(one_of(ds_ids)) %>%
  prcomp(.,scale. = TRUE)

nfhs4_d <- districts %>% 
  dplyr::select(id,sdistri,nfhs4d_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4d_total) %>% 
  dplyr::select(one_of(ds_ids)) %>% 
  predict(pca_obj,.)

nfhs4_s <- states %>% 
  dplyr::select(id,state,nfhs4s_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4s_total) %>% 
  dplyr::select(one_of(ds_ids)) %>% 
  predict(pca_obj,.)

nfhs3_s <- states %>% 
  dplyr::select(id,state,nfhs3s_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs3s_total) %>% 
  dplyr::select(one_of(ds_ids)) %>% 
  predict(pca_obj,.)

nfhs5_d <- districts %>% 
  dplyr::select(id,sdistri,nfhs5d_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs5d_total) %>% 
  dplyr::select(one_of(ds_ids)) %>% 
  predict(pca_obj,.)

nfhs5_s <- states %>% 
  dplyr::select(id,state,nfhs5s_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs5s_total) %>% 
  dplyr::select(one_of(ds_ids)) %>% 
  predict(pca_obj,.)

state_pc <- states %>% 
  dplyr::select(id,state,nfhs4s_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4s_total) %>% 
  dplyr::select(state) %>% 
  mutate(nfhs4s_pc1 = nfhs4_s[,1],
         nfhs5s_pc1 = nfhs5_s[,1],
         nfhs3s_pc1 = nfhs3_s[,1]
  )

district_pc <- districts %>% 
  dplyr::select(id,sdistri,nfhs4d_total) %>% 
  dplyr::filter(id %in% ds_ids) %>% 
  pivot_wider(names_from=id,values_from=nfhs4d_total) %>% 
  dplyr::select(sdistri) %>%
  mutate(nfhs4d_pc1 = nfhs4_d[,1],
         nfhs5d_pc1 = nfhs5_d[,1]
  )

write_csv(state_pc,"analysis/state_pc.csv")
write_csv(district_pc,"analysis/district_pc.csv")
