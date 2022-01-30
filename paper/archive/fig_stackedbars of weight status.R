districts <- read_csv("chronic disease/districts_plus_uts.csv")

states <- read_csv("chronic disease/states.csv")

state_pc_levels <- read_csv("unhealthy weight/state_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  rename(nfhs5s_total = nfhs5s_pc1,
         nfhs4s_total = nfhs4s_pc1) %>% 
  arrange(nfhs5s_total) %>% 
  dplyr::select(state) %>% pull()

states_stacked <- function(state_ids,plot_title){
  
  states %>% 
    dplyr::filter(id %in% state_ids) %>% 
    dplyr::select(state,id,nfhs5s_total,nfhs4s_total) %>%
    mutate(state = factor(state,levels=state_pc_levels,labels=state_pc_levels)) %>% 
    pivot_longer(cols=-c(state,id),names_to="group",values_to="prevalence") %>% 
    mutate(group = case_when(group == "nfhs4s_total" ~ "NFHS-4",
                             group == "nfhs5s_total" ~ "NFHS-5"),
           id = case_when(id == state_ids[1] ~ "Underweight",
                          id == state_ids[2] ~ "Overweight",
                          TRUE ~ NA_character_)) %>% 
    ggplot(data=.,aes(x=prevalence,y=state)) +
    geom_col(aes(fill=id)) +
    facet_grid(~group) +
    theme_bw() +
    xlab("Prevalence (%)") +
    ylab("") +
    theme(legend.position = "bottom") +
    scale_fill_discrete(name="")
  
  
}


ggsave(paste0(path_ecological_analysis,"/figures/fig6A.png"),states_stacked(state_ids = c("S86","S88"),plot_title=NULL))
ggsave(paste0(path_ecological_analysis,"/figures/fig6B.png"),states_stacked(state_ids = c("S87","S89"),plot_title=NULL))
