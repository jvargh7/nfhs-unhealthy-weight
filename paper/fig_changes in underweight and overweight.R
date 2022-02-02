source("paper/figaux02_appending unhealthy weight.R")
source("paper/uw_functions.R")
state_pc <- read_csv("analysis/state_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  rename(nfhs5s_total = nfhs5s_pc1,
         nfhs4s_total = nfhs4s_pc1)
district_pc <- read_csv("analysis/district_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  rename(nfhs5d_total = nfhs5d_pc1,
         nfhs4d_total = nfhs4d_pc1)


figA <- bind_rows(states,
          state_pc) %>% 
  states_dots_change(.,state_ids = c("S86","S87"))
figB <- bind_rows(states,
                  state_pc) %>% 
  states_dots_change(.,state_ids = c("S88","S89"),yaxis="none")

require(ggpubr)

ggarrange(figA,
          figB,widths = c(1.8,1),
          common.legend = TRUE,
          legend="bottom",
          labels=c("A","B")) %>% 
  ggsave(.,file=paste0(path_ecological_analysis,"/figures/figure 10.png"),width = 12,height=14)
