districts <- read_csv("chronic disease/districts_plus_uts.csv")

states <- read_csv("chronic disease/states.csv")


source("unhealthy weight/uw_functions.R")


library(ggpubr)
ggsave(paste0(path_ecological_analysis,"/figures/fig6.png"),
       ggarrange(states_scatter(state_ids = c("S86","S88"),plot_title=NULL),
       states_scatter(state_ids = c("S87","S89"),plot_title=NULL),ncol = 2,nrow=1,
       common.legend = TRUE,legend = "bottom",labels = c("A","B")),width = 2000,height=1000,units="px"
       )
       
ggsave(paste0(path_ecological_analysis,"/figures/fig8.png"),
       districts_scatter_change(district_ids = c("S86","S88"),plot_title=NULL)
)
