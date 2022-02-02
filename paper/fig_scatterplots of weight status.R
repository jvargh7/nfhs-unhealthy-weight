source("papers/figaux02_appending unhealthy weight.R")

source("paper/uw_functions.R")


library(ggpubr)
ggsave(paste0(path_ecological_analysis,"/figures/fig6.png"),
       ggarrange(states_scatter(state_ids = c("S86","S88"),plot_title=NULL),
       states_scatter(state_ids = c("S87","S89"),plot_title=NULL),ncol = 2,nrow=1,
       common.legend = TRUE,legend = "bottom",labels = c("A","B")),width = 2000,height=1000,units="px"
       )
       
ggsave(paste0(path_ecological_analysis,"/figures/fig8.png"),
       districts_scatter_change(district_ids = c("S86","S88"),plot_title=NULL)
)


ggsave(paste0(path_ecological_analysis,"/figures/fig9.png"),
       ggarrange(states_scatter(state_ids = c("unhealthy_f","propf_overweight"),plot_title=NULL,
                                names = c("Unhealthy Weight","Share of Overweight")),
                 states_scatter(state_ids = c("unhealthy_m","propm_overweight"),plot_title=NULL,
                                names = c("Unhealthy Weight","Share of Overweight")),
                 ncol = 2,nrow=1,
                 common.legend = TRUE,legend = "bottom",labels = c("A","B")),width = 2000,height=1000,units="px"
)
