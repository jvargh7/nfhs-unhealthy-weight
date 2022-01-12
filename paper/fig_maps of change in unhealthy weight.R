source("paper/figaux01_merging with shapefiles.R")
source("analysis/uw_functions.R")

# Female State ------
tmap_save(state_plot("nfhs5s4_f_unhealthyweight","A",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4A.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_m_unhealthyweight","B",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4B.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S86","C",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4C.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S87","D",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4D.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S88","E",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4E.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S89","F",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4F.png"),height=2300/300)
