source("paper/figaux01_merging with shapefiles.R")
source("analysis/uw_functions.R")


tmap_save(state_plot("nfhs3s_f_unhealthyweight","A"),paste0(path_ecological_analysis,"/figures/fig5A.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_m_unhealthyweight","B"),paste0(path_ecological_analysis,"/figures/fig5B.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S86","C"),paste0(path_ecological_analysis,"/figures/fig5C.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S87","D"),paste0(path_ecological_analysis,"/figures/fig5D.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S88","E"),paste0(path_ecological_analysis,"/figures/fig5E.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S89","F"),paste0(path_ecological_analysis,"/figures/fig5F.png"),height=2300/300)


# change from NFHS3 to NFHS4 ------
tmap_save(state_plot("nfhs4s3_f_unhealthyweight","A",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig6A.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_m_unhealthyweight","B",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig6B.png"),height=2300/300)

tmap_save(state_plot("nfhs4s3_total_S86","C",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig6C.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_total_S87","D",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig6D.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_total_S88","E",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig6E.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_total_S89","F",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig6F.png"),height=2300/300)
