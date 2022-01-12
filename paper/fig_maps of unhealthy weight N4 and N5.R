source("paper/figaux01_merging with shapefiles.R")
source("analysis/uw_functions.R")

# Female State ------
tmap_save(state_plot("nfhs4s_f_unhealthyweight","A"),paste0(path_ecological_analysis,"/figures/fig1A.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_f_unhealthyweight","B"),paste0(path_ecological_analysis,"/figures/fig1B.png"),height=2300/300)

tmap_save(state_plot("nfhs4s_total_S86","C"),paste0(path_ecological_analysis,"/figures/fig1C.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S86","D"),paste0(path_ecological_analysis,"/figures/fig1D.png"),height=2300/300)
tmap_save(state_plot("nfhs4s_total_S88","E"),paste0(path_ecological_analysis,"/figures/fig1E.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S88","F"),paste0(path_ecological_analysis,"/figures/fig1F.png"),height=2300/300)

# Female district ------
tmap_save(district_plot("nfhs4d_f_unhealthyweight","A"),paste0(path_ecological_analysis,"/figures/fig2A.png"),height=2300/300)
tmap_save(district_plot("nfhs5d_f_unhealthyweight","B"),paste0(path_ecological_analysis,"/figures/fig2B.png"),height=2300/300)
tmap_save(district_plot("nfhs4d_total_S86","C"),paste0(path_ecological_analysis,"/figures/fig2C.png"),height=2300/300)
tmap_save(district_plot("nfhs5d_total_S86","D"),paste0(path_ecological_analysis,"/figures/fig2D.png"),height=2300/300)
tmap_save(district_plot("nfhs4d_total_S88","E"),paste0(path_ecological_analysis,"/figures/fig2E.png"),height=2300/300)
tmap_save(district_plot("nfhs5d_total_S88","F"),paste0(path_ecological_analysis,"/figures/fig2F.png"),height=2300/300)

# Male state ------
tmap_save(state_plot("nfhs4s_m_unhealthyweight","A"),paste0(path_ecological_analysis,"/figures/fig3A.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_m_unhealthyweight","B"),paste0(path_ecological_analysis,"/figures/fig3B.png"),height=2300/300)

tmap_save(state_plot("nfhs4s_total_S87","C"),paste0(path_ecological_analysis,"/figures/fig3C.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S87","D"),paste0(path_ecological_analysis,"/figures/fig3D.png"),height=2300/300)
tmap_save(state_plot("nfhs4s_total_S89","E"),paste0(path_ecological_analysis,"/figures/fig3E.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S89","F"),paste0(path_ecological_analysis,"/figures/fig3F.png"),height=2300/300)
