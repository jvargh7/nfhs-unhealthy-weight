source("unhealthy weight/uwaux01_merging with shapefiles.R")
source("unhealthy weight/uw_functions.R")



# FEMALE  ---------------

tmap_save(state_plot("nfhs4s_total_S86","A"),paste0(path_ecological_analysis,"/figures/fig1A.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S86","B"),paste0(path_ecological_analysis,"/figures/fig1B.png"),height=2300/300)
tmap_save(state_plot("nfhs4s_total_S88","C"),paste0(path_ecological_analysis,"/figures/fig1C.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S88","D"),paste0(path_ecological_analysis,"/figures/fig1D.png"),height=2300/300)


tmap_save(district_plot("nfhs4d_total_S86","A"),paste0(path_ecological_analysis,"/figures/fig3A.png"),height=2300/300)
tmap_save(district_plot("nfhs5d_total_S86","B"),paste0(path_ecological_analysis,"/figures/fig3B.png"),height=2300/300)
tmap_save(district_plot("nfhs4d_total_S88","C"),paste0(path_ecological_analysis,"/figures/fig3C.png"),height=2300/300)
tmap_save(district_plot("nfhs5d_total_S88","D"),paste0(path_ecological_analysis,"/figures/fig3D.png"),height=2300/300)


# MALE ---------------

tmap_save(state_plot("nfhs4s_total_S87","A"),paste0(path_ecological_analysis,"/figures/fig2A.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S87","B"),paste0(path_ecological_analysis,"/figures/fig2B.png"),height=2300/300)
tmap_save(state_plot("nfhs4s_total_S89","C"),paste0(path_ecological_analysis,"/figures/fig2C.png"),height=2300/300)
tmap_save(state_plot("nfhs5s_total_S89","D"),paste0(path_ecological_analysis,"/figures/fig2D.png"),height=2300/300)

# CHANGE STATE -----------

tmap_save(state_plot("nfhs5s4_total_S86","A",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4A.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S87","B",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4B.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S88","C",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4C.png"),height=2300/300)
tmap_save(state_plot("nfhs5s4_total_S89","D",breaks = c(-10,-5,0,5,10)),paste0(path_ecological_analysis,"/figures/fig4D.png"),height=2300/300)


with(states,cor.test(nfhs5s4_total_S86,nfhs5s4_total_S88))
with(states,cor.test(nfhs5s4_total_S87,nfhs5s4_total_S89))

with(districts,cor.test(nfhs5d4_total_S86,nfhs5d4_total_S88))

# NFHS-3 -----------

tmap_save(state_plot("nfhs3s_total_S86","A"),paste0(path_ecological_analysis,"/figures/fig5A.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S87","B"),paste0(path_ecological_analysis,"/figures/fig5B.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S88","C"),paste0(path_ecological_analysis,"/figures/fig5C.png"),height=2300/300)
tmap_save(state_plot("nfhs3s_total_S89","D"),paste0(path_ecological_analysis,"/figures/fig5D.png"),height=2300/300)

tmap_save(state_plot("nfhs4s3_total_S86","A",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig9A.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_total_S87","B",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig9B.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_total_S88","C",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig9C.png"),height=2300/300)
tmap_save(state_plot("nfhs4s3_total_S89","D",breaks = c(-35,-15,0,10,20)),paste0(path_ecological_analysis,"/figures/fig9D.png"),height=2300/300)
