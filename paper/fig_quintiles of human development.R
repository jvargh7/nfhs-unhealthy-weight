
# Ref: analysis/uw07_marginal model by baseline levels.R ------------
(district_wide %>% 
  arrange(desc(nfhs5)) %>% 
  group_by(sdistri,hdi_quintile) %>% 
  dplyr::summarize_at(vars(S86,S88, unhealthy),function(x) x - dplyr::lead(x,1)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(S86)) %>% 
  pivot_longer(cols=one_of("S86","S88", "unhealthy"),names_to="id",values_to="val") %>% 
  mutate(hdi_quintile = factor(hdi_quintile,labels=paste0("Q",c(1:5))),
         id = factor(id,levels=c("S86","S88","unhealthy"),
                     labels=c("Underweight","Overweight","Unhealthy weight"))) %>% 
  ggplot(data=.,aes(x=id,y=val,fill=hdi_quintile)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Change in Prevalence (%)") +
  scale_fill_manual(name="",values=c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')) +
  theme(legend.position = c(0.33,0.8))) %>% 
  ggsave(.,filename = paste0(path_ecological_analysis,"/figures/change in prevalence.png"),width=8,height=6)







library(sp)
library(rgdal)
library(tmap)
path_shape_files <- "C:/Cloud/OneDrive - Emory University/data/NFHS/NFHS4 Factsheets/maps"
shape_df <-  readOGR(paste0(path_shape_files,"/sdr_subnational_boundaries_2020-12-28/shps"),"sdr_subnational_boundaries2")
bound_df <- readOGR(paste0(path_shape_files,"/maps-master/States"),"Admin2")


shape_df2 <- sp::merge(shape_df,district_wide %>% 
                         dplyr::filter(nfhs5==0) %>% 
                         mutate(hdi_quintile = factor(hdi_quintile,
                                                      labels = paste0("Q",c(1:5))
                                                      # labels=c("[-5.7, 1.8]",
                                                      #          "(-1.8, -0.6]",
                                                      #          "(-0.6, 0.6]",
                                                      #          "(0.6, 2.0]",
                                                      #          "(2.0, 5.2]")
                                                      
                                                      )),
                       by.x="REGCODE",by.y="sdistri",all.x=TRUE)
# bound_df is usable

figB <- tm_shape(shape_df2,ext=1.2) + 
  tm_borders() + tm_fill(title= "",
                         col="hdi_quintile",
                         palette=c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba'),
                         style = "fixed",
                         # midpoint = NA,
                         textNA="Data not available",
                         colorNA = "white")+ 
  tm_shape(bound_df) + tm_borders(col="black") + 
  tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
  tm_legend(legend.position = c("right","top"),
            legend.outside=FALSE,
            legend.just=c("left","top"))+ 
  
  tm_xlab("") +
  tm_ylab("") +
  tm_layout("",title.size = 2,
            legend.text.size = 1.2,
            legend.title.size = 1.2)

tmap_save(figB,paste0(path_ecological_analysis,"/figures/hdi quintiles in 2015.png"),height=2300/300)
