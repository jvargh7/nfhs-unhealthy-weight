ds_ids = c("INDEX",
           "S07","S08","S09",
           "S10","S12",
           "S14","S16","S20_rev")

outcome_ids <- c("S86","S87","S88","S89")

district_pc <- read_csv("analysis/district_pc.csv")

district_mapping <- readxl::read_excel("data/uw_mapping.xlsx",sheet="nfhs4 to nfhs5")


district_wide <- read_csv("data/districts_plus_uts.csv") %>% 
  dplyr::select(id,sdistri,nfhs5d_total,nfhs4d_total) %>% 
  pivot_longer(cols=contains("total"),names_to="group",values_to="prevalence")  %>% 
  mutate(nfhs5 = case_when(group == "nfhs5d_total" ~ 1,
                           TRUE ~ 0))  %>% 
  pivot_wider(names_from=id,values_from=prevalence) %>% 
  left_join(district_mapping %>% 
              dplyr::select(sdistri,nfhs5_statecode,v024_nfhs4) %>% 
              distinct(sdistri,.keep_all=TRUE) %>% 
              mutate(sdistri = as.numeric(sdistri)),
            by = "sdistri") %>% 
  mutate(v024_nfhs5 = case_when(nfhs5_statecode == "DD" ~ 8,
                                nfhs5_statecode == "LH" ~ 37,
                                TRUE ~ v024_nfhs4),
         unhealthy = S86 + S88) %>% 
  left_join( read_csv("data/census_2011_pop.csv") %>% 
               dplyr::select(sdistrict,urbanization),
             by=c("sdistri" = "sdistrict")) %>% 
  left_join(district_pc,
            by="sdistri") %>% 
  dplyr::select(sdistri,v024_nfhs5,nfhs5,S86,S88,unhealthy,urbanization,nfhs5d_pc1,nfhs4d_pc1) %>% 
  mutate(hdi_quintile = cut_number(nfhs4d_pc1,n=5))

# Change in prevalence boxplot ------------
source("paper/fig_quintiles of human development.R")



require(geepack)
model_summary <- bind_rows(
  (m1 <- geeglm(unhealthy ~ nfhs5*hdi_quintile + urbanization,id = v024_nfhs5,data=district_wide,corstr="exchangeable")) %>% 
    broom::tidy(.) %>% 
    mutate(outcome = "UNHEALTHY"),
  
  (m2 <- geeglm(S86 ~ nfhs5*hdi_quintile + urbanization,id = v024_nfhs5,data=district_wide,corstr="exchangeable")) %>% 
    broom::tidy(.) %>% 
    mutate(outcome = "S86"),
  (m3 <- geeglm(S88 ~ nfhs5*hdi_quintile + urbanization,id = v024_nfhs5,data=district_wide,corstr="exchangeable")) %>% 
    broom::tidy(.) %>% 
    mutate(outcome = "S88")) %>% 
  mutate(coef_ci = paste0(estimate %>% round(.,2), "(",
                          (estimate - 1.96*std.error) %>% round(.,2),", ",
                          (estimate + 1.96*std.error) %>% round(.,2),")"
  ))

model_summary %>% 
  # dplyr::filter(str_detect(term,"nfhs5")) %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci) %>% 
  dplyr::select(term,S86,S88,UNHEALTHY) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/table marginal models of human development BY QUINTILES.csv"))

source("C:/code/external/functions/imputation/contrasts_geeglm.R")

contrasts_models <- bind_rows(
  contrasts_geeglm(m1,model_matrix=matrix(c(0,1,0,0,0,0,0,0,0,0,0,
                                            0,1,0,0,0,0,0,1,0,0,0,
                                            0,1,0,0,0,0,0,0,1,0,0,
                                            0,1,0,0,0,0,0,0,0,1,0,
                                            0,1,0,0,0,0,0,0,0,0,1),nrow=5,byrow=TRUE),
                   row_names = paste0("Change in Q",1:5)) %>% mutate(outcome = "UNHEALTHY"),
  contrasts_geeglm(m2,model_matrix=matrix(c(0,1,0,0,0,0,0,0,0,0,0,
                                            0,1,0,0,0,0,0,1,0,0,0,
                                            0,1,0,0,0,0,0,0,1,0,0,
                                            0,1,0,0,0,0,0,0,0,1,0,
                                            0,1,0,0,0,0,0,0,0,0,1),nrow=5,byrow=TRUE),
                   row_names = paste0("Change in Q",1:5)) %>% mutate(outcome = "S86"),
  contrasts_geeglm(m3,model_matrix=matrix(c(0,1,0,0,0,0,0,0,0,0,0,
                                            0,1,0,0,0,0,0,1,0,0,0,
                                            0,1,0,0,0,0,0,0,1,0,0,
                                            0,1,0,0,0,0,0,0,0,1,0,
                                            0,1,0,0,0,0,0,0,0,0,1),nrow=5,byrow=TRUE),
                   row_names = paste0("Change in Q",1:5)) %>% mutate(outcome = "S88")
) %>% 
  mutate(coef_ci = paste0(Estimate %>% round(.,2), " (",
                          LCI %>% round(.,2),", ",
                          UCI %>% round(.,2),")"
  )) 

(contrasts_models %>%
  mutate(quintile = str_replace(term,"Change in ",""),
         outcome = factor(outcome,levels=c("S86","S88","UNHEALTHY"),
                          labels = c("Underweight","Overweight","Unhealthy weight"))) %>% 
  ggplot(data=.,aes(x=outcome,y=Estimate,ymin=LCI,ymax=UCI,fill=quintile)) + 
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9),width=0.2) +
  theme_bw() +
  xlab("") +
  ylab("Estimated change in Prevalence (%)") +
  scale_fill_manual(name="",values=c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba')) +
  theme(legend.position = c(0.2,0.8))) %>% 
  ggsave(.,filename = paste0(path_ecological_analysis,"/figures/model change in prevalence.png"),width=8,height=6)

  

contrasts_models %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci) %>% 
  dplyr::select(term, S86,S88, UNHEALTHY) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/table contrasts of marginal models of human development by QUINTILES.csv"))
