ds_ids = c("INDEX",
           "S07","S08","S09",
           "S10","S12",
           "S14","S16","S20_rev")

outcome_ids <- c("S86","S87","S88","S89")

district_pc <- read_csv("analysis/district_pc.csv") %>% 
  mutate(id = "INDEX") %>% 
  rename(nfhs5d_total = nfhs5d_pc1,
         nfhs4d_total = nfhs4d_pc1)

district_mapping <- readxl::read_excel("data/uw_mapping.xlsx",sheet="nfhs4 to nfhs5")


district_wide <- read_csv("data/districts_plus_uts.csv") %>% 
  dplyr::select(id,sdistri,nfhs5d_total,nfhs4d_total)  %>%
  bind_rows(district_pc) %>% 
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
             by=c("sdistri" = "sdistrict"))

library(geepack)

source("C:/code/external/functions/imputation/contrasts_geeglm.R")

model_summary <- bind_rows(
  (m1 <- geeglm(unhealthy ~ nfhs5*INDEX + urbanization,id = v024_nfhs5,data=district_wide,corstr="exchangeable")) %>% 
    broom::tidy(.) %>% 
    mutate(outcome = "UNHEALTHY"),
  
  (m2 <- geeglm(S86 ~ nfhs5*INDEX + urbanization,id = v024_nfhs5,data=district_wide,corstr="exchangeable")) %>% 
  broom::tidy(.) %>% 
    mutate(outcome = "S86"),
  (m3 <- geeglm(S88 ~ nfhs5*INDEX + urbanization,id = v024_nfhs5,data=district_wide,corstr="exchangeable")) %>% 
    broom::tidy(.) %>% 
    mutate(outcome = "S88")) %>% 
  mutate(coef_ci = paste0(estimate %>% round(.,2), "(",
                          (estimate - 1.96*std.error) %>% round(.,2),", ",
                          (estimate + 1.96*std.error) %>% round(.,2),")"
                          ))
  
model_summary %>% 
  dplyr::filter(term %in% c("nfhs5","INDEX","nfhs5:INDEX")) %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/table marginal models of human development.csv"))

bind_rows(
  contrasts_geeglm(m1,model_matrix=matrix(c(0,0,1,0,0,
                                            0,0,1,0,1,
                                            0,0,0,0,1),nrow=3,byrow=TRUE),
                   row_names = c("NFHS 4","NFHS 5","Difference")) %>% mutate(outcome = "UNHEALTHY"),
  contrasts_geeglm(m2,model_matrix=matrix(c(0,0,1,0,0,
                                            0,0,1,0,1,
                                            0,0,0,0,1),nrow=3,byrow=TRUE),
                   row_names = c("NFHS 4","NFHS 5","Difference")) %>% mutate(outcome = "S86"),
  contrasts_geeglm(m3,model_matrix=matrix(c(0,0,1,0,0,
                                            0,0,1,0,1,
                                            0,0,0,0,1),nrow=3,byrow=TRUE),
                   row_names = c("NFHS 4","NFHS 5","Difference")) %>% mutate(outcome = "S88")
) %>% 
  mutate(coef_ci = paste0(Estimate %>% round(.,2), " (",
                          LCI %>% round(.,2),", ",
                          UCI %>% round(.,2),")"
  )) %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from=outcome,values_from=coef_ci) %>% 
  write_csv(.,paste0(path_ecological_analysis,"/working/table contrasts of marginal models of human development.csv"))

district_wide %>% 
  dplyr::select(sdistri,nfhs5,INDEX,S86,S88,unhealthy) %>% 
  pivot_longer(cols=c("S86","S88","unhealthy"),names_to="id",values_to="prevalence") %>% 
  mutate(id = case_when(id == "S86" ~ "Underweight",
                        id == "S88" ~ "Overweight",
                        id == "unhealthy" ~ "Unhealthy Weight",
                        TRUE ~ NA_character_),
         nfhs5 = case_when(nfhs5 == 1 ~ "NFHS-5",
                           nfhs5 == 0 ~ "NFHS-4",
                           TRUE ~ NA_character_)) %>% 
  ggplot(data=.,aes(x=INDEX,y=prevalence,group=nfhs5,col=nfhs5)) +
  geom_point(alpha=0.8,size=0.5) +
  geom_smooth(method="lm",se=FALSE) + 
  facet_grid(~id) +
  theme_bw() +
  theme(legend.position="bottom") +
  xlab("Index of Human Development") +
  ylab("Prevalence (%)") +
  scale_color_discrete(name="")
  

