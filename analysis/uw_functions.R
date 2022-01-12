district_plot <- function(col_name,plot_title,breaks = c(0,10,20,30,40,50,60)){
  
  tm_shape(shape_df2,ext=1.2) + 
    tm_borders() + tm_fill(title= "",
                           col=col_name,
                           palette="-RdYlGn",
                           style = "fixed",
                           breaks=breaks,
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
    tm_layout(plot_title,title.size = 2,
              legend.text.size = 1.2,
              legend.title.size = 1.2)
  
}

state_plot <- function(col_name,plot_title,breaks = c(0,10,20,30,40,50,60)){
  tm_shape(bound_df2,ext=1.2) + 
    tm_fill(title= "",
            col=col_name,
            palette="-RdYlGn",
            style = "fixed",
            breaks= breaks,
            # midpoint = NA,
            textNA="Data not available",
            colorNA = "white")+ 
    tm_borders(col="black") + 
    tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
    tm_legend(legend.position = c("right","top"),
              legend.outside=FALSE,
              legend.just=c("left","top"))+ 
    tm_xlab("") +
    tm_ylab("") +
    tm_layout(plot_title,title.size = 2,
              legend.text.size = 1.2,
              legend.title.size = 1.2)
}

states_scatter <- function(state_ids,plot_title,names = c("Underweight","Overweight")){
  
  states %>% 
    dplyr::filter(id %in% state_ids) %>% 
    dplyr::select(state,id,nfhs5s_total,nfhs4s_total) %>%
    
    mutate(
      
      id = case_when(id == state_ids[1] ~ names[1],
                     id == state_ids[2] ~ names[2],
                     TRUE ~ NA_character_)) %>% 
    ggplot(data=.,aes(x=nfhs4s_total,y=nfhs5s_total)) +
    geom_point(aes(col=id)) +
    geom_smooth(method="lm",aes(group=id,col=id),se = FALSE) +
    theme_bw() +
    geom_abline(intercept = 0,slope=1,col="black",linetype=2) +
    xlab("NFHS-4 (%)") +
    ylab("NFHS-5 (%)") +
    theme(legend.position = "bottom") +
    scale_color_discrete(name="")
  
  
}

states_scatter_change <- function(state_ids,plot_title,names = c("Underweight","Overweight")){
  
  states %>% 
    dplyr::filter(id %in% state_ids) %>% 
    dplyr::select(state,id,nfhs5s_total,nfhs4s_total) %>%
    
    mutate(
      
      id = case_when(id == state_ids[1] ~ names[1],
                     id == state_ids[2] ~ names[2],
                     TRUE ~ NA_character_),
      change5s4 = nfhs5s_total - nfhs4s_total) %>% 
    ggplot(data=.,aes(x=change5s4,y=nfhs5s_total)) +
    geom_point(aes(col=id)) +
    geom_smooth(method="lm",aes(group=id,col=id),se = FALSE) +
    theme_bw() +
    # geom_abline(intercept = 0,slope=1,col="black",linetype=2) +
    xlab("Change from NFHS-4 to NFHS-5 (%)") +
    ylab("NFHS-5 (%)") +
    scale_x_continuous(limits=c(-10,20)) +
    theme(legend.position = "bottom") +
    scale_color_discrete(name="")
  
  
}


districts_scatter_change <- function(district_ids,plot_title,names = c("Underweight","Overweight")){
  
  districts %>% 
    dplyr::filter(id %in% district_ids) %>% 
    dplyr::select(sdistri,id,nfhs5d_total,nfhs4d_total) %>%
    
    mutate(
      
      id = case_when(id == district_ids[1] ~ names[1],
                     id == district_ids[2] ~ names[2],
                     TRUE ~ NA_character_),
      change5d4 = nfhs5d_total - nfhs4d_total) %>% 
    ggplot(data=.,aes(x=change5d4,y=nfhs5d_total)) +
    geom_point(aes(col=id),size=0.5) +
    geom_smooth(method="lm",aes(group=id,col=id),se = FALSE) +
    theme_bw() +
    # geom_abline(intercept = 0,slope=1,col="black",linetype=2) +
    xlab("Change from NFHS-4 to NFHS-5 (%)") +
    ylab("NFHS-5 (%)") +
    scale_x_continuous(limits=c(-10,20)) +
    theme(legend.position = "bottom") +
    scale_color_discrete(name="")
  
  
}
