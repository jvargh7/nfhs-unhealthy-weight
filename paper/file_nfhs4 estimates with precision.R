districts <- read_csv("nfhs4/district long.csv")
states <- read_csv("nfhs4/state long.csv")
india <- read_csv("nfhs4/india long.csv")

bind_rows(districts,
          states,
          india %>% mutate(state = "India")) %>% 

write_csv(.,path = "data/supplementary file 2.csv")

bind_rows(districts,
          states,
          india %>% mutate(state = "India")) %>% 
  
  write_csv(.,path = paste0(path_ecological_analysis,"/writing/supplementary file 2.csv"))
