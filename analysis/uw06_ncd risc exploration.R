bmi_country2016 <- read_csv("C:/Cloud/OneDrive - Emory University/Papers/NCD RisC/trends/NCD_RisC_Lancet_2017_BMI_age_standardised_country.txt") %>%
  group_by(ISO) %>% 
  dplyr::filter(Year == 2016) %>% 
  ungroup() %>% 
  dplyr::select(-contains("uncertainty"))

bmi_country1975 <- read_csv("C:/Cloud/OneDrive - Emory University/Papers/NCD RisC/trends/NCD_RisC_Lancet_2017_BMI_age_standardised_country.txt") %>%
  group_by(ISO) %>% 
  dplyr::filter(Year == 1975) %>% 
  ungroup() %>% 
  dplyr::select(-contains("uncertainty"))

names(bmi_country2016) <- names(bmi_country1975) <- c("country","iso","sex","year",
                        "mean_bmi","obesity","severe_obesity",
                        "underweight","bmi_18to20","bmi_20to25",
                        "overweight","obesity_class1","obesity_class2",
                        "obesity_class3")

bmi_country2016 %>% 
  dplyr::mutate(owob = obesity + overweight) %>% 
  dplyr::mutate(status = case_when(owob > underweight ~ 1,
                                   TRUE ~ 0)) %>% 
  group_by(sex) %>% 
  dplyr::summarise(status = mean(status))


bmi_country1975 %>% 
  dplyr::mutate(owob = obesity + overweight) %>% 
  dplyr::mutate(status = case_when(owob > underweight ~ 1,
                                   TRUE ~ 0)) %>% 
  group_by(sex) %>% 
  dplyr::summarise(status = mean(status))

bmi_country2016 %>% 
  dplyr::mutate(status = case_when(obesity > underweight ~ 1,
                                   TRUE ~ 0)) %>% 
  group_by(sex) %>% 
  dplyr::summarise(status = mean(status))

bmi_country1975 %>% 
  dplyr::mutate(status = case_when(obesity > underweight ~ 1,
                                   TRUE ~ 0)) %>% 
  group_by(sex) %>% 
  dplyr::summarise(status = mean(status))
