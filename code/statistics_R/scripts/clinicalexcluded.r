
d.excluded <- gdata::read.xls('./../../clinical/Wake_up_imaging_data_masterfile_V5_update_RAPID_results_only_randomized.xlsx', sheet = 1) %>% 
  mutate(ID = stringr::str_sub(ID, 1, 8)) %>% 
  filter(lesion_location == 'mcaOrAca' & lesion_side != 'both' & !ID %in% dd$ID)

gdata::read.xls('./../../clinical/KeyStudyData_21-Nov-2017_short_version.xls', sheet = 1) %>% 
  rename('ID' = Subject.ID
         , 'mRS' = Visit.5.MRS
         , 'age' = Demographic.data..age
         , 'NIHSS' = Visit.0.NIHSS.sum.score) %>% 
  mutate(goodOutcome = mRS <= 1) %>% 
  filter(ID %in% d.excluded$ID) %>% droplevels() %>% 
  summarise(across(c(age, mRS, NIHSS), c(mean=mean, sd=sd, median=median, q=~quantile(., probs = c(.25, .5, .75), na.rm = TRUE))))
