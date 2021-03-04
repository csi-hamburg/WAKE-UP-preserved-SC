d.times <- read.csv('./../../clinical/mri_export%20V0%20V3.csv', sep = ';')
str(d.times)


require(tidyverse)
require(lubridate)

d.times %>% mutate(Date.and.time = ymd_hm(Date.and.time)) %>% 
  filter(Subject.ID %in% d$ID) %>% 
  group_by(Subject.ID) %>% 
  mutate(delta = hms::as.hms(Date.and.time - Date.and.time[Visit.Sequence == 'Visit 0'])) %>% 
  filter(Visit.Sequence == 'Visit 3') %>% 
  ungroup() %>% 
  summarise(m = mean(delta, na.rm = TRUE) %>% hms::as.hms()
            , sd = sd(delta, na.rm = TRUE) %>% hms::as.hms()
            , lwr = quantile(delta, probs = 0.25) %>% hms::as.hms()
            , mid = median(delta) %>% hms::as.hms()
            , upr = quantile(delta, probs = 0.75) %>% hms::as.hms())
