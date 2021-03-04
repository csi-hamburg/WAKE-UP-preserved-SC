require(tidyverse)
dd <- read.csv(paste0('data_', asz, '.dat')) %>% 
  mutate(across(c(treatment, visit, ROI), as.factor)) %>% 
  mutate(treatment = factor(treatment, levels = c('Placebo', 'rtPA'), ordered = FALSE)
         , visit = factor(visit, levels = c('V0', 'V3'), ordered = FALSE)
         , lobe = factor(lobe, levels = c('Frontal','Parietal','Temporal','Occipital','Limbic','Subcortical')))

