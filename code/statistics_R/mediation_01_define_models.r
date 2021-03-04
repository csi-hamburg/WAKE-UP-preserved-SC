
# pre-treatment covariates
covs <- c('age',  'NIHSS', 'logvol0')

df <- dd %>%
  ungroup() %>% 
  group_by(ROI, ID) %>% 
  mutate(delta.nemo = nemoscore - nemoscore[visit=='V0']
         , logvol0 = log(vol0)
         , delta.vol = log(lesionvolume) - log(lesionvolume[visit == 'V0'])
  ) %>% 
  ungroup() %>% 
  filter(visit == 'V3') %>% 
  filter(is.finite(delta.nemo)) %>% 
  dplyr::select(ID, visit, goodOutcome, treatment, delta.nemo, ROI, delta.vol, all_of(covs)) %>% 
  spread(ROI, delta.nemo, fill = 0) %>% 
  dplyr::rename(x = treatment, y = goodOutcome) %>% 
  mutate(x = forcats::fct_recode(x, c = 'Placebo', t = 'rtPA')) %>% 
  na.omit()


nrow(df)

# association functional outcome ~ infarct growth
glm(y ~ log2(exp(delta.vol)) + x + age + NIHSS + logvol0, data = df, family = binomial) %>% 
  tidy(exponentiate=TRUE, conf.int=TRUE)

ROI.stat.O <- dd %>%
  group_by(ID,ROI) %>% 
  mutate(delta.nemo = nemoscore - nemoscore[visit=='V0']
         , logvol0 = log(vol0)) %>% 
  ungroup() %>% 
  filter(visit == 'V3') %>% 
  filter(is.finite(delta.nemo)) %>% 
  dplyr::select(goodOutcome, treatment, delta.nemo, ROI, all_of(covs)) %>% 
  group_by(ROI) %>% 
  mutate(delta.nemo = scale(delta.nemo)) %>% 
  nest() %>% 
  mutate(mdl = map(data, ~glm(goodOutcome ~ delta.nemo + age + NIHSS + logvol0, family = binomial(), data = .))
         , tidy = map(mdl, ~tidy(., exponentiate = TRUE, conf.int = TRUE))) %>% 
  unnest(tidy) %>% 
  ungroup() %>% 
  dplyr::filter(term == 'delta.nemo') %>% 
  arrange(statistic) %>% 
  dplyr::select(ROI, statistic, estimate, conf.low, conf.high)
ROI.stat.O

ROI.stat.T <- dd %>%
  group_by(ID,ROI) %>% 
  mutate(delta.nemo = nemoscore - nemoscore[visit=='V0']
         , logvol0 = log(vol0)) %>% 
  ungroup() %>% 
  filter(visit == 'V3') %>% 
  filter(is.finite(delta.nemo)) %>% 
  dplyr::select(goodOutcome, treatment, delta.nemo, ROI, all_of(covs)) %>% 
  group_by(ROI) %>%
  mutate(delta.nemo = scale(delta.nemo)) %>% 
  nest() %>% 
  mutate(mdl = map(data, ~MASS::rlm(delta.nemo ~ treatment + age + NIHSS + logvol0, data = .))
         , tidy = map(mdl, tidy)
         , CI = map(mdl, ~broom::confint_tidy(., func = stats::confint.default) )) %>% 
  unnest(c(tidy, CI)) %>% 
  ungroup() %>% 
  dplyr::filter(term == 'treatmentrtPA') %>% 
  arrange(statistic) %>% 
  dplyr::select(ROI, statistic, estimate, conf.low, conf.high)
ROI.stat.T


# total unmber of distinct models examined
f <- function(n.T, n.O, g = intersect){ROI.stat.T %>% 
    slice_head(n = n.T) %>% 
    pull('ROI') %>% sort() %>% as.vector() %>% 
    g(ROI.stat.O %>% 
        slice_head(n = n.O) %>% 
        pull('ROI') %>% sort() %>% as.vector()
    ) %>% unique() %>% sort() %>% paste(collapse = ' + ') %>% paste0(' + delta.vol')
}

if (asz == 86){
  min1 <- 9
  min2 <- 15  
} else if (asz == 116){
  min1 <- 6
  min2 <- 12
}


allmdls <- outer(1:min1
                 , 1:min2
                 , function(x,y)mapply(f,x,y)
)

allmdls.unique <- allmdls %>% as.vector() %>% unique()
allmdls.unique %>% length()

# manual model for AAL atlas, to reflect best model from DK atlas.
if (asz == 116){
  allmdls.unique <- c(allmdls.unique, 'Frontal_Inf_Orb + Cingulum_Ant + Cingulum_Mid + Cingulum_Post + SupraMarginal + Angular + delta.vol')  
}


mdls.df <- NULL
idx <- sapply(allmdls.unique, function(x)which(allmdls==x, arr.ind = TRUE))
for (mdl in allmdls.unique){
  mdls.df <- bind_rows(mdls.df, tibble(x = idx[[mdl]][,'row']
                                       , y = idx[[mdl]][,'col']
                                       , m = mdl
                                       , id = which(allmdls.unique == mdl))
  )
}


# similarity of rankings by alteplase-reponsivity and clinical eloquence
cor.test(sapply(ROI.stat.O$ROI, function(x)which(x == ROI.stat.T$ROI)), 1:nrow(ROI.stat.O))


cols <- scales::hue_pal()(length(allmdls.unique))
c1 <- ROI.stat.T %>% merge(dd %>% dplyr::select(ROI,lobe) %>% distinct()) %>% arrange(statistic) %>% 
  mutate(col1 = cols[as.numeric(lobe)]) %>% pull('col1')
c2 <- ROI.stat.O %>% merge(dd %>% dplyr::select(ROI,lobe) %>% distinct()) %>% arrange(statistic) %>% 
  mutate(col2 = cols[as.numeric(lobe)]) %>% pull('col2')


## visualise rankings as table and indicate matching regions

d.tab <- ROI.stat.T %>% merge(dd %>% dplyr::select(ROI,lobe) %>% distinct()) %>% arrange(statistic) %>% rowid_to_column('id1') %>%  
  bind_cols(ROI.stat.O %>% merge(dd %>% dplyr::select(ROI,lobe) %>% distinct()) %>% arrange(statistic) %>% rowid_to_column('id2')) %>% 
  setNames(c('id1', 'ROI1','s1', 'e1', 'cl1', 'ch1','lobe1','id2','ROI2','s2', 'e2','cl2', 'ch2', 'lobe2')) %>% 
  mutate(matchid = map_dbl(ROI1,function(x)which(x==ROI2))
         , matchid2 = map_dbl(ROI2,function(x)which(x==ROI1))
         , dummy = 'bla'
         , alpha = ifelse(id1 <= min1 & matchid <= min2, 1, .5)
         , alpha2 = ifelse(id2 <= min2 & matchid2 <= min1, 1, .5)
         , textcolor = ifelse(id1 <= min1, 'black', 'darkgrey')
         , textcolor2 = ifelse(id2 <= min2, 'black', 'darkgrey'))

w <- .5

d.tab %>%   
ggplot() +
  geom_rect(aes(xmin = -3/2*w, xmax = -w/2, ymin = 43 - id1 - .45, ymax = 43 - id1 + .45, fill = lobe1, alpha = alpha)) +
  geom_text(data = . %>% filter(id1 == 1), aes(x = -3/2*w, y = 44), label = 'Association~Delta[~~ChaCo]%~%Treatment', hjust = .5, size = 3, fontface = 2, parse = TRUE) +
  geom_text(data = . %>% filter(id1 == 1), aes(x = -w, y = 43), label = 'Region', hjust = .5, size = 3) +
  geom_text(aes(x = -w - .04, y = 43-id1, label = ROI1), color = d.tab$textcolor, hjust = .5, size = 2.5) +
  
  geom_text(data = . %>% filter(id1 == 1), aes(x = -2.25*w, y = 43), label = 'Beta', hjust = .5, size = 3) +
  geom_text(data = . %>% filter(id1 == 1), aes(x = -1.75*w, y = 43), label = '95% CI', hjust = .5, size = 3) +
  geom_text(aes(x = -2.25*w, y = 43-id1, label = scales::number(e1, accuracy = 0.01)), color = d.tab$textcolor, hjust = .5, size = 3) +
  geom_text(aes(x = -2*w - 0.05, y = 43-id1, label = paste0('['
                                                               , scales::number(cl1, accuracy = 0.01)
                                                               , ',')), color = d.tab$textcolor, hjust = 0, size = 3) +
  geom_text(aes(x = -1.6*w + 0.02, y = 43-id1, label = paste0(scales::number(ch1, accuracy = 0.01)
                                                               , ']')), color = d.tab$textcolor, hjust = 1, size = 3) +
  
  geom_rect(aes(xmin = w/2, xmax = 3/2*w, ymin = 43 - id2-.45, ymax = 43 - id2 + .45, fill = lobe2, alpha = alpha2)) +
  geom_text(data = . %>% filter(id2 == 1), aes(x = 3/2*w, y = 44), label = 'Association~Outcome%~%Delta[~~ChaCo]', hjust = .5, size = 3, fontface = 2, parse = TRUE) +
  geom_text(data = . %>% filter(id2 == 1), aes(x = w, y = 43), label = 'Region', hjust = .5, size = 3) +
  geom_text(aes(x = w - 0.04, y = 43-id2, label = ROI2), color = d.tab$textcolor2, hjust = .5, size = 2.5) +
  geom_text(data = . %>% filter(id2 == 1), aes(x = 1.75*w, y = 43), label = 'OR', hjust = .5, size = 3) +
  geom_text(data = . %>% filter(id2 == 1), aes(x = 2.25*w, y = 43), label = '95% CI', hjust = .5, size = 3) +
  geom_text(aes(x = 1.75*w, y = 43-id2, label = scales::number(e2, accuracy = 0.01)), color = d.tab$textcolor2, hjust = .5, size = 3) +
  geom_text(aes(x = 2.25*w - 0.02, y = 43-id2, label = paste0('['
                                                              , scales::number(cl2, accuracy = 0.01)
                                                              , ',')), color = d.tab$textcolor2, hjust = 1, size = 3) +
  geom_text(aes(x = 2.25*w + 0.02, y = 43-id2, label = paste0(scales::number(ch2, accuracy = 0.01)
                                                              , ']')), color = d.tab$textcolor2, hjust = 0, size = 3) +
  
  
  geom_segment(x = -2.5*w, xend = -w/2, y = 42.5 - min1, yend = 42.5 - min1, color = 'black') +
  geom_segment(x = w/2, xend = 2.5*w, y = 42.5 - min2, yend = 42.5 - min2, color = 'black') +
  geom_segment(aes(x = -w/2, xend = w/2, y = 43 - id1, yend = 43 - matchid
                   , color = lobe1
                   , alpha = alpha), size = 2, lineend = 'round') +
  scale_x_continuous('',  limits = c(-2.4*w, 2.5*w), breaks = NULL, expand = c(0,0)) +
  scale_y_continuous('', breaks = NULL) +
  guides(color = FALSE, fill = FALSE, alpha = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica')
        , panel.grid = element_blank())

#ggsave(paste0('./../../derivatives/figures/mediation/selectROIs_', asz, '.tiff'), device = 'tiff', width = 19.5, height = 25, units = 'cm', dpi = 1200)
ggsave(paste0('./../../derivatives/figures/mediation/selectROIs_', asz, '.eps'), device = 'eps', width = 18, height = 24, units = 'cm')
ggsave(paste0('./../../derivatives/figures/mediation/selectROIs_', asz, '.svg'), device = 'svg', width = 18, height = 24, units = 'cm')
