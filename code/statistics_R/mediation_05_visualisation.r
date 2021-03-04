pl.models.tile <- df.IPW %>%
  merge(mdls.df) %>% 
  mutate(m = ifelse(m != " + delta.vol", m, NA)) %>% 
  ggplot(aes(x = x, y = y, fill = reorder(m,-est))) +
  geom_tile() +
  scale_fill_hue(name='Model index', na.value = 'white') +
  scale_x_continuous('', breaks = 1:min1, limits = c(-5,min1+1), expand = c(0,0)
                     , labels = ROI.short.dict[ROI.stat.T %>% slice_head(n = min1) %>% pull('ROI') %>% as.vector()] %>% unlist()) +
  scale_y_continuous('', breaks = 1:min2, limits = c(-5.5,min2+1), expand = c(0,0)
                     , labels = ROI.short.dict[ROI.stat.O %>% slice_head(n = min2) %>% pull('ROI') %>% as.vector()] %>% unlist()) +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, min2+1), xlim = c(0,min1+1), clip="off") +
  geom_polygon(data = data.frame(x = c(.5, .5, min1+.5), y = c(-3.5,-5.5,-4.5)), aes(x=x, y=y), fill='white', color = 'black', alpha = 0, size = .1)+
  annotate(geom = "text", x = .5, y = -4.5, label = 'Association~Treatment%~%Delta[~ChaCo]', parse = TRUE, hjust = -.01, vjust = .5, size = 1.5) +
  geom_polygon(data = data.frame(x = c(-min1/3+.75, -min1/3-.75, -min1/3), y = c(.5, .5, min2)), aes(x=x, y=y), fill='white', color = 'black', alpha = 0, size = .1)+
  annotate(geom = "text", y = .5, x = -min1/3, label = 'Association~Delta[~ChaCo]%~%Outcome', parse = TRUE, hjust = -.01, vjust = .5, angle = 90, size = 1.5) +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica')
        , aspect.ratio = 1
        , panel.grid = element_blank()
        , axis.text.x = element_text(angle = 90)
        , panel.border = element_rect(fill = FALSE)
        , plot.margin = unit(c(.1,.1,1,1), "cm")
        , axis.text = element_text(size = 6))
pl.models.tile


delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}

intersections <- df.IPW %>% 
  merge(mdls.df) %>% 
  arrange(-est) %>% 
  dplyr::select(-x, -y) %>% 
  distinct() %>% 
  pull('m') %>% 
  str_split(' \\+ ') %>% 
  lapply(function(x){x %>% setdiff(c("", "delta.vol")) %>% as.list()}) %>% 
  delete.NULLs()

cols <- scales::hue_pal()(length(intersections))
queries <- mapply(function(i,c){upset_query(intersect = unlist(i)
                                            , color = c
                                            , only_components = c('intersections_matrix'))}, intersections, cols, SIMPLIFY = FALSE)
data.upset <- df.IPW %>%
  mutate(meds = strsplit(m, " \\+ ")
         , dummy = 1) %>% 
  unnest(meds) %>%
  filter(!meds %in% c('', 'delta.vol')) %>% 
  pivot_wider(names_from = meds, values_from = dummy, values_fill = 0) %>% 
  merge(df.thr %>% 
          mutate(m = map_chr(id, ~df.IPW$m[.])) %>% 
          filter(hyposet == 0)
  ) 

pl.upset <- upset(data = data.upset
      , intersect = mdls.df %>% pull('m') %>% unique() %>% str_split(' \\+ ') %>% unlist() %>% unique() %>% setdiff(c("", "delta.vol"))
      , sort_sets = FALSE
      , queries = queries
      , sort_intersections = 'ascending'
      , base_annotations = NULL
      , set_sizes = FALSE
      , name = 'Model index'
      , height_ratio = 1
      , wrap = FALSE
      , labeller = as_labeller(ROI.short.dict)
      , stripes = c('white', 'white')
      , dot_size = 1.3
      , themes = upset_modify_themes(
        list(
          'intersections_matrix' = theme(text = element_text(family = 'Arial')
                                         , panel.grid = element_blank()
                                         , axis.text.x = element_text(angle = 0)
                                         , axis.title = element_text(size = 6)
                                         , axis.text = element_text(size = 6))
          , 'overall_sizes' =  theme(text = element_text(family = 'Arial')
                                     , panel.grid = element_blank())
          , 'FDR' = theme(text = element_text(family = 'Arial')
                          , axis.title.y = element_text(size = 6)
                          , axis.text.x = element_blank()
                          , axis.title.x = element_blank())
          , 'NIE' = theme(text = element_text(family = 'Arial')
                          , axis.title.y = element_text(size = 6)
                          , axis.text.x = element_blank()
                          , axis.title.x = element_blank()
                          )
        )
      )
)
pl.upset

pl.NIE <- data.upset %>% 
  filter(comparator == '1') %>% 
  ggplot(aes(x = orderid, y = propOR, color =  reorder(orderid,-est))) +
           geom_segment(x=-Inf, xend=Inf, y=0, yend=0, color='black', size=1, linetype='dashed')+
           geom_errorbar(aes(ymin = lwr.BC.propOR, ymax = upr.BC.propOR, width = .5))+
           geom_point()+
           geom_text(data=. %>% filter(rep=="rep1"), aes(label = scales::percent(propOR, accuracy = .1), y = upr.BC.propOR), vjust = -.5, size = 1.5)+
           geom_text(data=. %>% filter(rep=="rep1"), aes(label = ifelse(lwr.BC.propOR > 0, "*", ""), y = upr.BC.propOR+0.05), vjust = -.5, size = 2, color = 'black')+
           scale_color_hue()+
           scale_x_continuous('Model index', breaks = 1:(length(allmdls.unique)-1)) +
           scale_y_continuous('Proportion jointly mediated', labels = scales::percent)+
           coord_cartesian(ylim = c(-.1,1), clip = 'off')+
           guides(color = FALSE) +
           theme_minimal() +
  theme(text = element_text(family = 'Arial')
        , panel.grid.minor = element_blank()
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6))

pl.NIE

pl.vol <- data.upset %>% 
  filter(comparator == 'vol') %>% 
  ggplot(aes(x = orderid, y = est.vol, color =  reorder(orderid,-est))) +
  geom_segment(x=-Inf, xend=Inf, y=1, yend=1, color='black', size=1, linetype='dashed')+
  geom_errorbar(aes(ymin = lwr.BC.vol, ymax = upr.BC.vol), width = .5)+
  geom_point()+
  geom_text(data=. %>% filter(rep=="rep1"), aes(label = scales::number(est.vol, accuracy = .01), y = upr.BC.vol), vjust = -.5, size = 1.5)+
  geom_text(data=. %>% filter(rep=="rep1"), aes(label = ifelse(lwr.BC.vol > 1, "*", ""), y = upr.BC.vol + 0.01), vjust = -.5, size = 2, color = 'black') +
  scale_color_hue()+
  scale_x_continuous('Model index', breaks = 1:(length(allmdls.unique)-1), expand = c(0,0)) +
  scale_y_continuous(as.expression('Excess effect of connectivity change')) +
  coord_cartesian(ylim = c(.9, 1.4), clip = "off")+
  guides(color = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = 'Arial')
        , panel.grid.minor = element_blank()
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6))
pl.vol
  

pl.FDR <- df.thr %>% ungroup() %>% 
  mutate(m = map_chr(id, ~df.IPW$m[.])) %>% 
  filter(hyposet == 0 & comparator == '1') %>% 
  ggplot(aes(x = orderid, color = as.factor(orderid))) + 
  stat_summary(
    aes(y = thr + 1, fill = ordered(stat(.width)), group = -stat(.width)),
    geom = "step2ribbon", fun.data = median_qi, fun.args = list(.width = c(.95)), alpha = .5, direction = 'mid', color = NA) +
  scale_fill_grey() +
  geom_step(aes(y = 1 + meanthr), direction = 'mid', color = 'black') + 
  geom_point(aes(y = 1 + T), shape = 25, size = 2) +
  scale_x_continuous('Model index', breaks = 1:(s+1)) +
  scale_y_continuous('Step down rejection threshold (FDR 5%)', limits = c(1,1.2), expand = c(0,0)) +
  scale_color_hue()+
  guides(fill = FALSE, color = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = 'Arial')
        , legend.position = c(0.8, .8)
        , panel.grid.minor = element_blank()
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6))
pl.FDR


## number of manually defined models
noMM <- 0
if (asz == 116)
  noMM <- 1

pl.cor.cortical <- df.cor %>% 
  filter(comparator == '1') %>% 
  mutate(lab = case_when(x < y ~ scales::number(c, accuracy = .01)
                         , x == y ~ paste0('Mdl ', rep(1:(nrow(df.IPW)-1-noMM), nrow(df.IPW)-1-noMM)) %>% as.character()
                         , TRUE ~ '')
         , c = case_when(x > y ~ c
                         , x == y ~ 0
                         , TRUE ~ .5)
  ) %>% 
  ggplot(aes(x = x, y = y, fill = c)) +
  geom_tile() +
  geom_text(aes(x=x, y=y, label = lab), size = 1.5) +
  scale_fill_material('teal', name = '', limits = c(0.5, 1), breaks = c(.5, .75, 1)) +
  scale_x_continuous('', breaks = 1:s, expand = c(0,0), labels = NULL) +
  scale_y_continuous('', breaks = 1:s, expand = c(0,0), labels = NULL) + 
  facet_wrap( ~ hyposet ) +
  guides(fill = FALSE) + 
  theme_minimal() +
  theme(text = element_text(family = 'Arial')
        , panel.grid = element_blank()
        , aspect.ratio = 1
        , strip.text = element_blank()
        , axis.text = element_text(size = 8)
        , legend.position = c(-.2,.5)
  )

pl.cor.cortical


cols <- scales::hue_pal()(nrow(df.IPW)-1)

if (asz == 116)
  cols = c('black', cols[1:(length(cols)-1)])


if (asz == 86)
  breaks.manual <- c(0,1, 1.04,1.08,1.11,1.13,1.15,1.19)
if (asz == 116)
  breaks.manual <- c(0,1, 1.04,1.07,1.09,1.15)



pl.NIE.null <- NIE.joint.00 %>% tibble() %>% 
  setNames('NIE') %>% 
  filter(NIE < 1.2 & NIE > .8 & !is.na(NIE)) %>% 
  ggplot(aes(x = NIE), color = 'black') +
  mapply(function(est,i)geom_segment(x = est, xend = est, y=0, yend = Inf, color = cols[i]), df.IPW$est[df.IPW$p <= 1 & df.IPW$m != ' + delta.vol'] %>% sort(), (nrow(df.IPW)-1):1) +
  geom_segment(x = df.IPW$est[df.IPW$m == ' + delta.vol'], xend = df.IPW$est[df.IPW$m == ' + delta.vol'], y = 0, yend = Inf, color = 'black', size = .5) +
  geom_segment(x = 1, xend = 1, y = 0, yend = Inf, color = 'black', size = .5, linetype = 'dashed') +
  geom_density(fill="red", alpha = 0.2) +
  geom_histogram(aes(y = ..density..), binwidth = density(NIE.joint.00[!is.na(NIE.joint.00)])$bw, alpha = .8) +
  annotate(geom = 'text', x = df.IPW$est[df.IPW$m == ' + delta.vol'], y = 5, color = 'black', label = 'Infarct growth', angle = 0, vjust = -.5, hjust = 1, size = 1.5) + 
  annotate(geom = 'text', x = 1, y = 10, color = 'black', label = 'No effect', angle = 0, vjust = -.5, hjust = 1, size = 1.5) + 
  geom_label_repel(data = df.IPW %>% filter(m != ' + delta.vol') %>% mutate(label = map(m,f), cols = cols), aes(x = est, y = 10, label = label)
                   , direction = 'x', nudge_y = 0, size = 1, segment.size = 0, force = 10, segment.alpha = 1) +
  scale_x_continuous('Natural indirect effect', breaks = breaks.manual, expand = expansion(mult = 0, add = c(.01, .01))) + 
  scale_y_continuous('', limits = c(0,12), expand = c(0,0), labels = NULL) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = 'Helvetica')
        , panel.grid = element_blank()
        , axis.line.y = element_line()
        , axis.title = element_text(size = 6)
        , axis.text = element_text(size = 6))



### arrange plots
layout <- "ABG
           CDG
           EFG"
pl.models.tile + pl.upset + pl.NIE  + 
  pl.cor.cortical + pl.FDR +  
  pl.vol + pl.NIE.null +
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'a', tag_suffix = '') &
  theme(text = element_text(family = 'Helvetica'))


ggsave(paste0('./../../derivatives/figures/mediation/mmpanel_', asz, '.svg'), device = 'svg', width = 18, height = 18, units = 'cm')
ggsave(paste0('./../../derivatives/figures/mediation/mmpanel_', asz, '.eps'), device = 'eps', width = 18, height = 18, units = 'cm')

#ggsave(paste0('./../../derivatives/figures/mediation/mmpanel_', asz, '.tiff'), device = 'tiff', width = 18, height = 18, units = 'cm', dpi = 600)

 

