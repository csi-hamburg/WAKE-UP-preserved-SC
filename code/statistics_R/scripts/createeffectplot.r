temp <- dd %>% 
  group_by(visit,treatment) %>% 
  filter(nemoscore>0) %>%  
  mutate(lns = log(nemoscore)) %>% 
  summarise(mean.lns = mean(lns), sd.lns = sd(lns)/sqrt(n())) %>% 
  mutate(lower = exp(mean.lns - 1.96*sd.lns)
         , middle = exp(mean.lns)
         , upper = exp(mean.lns + 1.96*sd.lns))

df <- expand.grid(visit = unique(dd$visit), treatment = unique(dd$treatment)) %>% as_tibble()
fcn.assign <- function(visit, treatment)
  {
  visit <- as.character(visit)
  treatment <- as.character(treatment)
  if (visit == 'V0' & treatment == 'Placebo') { return(V0.Placebo) }
  if (visit == 'V0' & treatment == 'rtPA') { return(V0.Alteplase) }
  if (visit == 'V3' & treatment == 'Placebo') { return(V3.Placebo) }
  if (visit == 'V3' & treatment == 'rtPA') { return(V3.Alteplase) }
}
df.nest <- df %>% mutate(data  = pmap(., fcn.assign))

temp <- df.nest %>% unnest(cols = c(data)) %>% group_by(visit, treatment) %>% 
  summarise(lower = quantile(data, probs = 0.025) %>% exp()
            , middle = median(data) %>% exp()
            , upper = quantile(data, probs = 0.975) %>% exp())

pd <- position_dodge(0.1) # move them .05 to the left and right
p <- temp %>% ggplot(aes(x = visit %>% as.numeric(), y = middle, color=treatment, group = treatment)) +
  geom_segment(aes(xend = visit %>% as.numeric(), y = middle, yend = middle), x = .75, color = 'lightgrey', linetype = '33', size = 0.1) +
  geom_line(position = pd, size = .1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = pd, width=.2, size = .2) +
  geom_point(position = pd, shape = 21, size = .5, stroke = .5, fill = 'white') +
  scale_y_continuous(name = 'Structural disconnection (ChaCo score)', trans = 'log', breaks = NULL, labels = formatC(temp$middle, format = 'fg', digits = 3)) +
  scale_x_continuous(name = 'Time with respect to randomisation', breaks = c(1,2), labels = c('before','22-36 hours after'), limits = c(.75,2.25), expand = c(0,0)) +
  scale_color_manual(values = pal_npg()(4)[3:4]) +
  geom_text_repel(data = data.frame(x = .75, y = temp$middle, label = formatC(temp$middle, format = 'fg', digits = 3))
                  , aes(x = x, y = y, label = label), hjust = 0, nudge_x = -.3, direction = 'y', inherit.aes = FALSE, size = 2, xlim = c(0,3)
                  , segment.linetype = '33', segment.color = 'lightgrey', segment.size = .1, min.segment.length = 0) +
  guides(color = FALSE) + 
  theme_minimal() +
  coord_cartesian(xlim = c(.75,2.25), clip = 'off') +
  theme(text = element_text(family = 'Helvetica')
        , legend.direction = 'vertical', legend.position = c(.2,.9)
        , legend.background = element_blank()
        , axis.line = element_line(color = 'black', size = .2)
        , axis.ticks = element_line(color = 'black', size = .1)
        , panel.grid = element_blank()
        , legend.key.width = unit(1,'cm')
        , axis.title = element_text(size = 6)
        , axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0))
        , axis.text = element_text(size = 5)
        )
p

ggsave(paste0('./../../derivatives/figures/effectplot/effectggplot_', asz, '.eps'), plot = p, device = 'eps'
       , width = 8.5, height = 8.5, units = 'cm')
ggsave(paste0('./../../derivatives/figures/effectplot/effectggplot_', asz, '.svg'), plot = p, device = 'svg'
       , width = 8.5, height = 8.5, units = 'cm')
