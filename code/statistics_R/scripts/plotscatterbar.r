d.plot <- dd.delta %>% 
  ungroup() %>% 
  mutate(ROI = forcats::fct_reorder(ROI,p)) %>% 
  unnest(data) %>% 
  mutate(ROI = forcats::fct_reorder(ROI,as.numeric(lobe))) %>% 
  #filter(ROI %in% levels(dd$ROI)[1:42]) %>% 
  dplyr::select(-c(lesionvolume,llv, delta.nemo)) 

fancy_scientific <- function(l) {
  l <- format(l, scientific = TRUE)
  l <- gsub("^(.*)e", "e", l)
  l <- gsub("e", "10^", l)
  parse(text=l)
}

main.plot <- d.plot %>% 
  filter(nemoscore > 0) %>% 
  spread(visit, nemoscore) %>% 
  ggplot(aes(x = V0, y = V3, color = as.factor(lobe), shape = treatment))+
  scale_x_continuous('ChaCo score before randomisation', trans = 'log10', labels=fancy_scientific) +
  scale_y_continuous('ChaCo score 22-36 hours after randomisation', trans = 'log10', labels=fancy_scientific) +
  geom_point(alpha = .2) + 
  coord_fixed() + 
  geom_rug(alpha = .05, color = 'black') +
  stat_function(data = data.frame(x = c(1e-10,1)), fun = function(x)x, color='black', linetype = 3, inherit.aes = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(.~ROI, scales = 'fixed', ncol = 6) + 
  theme(text = element_text(size = 10, family = 'Helvetica')
        , axis.text = element_text(size = 8)
        , strip.text.x = element_text(size = 6)) + 
  guides(color = FALSE, shape = FALSE)

main.plot

## A function to plot the inset 
if(asz == 86)
  offset <- -.3
if(asz == 116)
  offset <- -.5

get_inset <- function(df){
  p <- df %>% 
    group_by(ROI,visit, treatment) %>% 
    mutate(proppos = mean(nemoscore>0)) %>% 
    ggplot(aes(x = treatment, y = proppos, fill = visit, group = visit))+
    geom_col(position = position_dodge(width = .8), width = 0.5, color = 'darkgrey', size = .1, alpha = .2)+
    geom_text(data = . %>% ungroup()  %>% group_by(ROI, treatment) %>% mutate(max.proppos = max(proppos)) %>% filter(visit == 'V0') %>% ungroup() %>% mutate(label = forcats::fct_recode(treatment, A = 'rtPA', P = 'Placebo'))
                , aes(x = treatment, y = max.proppos, label = label), vjust = -0.5, size = 2, inherit.aes = FALSE) + 
    geom_segment(aes(color = lobe), x = .5, xend = .5, y = 0, yend = 1, arrow = arrow(length = unit(0, "cm"), type = 'closed', angle = 90, ends = 'both')) +
    annotate("text", x = offset, y = 1, label = "1", hjust = -1, vjust = 1, size = 2) +
    annotate("text", x = offset, y = 0, label = "0", hjust = -1, vjust = 0, size = 2) +
    guides(fill = FALSE, color = FALSE) + xlab('') + 
    scale_y_continuous('', breaks = NULL, limits = c(0,1.3), expand = c(0,0)) + 
    scale_x_discrete(name = '', breaks = NULL) + 
    scale_fill_grey(start = .5, end = .9) + 
    scale_color_discrete(drop = FALSE) +
    facet_wrap(.~ROI, scales = 'free') +
    theme_minimal() + 
    theme(text = element_text(size = 6, family = 'Helvetica')
          , strip.background = element_blank()
          , strip.text.x = element_blank()
          , panel.grid = element_blank()
          )
  return(p)
}

## This function allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity
        , geom = ggplot2:::GeomCustomAnn,
        inherit.aes = FALSE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax
                                          ))
}


insets <- d.plot %>% droplevels() %>% 
  split(f = .$ROI) %>%
  purrr::map(~annotation_custom2(
    grob = ggplotGrob(get_inset(.)), 
    data = data.frame(.)
    , xmin = log(1e-2), ymin = log(3e-4), xmax = log(2e0), ymax = log(3e-2))
  )

p <- main.plot + insets
p
#ggsave(paste0('./../../derivatives/figures/scatterplot/scatterplot_nodes_', asz,'.tiff'), plot = p, device = 'tiff'
#       , width = 18, height = 29.7, units = 'cm', dpi = 2400)
ggsave(paste0('./../../derivatives/figures/scatterplot/scatterplot_nodes_', asz,'.eps'), plot = p, device = 'eps'
       , width = 18, height = 24, units = 'cm')
ggsave(paste0('./../../derivatives/figures/scatterplot/scatterplot_nodes_', asz,'.svg'), plot = p, device = 'svg'
       , width = 18, height = 24, units = 'cm')
