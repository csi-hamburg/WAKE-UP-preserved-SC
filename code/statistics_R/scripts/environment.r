require(knitr)
knitr::opts_chunk$set(cache = FALSE, warning = FALSE, message = FALSE, cache.lazy = FALSE)

inline_hook <- function(x) {
  if(is.numeric(x)) x <- round(x, 4)
  paste(as.character(x), collapse=", ")
}
knit_hooks$set(inline = inline_hook)

options(rlang_trace_top_env = rlang::current_env())
options(error = function() {
  sink()
  print(rlang::trace_back(bottom = sys.frame(-1)), simplify = "none")
})

library(chunkhooks)
hook_figure_unit()
hook_figure_unit("cm")

require(tidyverse)
require(magrittr)

require(memoise)

require(qqplotr)

require(ggplot2)
require(ggsci)
require(ggthemr)
require(ggthemes)
require(ggrepel)


require(glmmTMB)
require(broom.mixed)
require(effects)
require(sjPlot)
require(gamlss)

require(flextable)
require(pammtools)
require(tidybayes)
require(ComplexUpset)
require(patchwork)


ggthemr('earth')
set_swatch(c('black', '#0077bb','#ee7733','#009988','#33bbee','#ee3377','#cc3311'))



## Auxilliary functions

pvalformatter <- Vectorize(
  function(x){
    if(!is.finite(x)){
      return('')
    }
    if(abs(x) < 2e-100){
      return('0')
    }
    if(abs(x)>0.001){
      sprintf('%1.3f', x) %>% return()
    }else if(abs(x) < 0.001){
      sprintf('%1.2e', x) %>% return()
    }
  })


tab.anova.fcn <- function(m){
  m %>% 
    car::Anova(.) %>% as_tibble(rownames = 'term') %>% 
    #dplyr::select(-Df, -Chisq) %>% 
    rename(p = `Pr(>Chisq)`) %>% 
    flextable() %>% 
    set_formatter(values = list(p = pvalformatter
                              , Df = function(y)sprintf('%d',y))) %>% 
    set_header_labels(values = list(term = ''
                                    , Df = 'd.o.f.'
                                    , p = 'P value'
                                    , Chisq = 'χ²'))
}


source('./funcs/fcn_upset_data.r')
source('./funcs/fcn_InvPrWeighting.r')
source('./funcs/fcn_bootstrapFDR.r')
source('./scripts/geom-step2ribbon.r')


## bias-corrected confidence interval
BC.CI <- function(theta, conf.level = 0.95) {
  low <- (1 - conf.level)/2
  high <- 1 - low
  sims <- length(theta)
  z.inv <- length(theta[theta < mean(theta)])/sims
  z <- qnorm(z.inv)
  U <- (sims - 1) * (mean(theta) - theta)
  top <- sum(U^3)
  under <- 6 * (sum(U^2))^{
    3/2
  }
  a <- top/under
  lower.inv <- pnorm(z + (z + qnorm(low))/(1 - 
                                             a * (z + qnorm(low))))
  lower2 <- lower <- quantile(theta, lower.inv)
  upper.inv <- pnorm(z + (z + qnorm(high))/(1 - 
                                              a * (z + qnorm(high))))
  upper2 <- upper <- quantile(theta, upper.inv)
  return(c(lower, upper))
}


## dictionaries for plotting

ROI.short.dict.86 <- c(Supramarginal = 'SM'
                       , Posteriorcingulate = 'PCing'
                       , Caudate = 'Cau'
                       , Postcentral = 'PoC'
                       , Isthmuscingulate = 'ICing'
                       , Paracentral = 'PaC'
                       , Precentral = 'PreC'
                       , Superiorfrontal = 'SupF'
                       , Caudalanteriorcingulate = 'CACing'
                       , Parahippocampal = 'PH'
                       , Caudalmiddlefrontal = 'CMF'
                       , Insula = 'Ins'
                       , Thalamus_Proper = 'Tha'
                       , Lateralorbitofrontal = 'LOF'
                       , Parsorbitalis = 'POrb'
                       , Fusiform = 'Fus'
                       , Transversetemporal = 'TT'
                       , Superiorparietal = 'SupP'
                       , Medialorbitofrontal = 'MOF'
                       , Rostralanteriorcingulate = 'RACing'
                       , Precuneus = 'Prec'
                       , Superiortemporal = 'SupT'
                       , Bankssts = 'BSTS'
                       , Accumbens_area = 'Acc')

ROI.short.dict.116 <- c(Cingulum_Post = 'PCing'
                        , Parietal_Inf = 'InfP'
                        , Parietal_Sup = 'SupP'
                        , Cingulum_Mid = 'MCing'
                        , Isthmuscingulate = 'ICing'
                        , Rectus = 'Rec'
                        , Precuneus = 'Prec'
                        , Frontal_Sup_Orb = 'SOF'
                        , Cingulum_Ant = 'ACing'
                        , Putamen = 'Put'
                        , Frontal_Mid_Orb910 = 'MOF'
                        , Paracentral_Lobule = 'PaC'
                        , Frontal_Inf_Orb = 'IOF'
                        , Heschl = 'Hes'
                        , Caudate = 'Cau'
                        , SupraMarginal = 'SM'
                        , Postcentral = 'PoC'
                        , Temporal_Mid = 'MidT'
                        , Frontal_Mid_Orb2526 = 'MOF'
                        , Temporal_Sup = 'SupT'
                        , Pallidum = 'Pal'
                        , Insula = 'Ins'
                        , Precentral = 'PreC'
                        , Fusiform = 'Fus'
                        , Rolandic_Oper = 'RolO'
                        , ParaHippocampal = 'PH'
                        , Supp_Motor_Area = 'SuppM'
                        , Occipital_Sup = 'SupO'
                        , Hippocampus = 'Hip'
                        , Occipital_Inf = 'InfO'
                        , Cuneus = 'Cun'
                        , Frontal_Sup_Medial = 'MSF'
                        , Olfactory = 'Olf'
                        , Frontal_Inf_Tri = 'PTri'
                        , Angular = 'Ang'
                        , Thalamus = 'Tha'
                        , Frontal_Sup = 'SupF'
                        , Occipital_Mid = 'MidO'
                        , Temporal_Inf = 'InfT'
                        , Frontal_Inf_Oper = 'POper'
                        , Frontal_Mid = 'MidF'
                        , Amygdala = 'Amy'
                        , Lingual = 'Ling'
                        , Calcarine = 'Calc'
                        , Temporal_Pole_Mid = 'MidTP'
                        , Temporal_Pole_Sup = 'SupTP')

ROI.short.dict <- c(ROI.short.dict.86, ROI.short.dict.116)