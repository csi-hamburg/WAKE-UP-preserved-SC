createFSoverlay <- function(df, asz, ROI = 'ROI', p = 'p.value', label = 'test', divergent = FALSE, dom.min = NULL, dom.max = NULL){
  aparc.ctab<-read.table(paste0('../../code/figure_creation/surface_plots/aparc.annot', asz, '.ctab'), header = F)
  colnames(aparc.ctab) <- c('structure.number', 'ROI', 'R', 'G', 'B', 'flag')
  
  temp <- df %>%
    mutate(ROI = case_when(asz==86 ~ tolower(!!as.name(ROI))
                           , asz==116 ~ paste0(as.character(ROI), '_L'))) %>% 
    merge(aparc.ctab)  
  
  if (is.null(dom.min)) dom.min <- min(temp[[p]][is.finite(temp[[p]])], na.rm = T)
  if (is.null(dom.max)) dom.max <- max(temp[[p]][is.finite(temp[[p]])], na.rm = T)
  
  
  dom.min.orig <- dom.min
  dom.max.orig <- dom.max
  
  

  
  ## effect size centered around 0.5
  #if (divergent & dom.min < 0.5 & dom.max >= 0.5){
  if (divergent){
    dom <- max(0.5-dom.min, dom.max-0.5)
    dom.min <- 0.5-dom
    dom.max <- 0.5+dom
  }

  if (asz == 116 & label == 'e.abs.treatment'){
      print(dom.min)
    print(dom.min)
    print(dom)
  }
    
  pal <- leaflet::colorNumeric(
    #palette = viridis::cividis(n=1000,direction = -1) 
    palette = if (divergent){ # effect size
      if (stringr::str_detect(label, 'treatment')){
        scico::scico(n = 1e4, palette = 'vik', direction = 1)
      }else if (stringr::str_detect(label, 'visit')){
        scico::scico(n = 1e4, palette = 'cork', direction = 1)
      }else if (stringr::str_detect(label, 'ix')){
        scico::scico(n = 1e4, palette = 'broc', direction = 1)
      }
      
    }else{ # p values
      scico::scico(n = 1e4, palette = 'acton', direction = -1)
    }
    , domain = c(dom.min, dom.max)
  )
  
  dir.create(file.path('..', '..', 'derivatives', 'surface_maps', asz, label), showWarnings = FALSE, recursive = TRUE)
  tiff(filename = file.path('..', '..', 'derivatives', 'surface_maps', asz, label,   paste0('colorbar_', label, '.tiff'))
       , width = 4, height = 10, units = 'cm', res = 600)
  
  pretty.ticks <- pretty(c(dom.min, dom.max), n = 5)
  pretty.ticks <- pretty.ticks[pretty.ticks < dom.max & pretty.ticks > dom.min]
  
  if(!div.flag){
    pretty.ticks <- pretty(c(exp(dom.min), exp(dom.max)), n = 5)
    pretty.ticks <- pretty.ticks[pretty.ticks < exp(dom.max) & pretty.ticks > exp(dom.min)]
    at = formatC(log(pretty.ticks), format = 'fg') %>% as.numeric()
  }
  else{
    at = formatC(pretty.ticks, format = 'fg') %>% as.numeric()
  }
  labels = formatC(pretty.ticks, format = 'fg')
  
  
  color.bar(pal(seq(dom.min, dom.max, length.out = 100)), min = dom.min, max = dom.max
            , ticks = pretty.ticks, plot.min = dom.min.orig, plot.max = dom.max.orig
            , at = at, labels = labels, cex = if_else(stringr::str_detect(label,'ix'),1.5,3))
  dev.off()
  
  tibble.ctab <- temp %>%
    bind_cols(pal(.[[p]]) %>% col2rgb() %>% t()  %>% as_tibble) %>% 
    dplyr::select(c('structure.number','ROI','red','green','blue','flag'))
  
  file.path('..', '..', 'derivatives', 'surface_maps', asz, label,   paste0(label,'.ctab'))
  write.table(tibble.ctab
              , file.path('..', '..', 'derivatives', 'surface_maps', asz, label,   paste0(label,'.ctab'))
              , col.names = FALSE
              , row.names = FALSE
              , quote = FALSE)
  
  
}


color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='', plot.min=min, plot.max=max, at = NULL, labels = NULL, cex = 3) {
  scale = (length(lut)-1)/(max-min)
  par(mar=c(0,0,0,6), cex.axis = cex)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title, ylim=c(plot.min, plot.max))
  axis(4, at = c(min,max), las=1, labels = c('',''), lwd.ticks = 0)
  axis(4, at = at, labels = labels,  las=1)
  
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

