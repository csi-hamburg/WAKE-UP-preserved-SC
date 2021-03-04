NIE.sample <- NIE.joint[, colSums(is.na(NIE.joint))==0]

n <- dim(NIE.sample)[1]
s <- dim(NIE.sample)[2]

NIE.hat <- NIE.joint.0[!is.na(NIE.joint.0)]

reps <- 100
df.thr <- NULL
df.cor <- NULL
for (comparator in c('1', 'vol')){
  print(comparator)
  for (i in 0){
    
    if (i==0){
      idx.hypo <- 2:dim(NIE.sample)[2] # all models except volume alone
      if (asz == 116){
       idx.hypo <- idx.hypo[1:(length(idx.hypo)-1)] 
      }
    } 
      
    else if (i %in% 1:3)
      idx.hypo <- which(str_detect(allmdls.unique, SC[[i]])) # specific subcortical ROI
    else if (i==5){
      idx.hypo <- apply(sapply(SC, function(x)str_detect(allmdls.unique, x, negate = TRUE)), 1, all) %>% which() # no subcortical ROIs
      idx.hypo <- setdiff(idx.hypo,1)
    } else
      next()
    
    print(i)
    print(idx.hypo)
    
    
    if (comparator == '1'){
      T <- NIE.hat[idx.hypo] - 1 ## one-sided
      T.sample <- t(NIE.sample[, idx.hypo] %>% t() - NIE.hat[idx.hypo])  
    } else if (comparator == 'vol'){
      T <- NIE.hat[idx.hypo] - NIE.hat[1] - 0 ## one-sided
      T.sample <- t((NIE.sample[, idx.hypo] - NIE.sample[, 1]) %>% t() - (NIE.hat[idx.hypo] - NIE.hat[1]))  
    } else
      next()
    
    
    T.sort <- sort(T, decreasing = TRUE, index.return = TRUE)
    s <- length(idx.hypo)
    
    max(T)
    
    D <- boot::boot(data = T.sample, statistic = bsFDR.fun, R = reps, sim = "ordinary", T = T, k = s, alpha = 0.05
                    , parallel = "multicore", ncpus = 7)
    
    thrc <- D$t[, , drop = FALSE]
    df.thr <- bind_rows(df.thr
                        , thrc %>% t() %>%  as_tibble(.name_repair = "minimal") %>% setNames(paste0('rep', 1:reps)) %>% 
                          rowid_to_column('orderid') %>% 
                          mutate(id = idx.hypo[T.sort$ix]
                                 , hyposet = i
                                 , comparator = comparator
                                 , T = T.sort$x) %>% 
                          pivot_longer(starts_with('rep'), names_to = 'rep', values_to = 'thr') %>% 
                          mutate(thr = ifelse(thr == -2, -Inf, thr)) %>%
                          mutate(thr = ifelse(thr == -Inf, NA, thr)) %>% 
                          fill(thr, .direction = 'down') %>% 
                          group_by(id) %>% 
                          mutate(meanthr = mean(thr)) %>% 
                          ungroup() %>% 
                          group_by(rep) %>% 
                          mutate(thr = rev(thr), meanthr = rev(meanthr)) %>% 
                          ungroup())
    
    
    
    cormat <- cor(T.sample[,T.sort$ix], T.sample[,T.sort$ix])
    df.cor <- bind_rows(df.cor
                        , expand_grid(x = 1:s, y = 1:s) %>% 
                          mutate(c = cormat %>% as.vector
                                 , hyposet = i
                                 , comparator = comparator
                          ))
  }
}

