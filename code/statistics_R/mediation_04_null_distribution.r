
NIE.joint.00 <- c()
for(i in 1:1e4){
  med.list <- levels(dd$ROI)[rbernoulli(nlevels(dd$ROI), 0.5)]
  med.list <- c(med.list, 'delta.vol')
  tryCatch({
    m.out <- glm(as.formula(paste0('y ~ x +', paste(med.list, collapse = '+'), ' + ', paste(covs, collapse = ' + '))), data = df, family = binomial())
    mdl.num <- glm(x ~ 1, data = df, family = binomial(link = 'logit'))
    mdl.denom <- glm(as.formula(paste0('x ~ 1 + ', paste(covs, collapse = ' + '))), data = df, family = binomial())
    
    
    ###
    Call.num <- getCall(mdl.num)
    Call.denom <- getCall(mdl.denom)
    Call.out <- getCall(m.out)
    environment(IPW.fun) <- environment()
    
    
    D.0 <- IPW.fun(df = df, index = 1:nrow(df))
    
    NIE.joint.00[i] <- D.0["NIE"]
    propOR.joint.0[i] <- D.0["propOR"]
    
  }
  , error = function(c){
    print(med.list)
    print(paste0('[Error caught] ', c))
  }
  )
}

labs <- df.IPW %>% arrange(est) %>% filter(p <= 1 & m != ' + delta.vol') %>% pull('m')


f<-function(m){
  str_remove(m, ' \\+ delta.vol') %>% 
    str_split(' \\+ ') %>% unlist() %>% 
    ROI.short.dict[.] %>% 
    paste(., collapse = ' + ')
  }

