## Multiple mediation analysis of thrombolysis on functional outcome via infarct growth and local connectivity loss
## Uses Inverse Probability Weighting (VanderWeele 2015, DOI:10.1515/em-2012-0010)



#sims <- 1e5
NIE.joint <- propOR.joint <- matrix(nrow = sims, ncol = length(allmdls.unique))
NIE.joint.0 <- propOR.joint.0 <- matrix(nrow = 1, ncol = length(allmdls.unique))

i<-0
for (mdl in allmdls.unique){
  
    med.list <- mdl %>% stringr::str_split(' \\+ ') %>% unlist()
    med.list <- med.list %>% unique()
    med.list %>% print()
    i <- i + 1
    
    
    m.out <- glm(as.formula(paste0('y ~ x +', paste(med.list, collapse = '+'), ' + ', paste(covs, collapse = ' + ')))
                 , data = df, family = binomial())
    mdl.num <- glm(x ~ 1, data = df, family = binomial(link = 'logit'))
    mdl.denom <- glm(as.formula(paste0('x ~ 1 + ', paste(covs, collapse = ' + ')))
                     , data = df, family = binomial())
    
    
    ###
    Call.num <- getCall(mdl.num)
    Call.denom <- getCall(mdl.denom)
    Call.out <- getCall(m.out)
    environment(IPW.fun) <- environment()

    D.0 <- IPW.fun(df = df, index = 1:nrow(df))
    
    NIE.joint.0[i] <- D.0["NIE"]
    propOR.joint.0[i] <- D.0["propOR"]
    print(NIE.joint.0[i])
    
    set.seed(4321)
    NDE <- NA
    tryCatch(
      {
        D <- boot::boot(data = df, statistic = IPW.fun, 
                        R = sims, sim = "ordinary"
                        , parallel = "multicore", ncpus = 7)
        
        NDE <- D$t[, 1, drop = FALSE]
        NIE <- D$t[, 2, drop = FALSE]
        TE <- D$t[, 3, drop = FALSE]
        propOR <- D$t[, 4, drop = FALSE]
        propLin <- D$t[, 5, drop = FALSE]
      }
      , error = function(c){
        print(paste0('[Error caught] ', c))
      }
      , warning = function(x)message(x)
      , finally = {}
    )
    

    if(!is.na(NDE)){
      print(i)
      NIE.joint[, i] <- NIE
      propOR.joint[, i] <- propOR
      
      }
}

NIE.joint %>% dim()
# [number of bootstrap samples] x [number of unique models considered]

bs.success <- colSums(is.na(NIE.joint))==0
NIE.joint.bss <- NIE.joint[, bs.success]
NIE.joint.bss %>% dim()

NIE.joint.bss <- NIE.joint[, bs.success]
propOR.joint.bss <- propOR.joint[, bs.success]

NIE.joint.0.bss <- NIE.joint.0[bs.success]
propOR.joint.0.bss <- propOR.joint.0[bs.success]

allmdls.unique.bss <- allmdls.unique[bs.success, drop=FALSE]

p <- apply(NIE.joint.bss, 2, function(x)mean(x < 1))
CI.BC <- apply(NIE.joint.bss, 2, BC.CI)
CI.q <- apply(NIE.joint.bss, 2, function(x)quantile(x, probs = c(0.025, 0.975)))

CI.q.propOR <- apply(propOR.joint.bss, 2, function(x)quantile(x[!x %in% boxplot.stats(x)$out], probs = c(0.025, 0.975)))
CI.BC.propOR <- apply(propOR.joint.bss, 2, function(x)BC.CI(x[!x %in% boxplot.stats(x)$out]))

est.vol <- NIE.joint.0.bss / NIE.joint.0.bss[1]
p.vol <- apply(t((NIE.joint.bss / NIE.joint.bss[, 1]) %>% t()), 2, function(x)mean(x < 1))
CI.BC.vol <- apply(t((NIE.joint.bss / NIE.joint.bss[, 1]) %>% t()), 2, BC.CI)
CI.q.vol <- apply(t((NIE.joint.bss / NIE.joint.bss[, 1]) %>% t()), 2, function(x)quantile(x, probs = c(0.025, 0.975)))

df.IPW <- tibble(m = allmdls.unique.bss
                 , est = NIE.joint.0.bss, p = p, propOR = propOR.joint.0.bss
                 , lwr.BC = CI.BC[1, ], upr.BC = CI.BC[2, ]
                 , lwr.q = CI.q[1, ], upr.q = CI.q[2, ]
                 , lwr.BC.propOR = CI.BC.propOR[1, ], upr.BC.propOR = CI.BC.propOR[2, ]
                 , lwr.q.propOR = CI.q.propOR[1, ], upr.q.propOR = CI.q.propOR[2, ]
                 , est.vol = est.vol
                 , p.vol = p.vol
                 , lwr.q.vol = CI.q.vol[1, ], upr.q.vol = CI.q.vol[2, ]
                 , lwr.BC.vol = CI.BC.vol[1, ], upr.BC.vol = CI.BC.vol[2, ]
)

