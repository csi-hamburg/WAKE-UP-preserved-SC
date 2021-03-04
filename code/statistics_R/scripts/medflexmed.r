## Double check mediation analysis using imputation-based approach from medflex package.

require(medflex)

## By convention, the index ‘0’ is used for parameters (and corresponding auxiliary
## variables) indexing natural direct effects, whereas the index ‘1’ is used for parameters indexing
## natural indirect effects in the natural effect model.

for (mdl in allmdls.unique){
  med.list <- mdl %>% stringr::str_split(' \\+ ') %>% unlist()
  
  med.list <- med.list %>% unique()
  med.list <- med.list[lapply(med.list,nchar)>0]
  med.list %>% print()
  
  
  
  expData <- neImpute(as.formula(paste0('y ~ x +', paste(med.list, collapse = '+'), ' + ', paste(covs, collapse = ' + ')))
                      , family = binomial("logit")
                      , data = df
                      , nMed = length(med.list)
  )
  
  
  neMod1 <- neModel(as.formula(paste0('y ~ x0 + x1 +', paste(covs, collapse = ' + ')))
                    , family = binomial("logit")
                    , expData = expData
                    , se = "robust"
                    , )
  s <- summary(neMod1) 
  print(s)
  NIE <- s$coefficients['x1t','Estimate'] %>% exp()
  NDE <- s$coefficients['x0t','Estimate'] %>% exp()
  propOR <- log(NIE) / (log(NDE) + log(NIE))
  print(propOR)
}
