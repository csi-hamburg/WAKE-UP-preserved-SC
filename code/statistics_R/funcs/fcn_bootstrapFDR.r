## FDR correction using bootstrapped samples to account for dependencies between test
## based on [Romano 2008, DOI: https://doi.org/10.1007/s11749-008-0126-6]

bsFDR.fun <- function(T.sample, index, T, k = Inf, alpha = 0.05){
  
  T.sample <- T.sample[index, ]
  
  
  s <- dim(T.sample)[2] # number of hypotheses to test
  if (k>s) k <- Inf
  
  ix <- sort(T, decreasing = FALSE, index.return = TRUE)$ix
  
  if (k < Inf){
    ## Test only top k hypotheses
    T.sample <- T.sample[, ix[(s-k):s], drop=FALSE]
    ix <- 1:s
    s <- k    
  }
  
  
  star <- function(r,t){
    (apply(T.sample[, ix[1:t], drop = FALSE], 1, sort) %>% t() %>% matrix(nrow = n, ncol = t, byrow = FALSE))[, r]
  }
  mstar <- memoise(star)
  
  ff <- function(x,j){
    res <- 0
    for (r in seq(s-j+1, s)){
      reps <- mstar(j, j) >= x
      if(j>1){
        for (subidx in seq(j-1, min(j-1, s-r+1), by = -1)){
          reps <- reps & mstar(subidx, j) >= thrc[subidx]
          if (all(!reps)) next()
        }
      }
      if(s-r > 0){
        reps = reps & mstar(s-r, j) < thrc[s-r]
      }
      res <- res + (r-s+j)/r * mean(reps)
    }
    res
  }
  
  thrc <- c()
  thrc[1] <- -Inf
  for (j in 1:s){
    print(j)
    if ((ff(-2,j) - alpha) * (ff(2,j) - alpha) > 0){
      thrc[j] <- -Inf
      next()
    } else{
      thrc[j] <- uniroot(function(x){ff(x,j) - alpha}, c(-2,2))$root
    }
    print(thrc)
  }
  return(matrix(c(thrc), ncol=1)
  ) 
}