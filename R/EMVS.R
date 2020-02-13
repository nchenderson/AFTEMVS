    
AFTEMVS <- function(times, event, X, v0, v1, beta_init, hBF,
                 sigma_init = 1, epsilon = 10^(-5),
                 temperature, theta = 0.5, a = 1,
                 b = 1, v1_g) {
  
  Y <- log(times)
  YY.expec <- Y
  res <- 2
  
  Amat <- crossprod(X, X)
  dXtX <- diag(Amat)
  
  ## Start for loop here
  ##   Compute post.probs pp
  for(k in 1:maxiter) {
     pp <- PostInclProbs(beta.coef, theta.vec, sig.sq, v0, v1) 
     fitted.vals <- drop(X%*%beta.coef)
     resids <- (Y - fitted.vals)/sqrt(sig.sq)
     
     diag(Amat) <- dXtX + (1 - pp)/v0 + pp/v1
     ## update conditional expectation of Y here
     YY.expec[event==0] <- fitted.vals[event==0] + InvMillsRatio(resids[event==0])
     beta.new <- solve(Amat, crossprod(X, YY.expec))
     theta.new <- (pp + hBF - 1)/(1/hBF + hBF - 1)
     ## need to update sigma squared as well
     # sig.sq <- 
  }
  return(res)
}


