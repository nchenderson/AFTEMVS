PostInclProbs <- function(beta.coef, theta.vec, sig.sq, v0, v1) {
    aa <- dnorm(beta.coef, sd=sqrt(sig.sq*v1))*theta.vec
    bb <- dnorm(beta.coef, sd=sqrt(sig.sq*v0))*(1 - theta.vec)
    
    ans <- aa/(aa + bb)
    return(ans)
}