ln_Bern_h = function(samp, gamma, sigma){
  FUN=function(x, samp, gamma, sigma){dbinom(samp, 1, plogis(gamma+x))*dnorm(x,0,sigma)}
  out=integrate(
    f = FUN,
    lower=-Inf, upper=Inf, samp=samp, gamma=gamma, sigma=sigma
  )$value
  return(log(out))
}

dbinom2 = function(x, size, prob, log=FALSE){
  out = lchoose(size, x) + x*log(prob) + (size-x)*log(1-prob)
  if(log) return(out)
  else return(exp(out))
}

ln_lik = function(par, M, u, m){
  U = exp(par[1])
  tau = plogis(par[2])
  delta = plogis(par[3:length(par)])
  return(
    -2*(
      sum(dbinom(m, M, delta, T)) +
        sum(dbinom2(u, U, delta, T))
    ) -2*dbinom2(M, U+M, tau, T)
  )
}
