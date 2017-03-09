#' @title Binomial PDF 
#' @description A binomial pdf that handles a noninteger 'size' argument.
#' @param x vector of quantiles
#' @param size number of trials (zero or more). Unlike base 'dbinom' doesn't have to be an integer
#' @param prob probability of success on each trial
#' @param log logical; if TRUE, probabilities p are given as log(p)
#' @author Devin S. Johnson
#' @export 
dbinom2 = function(x, size, prob, log=FALSE){
  out = lchoose(size, x) + x*log(prob) + (size-x)*log(1-prob)
  if(log) return(out)
  else return(exp(out))
}


#' @title log likelihood funtion for observer by occasion model
#' @description calculate log-likelihood for the observer by occassion model
#' @param par parameter vector
#' @param M Number of marks distributed
#' @param u vector on unmarked animals counted by each observer on each occasion
#' @param m vector on marked animals counted by each observer on each occasion
#' @author Devin S. Johnson
#' @export
ln_lik_oo = function(par, M, u, m){
  U = exp(par[1])
  tau = plogis(par[2])
  delta = plogis(par[3:length(par)])
  return(-sum(dbinom(m, M, delta, T)) - sum(dbinom2(u, U, delta, T)) - dbinom2(M, U+M, tau, T))
}

#' @title Fit observer by occasion model to NFS resight data
#' @description Fit the observer by occasion model using maximum likelihood. The parameters, covariance matrix, 
#' and log-likelihood are returned in a list.
#' @param data A data frame containing the resight information. The data must contain the columns 
#' (1) 'obs'-- the categorical name of the observer.
#' (2) 'resample'-- a variable indicating the resample occasion
#' (3) 'm'-- the counts of marked pups by each observer
#' (4) 'u'-- the counts of unmarked pups
#' @param M the total number of unmarked pups in the population
#' @param start_val an optional vector of starting values for the model
#' @param refit logical. should the function attempt a refit to help  ensure global optimum is reached
#' @param ... holding space for any other variable supplied, these will be ignored
#' @import mvtnorm
#' @export

fit_oo = function(data, M, start_val=NULL, refit=TRUE, ...){
  if(is.null(start_val)){
    start_val = c(log(M/0.1), qlogis(0.1), qlogis(data$m/M))
  }
  fit = optim(
    par=start_val, 
    ln_lik_oo, 
    M=M, u=data$u, m=data$m, 
    hessian = T
  )
  vcv = solve(fit$hessian)
  par = fit$par
  if(refit){
    prop = mvtnorm::rmvnorm(1000, par, vcv)
    logLik=apply(prop, MARGIN=1, FUN=function(par, M, u, m){-ln_lik_oo(par, M, u, m)}, M=M, m=data$m, u=data$u)
    st = prop[logLik==max(logLik),]
    fit = optim(
      par=st, 
      ln_lik_oo, 
      M=M, u=data$u, m=data$m, 
      hessian = T
    )
    vcv = solve(fit$hessian)
    par = fit$par
  }
  nms = c("theta","alpha",paste(data$obs, data$resample, sep="_"))
  colnames(vcv) = nms
  rownames(vcv) = nms
  names(par) = nms
  return(list(par=par, vcv=vcv, logLik=-fit$value))
}

#' @title Draw posterior sample using Sample-Importance-Resample algorithm
#' @description Uses SIR methodology to draw a sample from the posterior distribution. A MVN proposal is used based on the MLE list output of the model fitting function
#' @param fit A list produced by `fit_oo' function
#' @param data The resampling data
#' @param M the total number of pups shear marked
#' @param res the size of the resample
#' @param initial size of the initial sample from the MVN proposal
#' @param ... holding space for any other variable supplied, these will be ignored
#' @export

get_IS_sample = function(fit, data, M, res=5000, initial=10000, ...){
  prop = mvtnorm::rmvnorm(initial, fit$par, fit$vcv)
  w = apply(prop, MARGIN=1, FUN=function(par, M, u, m){-ln_lik_oo(par, M, u, m)}, M=M, m=data$m, u=data$u) -
    mvtnorm::dmvnorm(prop, fit$par, fit$vcv, log=T)
  w=exp(w)/sum(exp(w))
  out = sample.int(initial, res, replace=T, prob=w)
  post = prop[out,]
  # N = coda::mcmc(round(exp(post[,1])+M))
  # tau = coda::mcmc(plogis(post[,2]))
  # delta=coda::mcmc(plogis(post[,-c(1:2)]))
  # N_lst = list(N = round(mean(N)), SE=sd(N), CI=coda::HPDinterval(N))
  # tau_lst = list(tau = mean(tau), SE=sd(tau), CI=coda::HPDinterval(tau))
  # delta_lst = list(delta=mean(delta), SE=sd(delta), CI=coda::HPDinterval(delta))
  # retun(data.frame(N=N_lst$N, SE=N_lst$SE, N_lst=N_lst, tau_lst=tau_lst, delta_lst=delta_lst))
}


#' @title Summarize the posterior sample
#' @description Here we summarize the posterior sample into a table for each rookery
#' @param SIR a matrix of posterior samples from the `get_IS_sample' function
#' @param M total number of sheared pups
#' @param ... holding space for any other variable supplied, these will be ignored
#' @import coda dplyr
#' @export

summarize_oo = function(SIR, M, ...){
  N = coda::mcmc(round(M + exp(SIR[,1])))
  tau = coda::mcmc(plogis(SIR[,2]))
  delta = coda::mcmc(plogis(SIR[,-c(1:2)]))
  out=data.frame(
    parameter=c("N","tau",paste0("delta_", colnames(delta))),
    estimate=c(round(mean(N)), mean(tau), colMeans(delta)), 
    se = c(round(sd(N)), sd(tau), apply(delta, 2, sd)),
    CI=dplyr::bind_rows(
      data.frame(round(coda::HPDinterval(N))), 
      data.frame(coda::HPDinterval(tau)), 
      data.frame(coda::HPDinterval(delta))
      )
  ) 
  return(out)
}

