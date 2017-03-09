# ln_Bern_h = function(samp, gamma, sigma){
#   FUN=function(x, samp, gamma, sigma){dbinom(samp, 1, plogis(gamma+x))*dnorm(x,0,sigma)}
#   out=integrate(
#     f = FUN,
#     lower=-Inf, upper=Inf, samp=samp, gamma=gamma, sigma=sigma
#   )$value
#   return(log(out))
# }






#' @title Locate raw example data
#' @description Show the location of the installed raw data .csv files for the 2016 shearing season
#' @export

show_data_loc = function(){
  cat("Raw example data located in the following directory:\n")
  cat(paste0(system.file(package="pupR"), "/raw_data/"))
}


#' @title Load raw example data
#' @description Load the 2016 shearing data into the global workspace. 
#' @export
load_demo_data = function(){
  dir=paste0(system.file(package="pupR"), "/raw_data/")
  shear_2016_snp = read.csv(paste0(dir,"shear_2016_snp.csv"))
  resight_2016_snp = read.csv(paste0(dir,"resight_2016_snp.csv"))
  shear_2016_sng = read.csv(paste0(dir,"shear_2016_sng.csv"))
  resight_2016_sng = read.csv(paste0(dir,"resight_2016_sng.csv"))
  assign("shear_2016_snp", shear_2016_snp, envir = .GlobalEnv)
  assign("resight_2016_snp", resight_2016_snp, envir = .GlobalEnv)
  assign("shear_2016_sng", shear_2016_sng, envir = .GlobalEnv)
  assign("resight_2016_sng", resight_2016_sng, envir = .GlobalEnv)
}





#' @title View demonstration
#' @description This function allows users to access a full step-by-step guide for using 'pupR' and other packages in the
#'  R statistical environment for estimating northern fur seal pup production. 
#' @export

see_demo = function(){
  dir=system.file(package="pupR")
  system(paste0("open ", dir, "/README.html"))
}

