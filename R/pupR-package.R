#' @title Model-based estimation of northern fur seal pup production
#' 
#' @description Mark-resight models for estimation of northern fur seal pup abundance.
#' 
#' \tabular{ll}{ 
#' Package: \tab pupR\cr 
#' Type: \tab Package\cr 
#' Version: \tab 0.0.1\cr 
#' Date: \tab February 27, 2017\cr 
#' License: \tab CC0 \cr 
#' LazyLoad: \tab yes\cr 
#' }
#' 
#' @note This software package is developed and maintained by scientists at the NOAA Fisheries Alaska 
#' Fisheries Science Center and should be considered a fundamental research communication. 
#' The reccomendations and conclusions presented here are those of 
#' the authors and this software should not be construed as official communication by NMFS, NOAA, 
#' or the U.S. Dept. of Commerce. In addition, reference to trade names does not imply endorsement by the 
#' National Marine Fisheries Service, NOAA. While the best efforts have been made to insure the 
#' highest quality, tools such as this are under constant development and are subject to change.
#' 
#' @name pupR-package
#' @aliases pupR-package pupR
#' @docType package
#' @author Devin S. Johnson
#' 
#' Maintainer: Devin S. Johnson <devin.johnson@@noaa.gov>
#' @importFrom stats dbinom plogis qlogis optim sd
#' @importFrom utils read.csv
#' 
NULL


.onAttach <- function(library, pkgname)
{
  info <-utils::packageDescription(pkgname)
  package <- info$Package
  version <- info$Version
  date <- info$Date
  packageStartupMessage(
    paste(package, version, paste("(",date, ")", sep=""), "\n")
  )
}



