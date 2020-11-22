#' \code{calcMSI} - Calculate multispecies indicator.
#' 
#' @description This function takes the output of \code{applyFilters} and produces a multispecies indicator
#'              using the chosen method.
#'
#' @param dat String. Object returned by \code{applyFilters}.
#' 
#' @param method String. Which indicator method to use. One of "lambda" or "BMA". 
#' 
#' @param write Logical. Whether or not to write the outputs to  file.
#'                  
#' @param outPath String. Where to store the outputs if write = TRUE. 
#'
#' @param plotLabel String. Title for the indicator plot.
#' 
#' @param bmaInd String. Use "prime" if all species data are available for all species in all years, and 
#'               "reg" otherwise. See Freeman et al. 2020 for details. 
#' 
#' @param ... String. Title for the indicator plot.Additional arguments to be passed to \code{BRCindicators::bma}
#' 	  
#' @return An list with elements indicator, short term assessment, long term assessment and plot.
#'         
#' @export
#' 

calcMSI <- function(dat, 
                    method, 
                    write, 
                    outPath,
                    plotLabel,
                    bmaInd = NULL,
                    ...) {
  
  if (!method %in% c("lambda","bma")) stop("Method must be one of lambda or bma")
  
  colnames(dat)[1:(ncol(dat) -2)] <- gsub("year_", "", colnames(dat[1:(ncol(dat) -2)]))
  
  minYr <- as.numeric(colnames(dat)[1])
  
  maxYr <- as.numeric(colnames(dat)[ncol(dat) - 2])
  
  if (method == "lambda") {
    
    arr <- sampArray(dat = dat,
                     startYear = minYr,
                     endYear = maxYr,
                     niter = 1000)
    
    ind <- BRCindicators::lambda_indicator( 
      input=arr, 
      index = 100, 
      threshold_sd = Inf, 
      threshold_Rhat = Inf, 
      threshold_yrs = 10, 
      upperQuantile = 0.975, 
      lowerQuantile = 0.025 
    )
    
    summary <- ind$summary
    
    lt <- BRCindicators::trend_assessment(ind, 
                                          start_year = 1,  
                                          end_year = max(ind$summary$year)) 
    
    st <- BRCindicators::trend_assessment(ind, 
                                          start_year = (max(ind$summary$year) - 6),  
                                          end_year = max(ind$summary$year)) 
    
    final <- BRCindicators::trend_assessment(ind, 
                                          start_year = (max(ind$summary$year) - 1),  
                                          end_year = max(ind$summary$year)) 
    
  } else {
    
    means <- aggregate(.~species, data=dat, mean)
    
    sds <- aggregate(.~species, data=dat, sd)
    
    getSumStats <- function(stat) {
      
      out <- stat[, -ncol(stat)]
      
      out <- melt(out, 
                  id.vars = "species",
                  variable.name = "year",
                  value.name = "index")
      
      return(out) 
      
    }
    
    means <- getSumStats(means)
    
    se <- getSumStats(sds)[,3]
    
    inDat <- data.frame(means, 
                        se = se)
    
    inDat$year <- as.numeric(inDat$year)
    
    ind <- bma(data = inDat,
               seFromData = TRUE,
               ...)
    
    if (bmaInd != "prime") {
      
      summary <- data.frame(indicator = ind$Index.M,
                            lower = ind$lowerCI.M,
                            upper = ind$upperCI.M,
                            Species_Number = length(unique(inDat$species)))
      
    } else {
      
      summary <- data.frame(indicator = ind$Index.Mprime,
                            lower = ind$lowerCI.Mprime,
                            upper = ind$upperCI.Mprime,
                            Species_Number = length(unique(inDat$species)))
      
    }
    
    lt <- NULL
    
    st <- NULL
    
    final <- NULL
    
  }

  out <- list(summary, ind, st, lt, final)
  
  names(out) <- c("Summary", "MetaData")

  return(out)
  
}