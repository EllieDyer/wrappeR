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
#' @return An list with elements indicator, short term assessment, long term assessment and plot.
#'         
#' @export
#' 

calcMSI <- function(dat, 
                    method, 
                    write, 
                    outPath,
                    plotLabel) {
  
  minYr <- as.numeric(gsub("year_", "", colnames(dat)[1]))
  
  maxYr <- as.numeric(gsub("year_", "", colnames(dat)[ncol(dat) - 2]))
  
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
    
  }
  
  lt <- BRCindicators::trend_assessment(ind, 
                                        start_year = 1,  
                                        end_year = max(ind$summary$year)) 
  
  st <- BRCindicators::trend_assessment(ind, 
                                        start_year = (max(ind$summary$year) - 6),  
                                        end_year = max(ind$summary$year)) 
  
  
  plot <- plotIndicator(ind = ind, 
                        minYear=minYr,  
                        maxYear = maxYr, 
                        label = plotLabel,
                        plotType = "indicator",
                        st = st, 
                        lt = lt)
  
  print(plot)
  
  return(list(ind, st, lt, plot))
  
}