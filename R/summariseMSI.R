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

summariseMSI <- function(minYear, 
                         maxYear, 
                         label, 
                         plotType, 
                         st = NULL, 
                         lt = NULL, 
                         final = NULL,
                         write,
                         indicator,
                         method) {


  if (is.null(st) | is.null(lt)) {
  
  warning("If you are using the lambda indicator you must specify lt and st")
    
  }
  
n <- max(indicator$summary$Species_Number)

nSpec <- indicator$summary$Species_Number

years <- minYear:maxYear

## plot the indicator 

if (plotType == "indicator") {
  
  p1 <- ggplot(data = NULL, aes(x= years, y= indicator$summary$indicator)) +
    geom_ribbon(data= NULL, aes(ymax= indicator$summary$upper, ymin = indicator$summary$lower), fill = "grey80") +
    geom_line() +
    geom_point() +
    theme_linedraw() +
    ylab("Occupancy index") +
    xlab("") +
    ylim(c(0, 180)) +
    ggtitle(label) +
    annotate("text", x=1985, y=30, label= paste(n, "species"))
  
  if (method == "lambda") {
    
    st <- st$species_assessment$category
    
    st <- data.frame(st, rep(as.factor(1), length(st)))
    
    colnames(st) <- c("val","type")
    
    lt <- lt$species_assessment$category
    
    lt <- data.frame(lt, rep(as.factor(2), length(lt)))
    
    colnames(lt) <- c("val","type")
    
    dat <- rbind(st,lt)
    
    p2 <- ggplot(dat, aes(x = factor(type), fill = forcats::fct_rev(val))) +
      geom_bar(position="fill") +
      theme_linedraw() +
      ylab("Proportion of species") +
      xlab("") +
      scale_x_discrete(labels=c("Short term","Long term")) +
      guides(fill=guide_legend(title=""))
    
    
    print(grid.arrange(p1, p2, ncol=2))
    
  } else {
    
    print(p1)
    
  }
  
  
  
} else if (plotType == "nSpecies") {
  
  p1 <- ggplot(data = NULL, aes(x= years, y= ind$summary$Species_Number)) +
    geom_line() +
    geom_point() +
    theme_linedraw() +
    ylab("Number of species contributing") +
    xlab("") +
    ggtitle(label)
  
  print(p1)
  
} else {
  
  width <- summary$upper - summary$lower
  
  p1 <- ggplot(data = NULL, aes(y= width, x= ind$summary$Species_Number)) +
    geom_point() +
    theme_linedraw() +
    ylab("Width of credible interval") +
    xlab("Number of species contributing") +
    ggtitle(label)
  
  print(p1)
  
}

## now generate summary stats for reporting 

if (method == "lambda") {
  
  indicator$summary$year <- ind$summary$year + (startYear - 1)
  
  out1 <- indicator$summary
  
  changeLT <- data.frame(table(indicator$species_change$category))
  
  rawLT <- indicator$species_change[,1]
  
  L <- length(rawLT)
  
  changeST <- data.frame(table(indicator$st$species_assessment$category))[,2]
  
  rawST <- indicator$st$species_assessment[,1]
  
  rawST <- c(rawST, rep(NA, (L - length(rawST))))
  
  changeFinal <- data.frame(table(indicator$final$species_assessment$category))[,2]
  
  rawFinal <- indicator$final$species_assessment[,1]
  
  rawFinal <- c(rawFinal, rep(NA, (L - length(rawFinal))))
  
  change <- data.frame(changeLT, changeST, changeFinal)
  
  colnames(change) <- c("Trend", "Long term", "Short term", "Final year")
  
  raw <- data.frame(rawLT, rawST, rawFinal)
  
  colnames(raw) <- c("Long term", "Short term", "Final year")
  
  out <- list(out1, change, raw)
  
} else {
  
  out <- p1
  
}

}