#' \code{summariseMSI} - Summarise the multispecies indicator for reporting to e.g. JNCC.
#' 
#' @description This function takes the output of \code{calcMSI} and summarises the outputs either as a plot or in the 
#'              format JNCC require for the UK biodiversity indicators.
#'
#' @param minYear Numeric. First year of the indicator.
#' 
#' @param maxYear Numeric. Final year of the indicator.
#' 
#' @param label String. title for indicator plot.
#'                  
#' @param plotType String. Type of plot to produce. Options are "indicator" to plot the indicator, or "nSpecies"
#'                 to plot the number of species contributing to each year of the indicator. 
#' 
#' @param method String. Method used to calculate the indicator. One of "lambda" or "bma".
#' 
#' @param indicator String. Object returned by \code{calcMSI}.
#' 	  
#' @return If method = "lambda" returns summary stats for reporting to JNCC. Otherwise produces a plot of he indicator.
#'         
#' @export
#'

summariseMSI <- function(minYear, 
                         maxYear, 
                         label, 
                         plotType, 
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
  
  p1 <- ggplot(data = NULL, aes(x= years, y= indicator$Summary$indicator)) +
    geom_ribbon(data= NULL, aes(ymax= indicator$Summary$upper, ymin = indicator$Summary$lower), fill = "grey80") +
    geom_line() +
    geom_point() +
    theme_linedraw() +
    ylab("Occupancy index") +
    xlab("") +
    ylim(c(0, 180)) +
    ggtitle(label) +
    annotate("text", x=1985, y=30, label= paste(n, "species"))
  
  if (method == "lambda") {
    
    st <- indicator$st$species_assessment$category
    
    st <- data.frame(st, rep(as.factor(1), length(st)))
    
    colnames(st) <- c("val","type")
    
    lt <- indicator$lt$species_assessment$category
    
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
  
  indicator$Summary$year <- indiccator$Summary$year + (startYear - 1)
  
  out1 <- indicator$Summary
  
  changeLT <- data.frame(table(indicator$metaData$species_change$category))
  
  rawLT <- indicator$metaData$species_change[,1]
  
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