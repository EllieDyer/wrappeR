#' \code{applyFilters} - Apply filters to occupancy model outputs.
#' 
#' @description This function can be used to subset the occupancy model
#'              outputs based on number of records, priority/ pollinator status
#'              and region. It works at the level of the taxonomic group so
#'              must be applied across multiple groups if needed.
#'
#' @param roster String. A dataframe with columns: datPath, modPath, ver, indicator, region,
#'               nSamps, minObs, write, outPath, clipBy, group (see \code{createRoster}). 
#'
#' @param meta metadata produced by applySamp
#'               
#' @param parallel Boolean. Should the operatoin run in parallel? If so then use n.cores-1.
#' 	  
#' @return A dataframe with processed model outputs to be passed to \code{calcMSI}.
#'         
#' @export
#' 

applyClipping <- function(data, roster, meta, parallel = TRUE) {
  
  
  samp_post <- data[[1]]
  meta <- data[[2]]
  
  if (roster$clipBy != "species") {
    meta[,3] <- min(meta[,3])
    meta[,4] <- max(meta[,4])
  }

  
  stacked_samps <- tempStackFilter(input = "memory",
                                   dat = samp_post,
                                   indata= NULL,
                                   output_path=NULL, 
                                   group_name=paste0(roster$indicator, roster$group), 
                                   metadata=meta, 
                                   region = roster$region,
                                   minObs = roster$minObs, 
                                   maxStartGap = 0, 
                                   maxEndGap = 0,
                                   maxMiddleGap = 10, 
                                   keepSpecies = NULL, 
                                   removeSpecies = NULL,
                                   ClipFirst = TRUE, 
                                   ClipLast = TRUE)
  
  # are we ever going to want to do this within wrappeR?
  #if (roster$write == TRUE) {
  #  save(stacked_samps, file = paste0(roster$outPath, roster$group, "_", roster$indicator, 
  #                                    "_", roster$region, ".rdata"))
  #}
  
  return(stacked_samps)
  
}
