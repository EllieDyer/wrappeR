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
#' @return A dataframe with processed model outputs to be passed to \code{calcMSI}.
#'         
#' @export
#' 

applyFilters <- function(roster) {
  
  samp_post <- tempSampPost(indata = paste0(roster$modPath, roster$group, "/"),
                            output_path = NULL,
                            REGION_IN_Q = paste0("psi.fs.r_", roster$region),
                            sample_n = roster$nSamps,
                            group_name = roster$group,
                            combined_output = TRUE,
                            #max_year_model = 2018,
                            #min_year_model = 1970,
                            write = FALSE)
  
  samp_post$species <- tolower(samp_post$species)
  
  meta <- extractMeta(inPath = paste0(roster$datPath),
                      group = as.character(roster$group),
                      outPath = roster$metaPath,
                      write = F,
                      region = roster$region)
  
  meta[ ,1] <- tolower(meta[,1])
  
  if (roster$clipBy != "species") {
    
    meta$min_year_data_r_GB <- min(meta$min_year_data_r_GB)
    
    meta$max_year_data_r_GB <- max(meta$max_year_data_r_GB)
    
  }
  
  if (any(!unique(samp_post$species) %in% meta$Species_r_GB)) {
    
    drop <- which(!samp_post$species %in% meta$Species_r_GB)
    
    samp_post <- samp_post[-drop, ]
    
  }

  if (roster$indicator == "priority") {
    
   prio <- sampSubset("priority",
                       inPath = roster$metaPath) 
    
    keep <- tolower(prio)
    
    drop <- NULL
    
  } else if (roster$indicator == "pollinators") {
    
    poll <- sampSubset("pollinators",
                       inPath = roster$metaPath)
    
    pollDrop <- sampSubset("poll2Drop",
                           inPath = roster$metaPath)
    
    keep <- tolower(poll)
    
    keep <- keep[-which(keep %in% tolower(pollDrop))] 
    
    drop <- NULL
    
  } else {
    
    keep <- NULL
    
    drop <- NULL
    
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
                                   keepSpecies = keep, 
                                   removeSpecies = drop,
                                   ClipFirst = TRUE, 
                                   ClipLast = TRUE)
  
  if (roster$write == TRUE) {
    
    save(stacked_samps, file = paste0(roster$outPath, roster$group, "_", roster$indicator, 
                                      "_", roster$region, ".rdata"))
    
  }
  
  return(meta)
  
}


