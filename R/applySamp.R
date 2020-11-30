#' \code{applySamp} - Reads a series of occupancy model and summarises the outputs

#' @param roster list
#' @param parallal Logical
#' @param sample Logical. Should the model sample from the posterior distibution or just get the a parameters instead?
#' @export
#' 

applySamp <- function(roster, parallel = TRUE, sample = TRUE) {
  
  if (roster$indicator == "priority") {
    
    keep <- sampSubset("priority",
                       inPath = roster$metaPath) 
    
  } else if (roster$indicator == "pollinators") {
    
    keep <- sampSubset("pollinators",
                       inPath = roster$metaPath)
    
  } else {
    
    keep <- gsub(".rdata", "", list.files(paste0(roster$modPath, roster$group, "/occmod_outputs/", roster$ver, "/"),
                                          pattern = ".rdata")) 
    
  }
  
  if(sample)
    out <- tempSampPost(indata = paste0(roster$modPath, roster$group, "/occmod_outputs/", roster$ver, "/"),
                      keep = keep,
                      output_path = NULL,
                      REGION_IN_Q = paste0("psi.fs.r_", roster$region),
                      sample_n = roster$nSamps,
                      group_name = roster$group,
                      combined_output = TRUE,
                      #max_year_model = 2018,
                      #min_year_model = 1970,
                      write = FALSE,
                      minObs = roster$minObs,
                      t0 = roster$t0,
                      tn = roster$tn,
                      parallel = parallel)
  else
    out <- getA(indata = paste0(roster$modPath, roster$group, "/occmod_outputs/", roster$ver, "/"),
                        keep = keep,
                        REGION_IN_Q = paste0("a_", roster$region),
                        group_name = roster$group,
                        combined_output = TRUE,
                        write = FALSE,
                        minObs = roster$minObs,
                        t0 = roster$t0,
                        tn = roster$tn,
                        parallel = parallel)
  
  samp_post <- out[[1]]
  
  samp_post$species <- tolower(samp_post$species)
  
  meta <- out[[2]]
  
  meta[ ,1] <- tolower(meta[, 1])
  
  return(list(samp_post = samp_post, 
              meta = meta,
              indicator = roster$indicator,
              group_name = roster$group,
              region = roster$region,
              clipBy = roster$clipBy,
              minObs = roster$minObs))
}