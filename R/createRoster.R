#' \code{createRoster} - Specify how to filter the occupancy model outputs.
#' 
#' @description This function can be used to specify the filters that will be applied to 
#'              the occupancy model outputs using \code{applyFilters}. It works by creating a list
#'              of 1-row dataframes with all the information needed for \code{applyFilters}. This list 
#'              is then applied to the applyFilters function later. Arguments should be provided
#'              as vectors of equal length, with each element in the vectors corresponding to one 
#'              call to \code{applyFilters}. 
#'              
#' @param modPath String. Location of the occupancy model outputs. 
#' 
#' @param ver String. Which set of occupancy model outputs? E.g. Charlie's are "2017_Charlie".
#' 
#' @param indicator String. Whether or not to subset species and, if so, based
#'                  on what. Options are: "priority" for priority species; 
#'                  "pollinators" for pollinators; and all to return all species in the group.
#'                  
#' @param region String. One of "UK", "GB", "England", "Wales", "Scotland", 
#'               or "NorthernIreland".
#'
#' @param nSamps Numeric. Number of samples to extract from each species' 
#'               posterior distribution.
#'               
#' @param minObs Numeric. Threshold number of observation below which a 
#'               species is dropped from the sample.
#'               
#' @param write Logical. If TRUE then the outputs are written as a 
#'              .rdata file to outPath.
#'              
#' @param outPath String. Location to store the outputs if write = TRUE. 
#'  	  
#' @param clipBy String. One of "species" or "group" indicating whether to clip outputs by the first and last years of data for
#'        each species or for the whole group, respectively.
#' 	  
#' @return A list of 1-row dataframes containing all arguments needed for applyFilters. applyFilters
#'         can then be applied to this list to filter models outputs for different groups, which may 
#'         come from different rounds (e.g. Charlie's or later), for different regions, etc. 
#'         
#' @export
#' 

createRoster <- function(index,
                         modPath = "/data-s3/occmods/", 
                         metaPath,
                         ver = "2017_Charlie",
                         group, 
                         indicator, 
                         region,
                         nSamps = 1000,
                         minObs = 50,
                         write,
                         outPath,
                         clipBy = "species",
                         t0,
                         tn) {
  
  df <- data.frame(index = index,
                   modPath = modPath,
                   datPath = paste0("/data-s3/occmods/",
                                    group, "/", ver, "/"),
                   metaPath = metaPath,
                   ver = ver, 
                   group = group, 
                   indicator = indicator,
                   region = region, 
                   nSamps = as.numeric(nSamps), 
                   minObs = minObs, 
                   write = write, 
                   outPath = outPath,
                   clipBy = clipBy,
                   t0 = t0,
                   tn = tn)
  
  roster <- split(df, seq(nrow(df)))
  
}