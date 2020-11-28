#' @importFrom parallel mclapply

tempSampPost <- function(indata = "../data/model_runs/", 
                         keep,
                         output_path = "../data/sampled_posterior_1000/",
                         REGION_IN_Q = "psi.fs",
                         sample_n = 1000,
                         group_name = "",
                         combined_output = TRUE,
                         max_year_model = NULL, 
                         min_year_model = NULL,
                         write = FALSE,
                         minObs = NULL,
                         t0, 
                         tn,
                         parallel = TRUE){
  
  ### set up species list we want to loop though ###
  spp.list <- list.files(indata, 
                         pattern = ".rdata") # species for which we have models
  
  spp.list <- gsub(".rdata", "", spp.list)
  
  spp.list <- spp.list[tolower(spp.list) %in% tolower(keep)]
  
  samp_post <- NULL # create the stacked variable, will be used if combined_output is TRUE.
  
  # loop through species
  
  combineSamps <- function(species, minObs) { 
    # NJBI this function refers to several global variables, e.g. tn - not good practice
    #print(species)
    out <- NULL
    raw_occ <- NULL
    load(paste0(indata, species, ".rdata"))
    
    nRec <- out$species_observations
    print(paste(species, nRec))
    
    if(nRec >= minObs) {
      raw_occ <- data.frame(out$BUGSoutput$sims.list[REGION_IN_Q])
      raw_occ <- raw_occ[sample(1:nrow(raw_occ), sample_n),]
      colnames(raw_occ) <- paste("year_", out$min_year:out$max_year, sep = "")
      raw_occ$iteration <- 1:sample_n
      raw_occ$species <- species
      
      if(combined_output != TRUE) {
        write.csv(raw_occ, file = paste(output_path, gsub(".rdata", "" ,i), "_sample_", sample_n, "_post_", REGION_IN_Q, ".csv", sep = ""), row.names = FALSE)
      } 
      
      out1 <- raw_occ
      
      dat <- out$model$data()
      dat <- data.frame(year = dat$Year,
                        rec = dat$y)
      
      #    nRec <- length(which(dat$rec == 1)) # this is VERY inefficient!
      
      #    if (nRec > 0) {
      
      first <- min(dat$year[dat$rec == 1]) + (t0 - 1)
      
      last <- max(dat$year[dat$rec == 1]) + (t0 - 1)
      
      firstMod <- t0
      
      lastMod <- tn
      
      yrs <- sort(unique(dat$year[dat$rec == 1]), decreasing = F)
      
      gaps <- NULL
      
      if (length(yrs) > 1) {
        
        for (i in (1:length(yrs) - 1)) {
          
          gaps <- c(gaps, yrs[i+1] - yrs[i])
          
        }
      }
      
      if (!is.null(gaps)) {
        
        gap <- max(gaps)
        
      } else {
        gap <- 1
      } 
      
      #    } else {  ## if no records then set other metadata arbitrarily high. This species will be dropped later anyway.
      #      first <- tn
      #      last <- tn 
      #      gap <- 200
      #     lastMod <- tn 
      #      firstMod <- tn
      #    }
      
      out2 <- data.frame(species, nRec, first, last, gap, firstMod, lastMod)
      return(list(out1, out2))
    } else return(NULL)
  }
  
  if(parallel) outputs <- parallel::mclapply(spp.list, 
                    combineSamps, minObs=minObs, mc.cores = 4)
  else outputs <- lapply(spp.list, 
                           combineSamps, minObs=minObs)
  
  
  if(parallel) samp_post <- parallel::mclapply(outputs, 
                                   function(x)  y <- x[[1]])
  else samp_post <- lapply(outputs, 
                      function(x)  y <- x[[1]])
  
  samp_post <- do.call("rbind", samp_post)
  
  if(parallel) meta <- parallel::mclapply(outputs, 
                              function(x) y <- x[[2]])
  else meta <- lapply(outputs, 
                 function(x) y <- x[[2]])
  
  meta <- do.call("rbind", meta)
  
  meta <- data.frame(Species = meta$species,
                     n_obs = meta$nRec,
                     min_year_data = meta$first,
                     max_year_data = meta$last,
                     min_year_model = meta$firstMod,
                     max_year_model = meta$lastMod,
                     gap_start = 0,
                     gap_end = 0,
                     gap_middle = meta$gap)
  
  colnames(meta) <- paste0(colnames(meta), "_r_", gsub("psi.fs.r_", "", REGION_IN_Q))
  
  if (write == TRUE) {
    save(samp_post, file = paste(output_path, group_name, "_all_spp_sample_", sample_n, "_post_", REGION_IN_Q, ".rdata", sep = ""))
  }
  
  return(list(samp_post, meta))
}



tempStackFilter <- function (input = "memory", dat = NULL, indata = NULL, output_path, group_name, metadata, region = NULL, 
                             minObs = NULL, maxStartGap = NULL, maxEndGap = NULL, maxMiddleGap = NULL, 
                             keepSpecies = NULL, removeSpecies = NULL, ClipFirst = FALSE, 
                             ClipLast = FALSE, write = FALSE) 
{
  if (input == "file") {
    load(indata)
  } else {
    stacked_samps <- dat
  }
  
  spp_list <- stacked_samps$species
  if (any(!is.null(c(minObs, maxStartGap, maxEndGap, maxMiddleGap)))) {
    if (is.null(metadata)) {
      stop("Metadata is required for this set of filters but not supplied")
    }
    if (length(setdiff(spp_list, metadata$Species)) > 0) {
      stop("Not all species have metadata available. Please ensure all species are included in the metadata and that all names match")
    }
  }
  if (!is.null(keepSpecies)) {
    if (length(setdiff(keepSpecies, spp_list)) > 0) {
      warning("Not all keepSpecies are contained within the indata")
    }
    if (!is.null(removeSpecies)) {
      if (length(setdiff(keepSpecies, removeSpecies) < 
                 length(keepSpecies))) {
        stop("Some species are listed under both keepSpecies and removeSpecies. Please ensure these vectors do not overlap")
      }
    }
  }
  if (!is.null(keepSpecies)) {
    stacked_samps <- droplevels(subset(stacked_samps, stacked_samps$species %in% 
                                         keepSpecies))
  }
  if (!is.null(removeSpecies)) {
    stacked_samps <- droplevels(subset(stacked_samps, !(stacked_samps$species %in% 
                                                          removeSpecies)))
  }
  if (is.null(region)) {
    if (!is.null(minObs)) {
      goodspecies <- droplevels(subset(metadata, metadata$n_obs >= 
                                         minObs))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
    if (!is.null(maxStartGap)) {
      goodspecies <- droplevels(subset(metadata, metadata$gap_start <= 
                                         maxStartGap))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
    if (!is.null(maxEndGap)) {
      goodspecies <- droplevels(subset(metadata, metadata$gap_end <= 
                                         maxEndGap))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
    if (!is.null(maxMiddleGap)) {
      goodspecies <- droplevels(subset(metadata, metadata$gap_middle <= 
                                         maxMiddleGap))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
  }
  else {
    if (!is.null(minObs)) {
      goodspecies <- droplevels(subset(metadata, metadata[, 
                                                          paste0("n_obs_r_", region)] >= minObs))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
    if (!is.null(maxStartGap)) {
      goodspecies <- droplevels(subset(metadata, metadata[, 
                                                          paste0("gap_start_r_", region)] <= maxStartGap))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
    if (!is.null(maxEndGap)) {
      goodspecies <- droplevels(subset(metadata, metadata[, 
                                                          paste0("gap_end_r_", region)] <= maxEndGap))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
    if (!is.null(maxMiddleGap)) {
      goodspecies <- droplevels(subset(metadata, metadata[, 
                                                          paste0("gap_middle_r_", region)] <= maxMiddleGap))
      stacked_samps <- droplevels(subset(stacked_samps, 
                                         stacked_samps$species %in% goodspecies$Species))
    }
  }
  if (ClipFirst == TRUE) {
    if (is.null(region)) {
      for (i in spp_list) {
        min_year_data <- metadata[metadata$Species == 
                                    i, "min_year_data"]
        min_year_model <- metadata[metadata$Species == 
                                     i, "min_year_model"]
        if (min_year_data > min_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% 
                          paste0("year_", min_year_model:(min_year_data - 
                                                            1))] <- NA
        }
      }
    }
    else {
      for (i in spp_list) {
        min_year_data <- metadata[metadata$Species == 
                                    i, paste0("min_year_data_r_", region)]
        min_year_model <- metadata[metadata$Species == 
                                     i, paste0("min_year_model_r_", region)]
        if (min_year_data > min_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% 
                          paste0("year_", min_year_model:(min_year_data - 
                                                            1))] <- NA
        }
      }
    }
  }
  if (ClipLast == TRUE) {
    if (is.null(region)) {
      for (i in spp_list) {
        max_year_data <- metadata[metadata$Species == 
                                    i, "max_year_data"]
        max_year_model <- metadata[metadata$Species == 
                                     i, "max_year_model"]
        if (max_year_data < max_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% 
                          paste0("year_", (max_year_data + 1):max_year_model)] <- NA
        }
      }
    }
    else {
      for (i in spp_list) {
        max_year_data <- metadata[metadata$Species == 
                                    i, paste0("max_year_data_r_", region)]
        max_year_model <- metadata[metadata$Species == 
                                     i, paste0("max_year_model_r_", region)]
        if (max_year_data < max_year_model) {
          stacked_samps[stacked_samps$species == i, names(stacked_samps) %in% 
                          paste0("year_", (max_year_data + 1):max_year_model)] <- NA
        }
      }
    }
  }
  
  if (write == TRUE) {
    save(stacked_samps, file = paste0(output_path, group_name, 
                                      "stacked_Filter", ".rdata")) 
  }
  
  return(stacked_samps)
}


