# this script is for postprocessing created datasets
data_name <- "covid"

synth.postproc <- function(data_name){
  
  # retrieving number of sub-populations
  path_synth <- paste0("PATH-TO-SD", data_name, "/", "data_synth/")
  assign("path_synth", path_synth, envir = .GlobalEnv)
  files <- list.files(path = path_synth, pattern = "\\_sdg.rds$")
  synth_no <- length(files)
  assign("synth_no", synth_no, envir = .GlobalEnv)
  
  # apply to each variable selection the following steps: parallel execution
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  invisible(parallel::clusterExport(cl, varlist = c("data_name", "synth_no", "path_synth")))
  error_df_lst <- pbapply::pblapply(1:synth_no, function(idx){
    
    # loading sdg file  
    sdg_filename <- list.files(path = path_synth, pattern = paste0(data_name, "_", idx, "_v\\d+_sdg.rds$"))
    sdg <- readRDS(paste0(path_synth, sdg_filename))
    
    # 7 generators: check for synthetic data completeness
    gen_lst <- c("synthcity_ctgan", "synthcity_bayesian_network", "replica_seq", 
                 "synthcity_tvae", "synthcity_rtvae", "synthcity_nflow", "synthcity_arf")
    
    # MemoryIssue in the case of replica_seq otherwise as AssertionError
    error_lst <- lapply(1:length(gen_lst), function(gen){
      gen_name <- gen_lst[[gen]]
      syn_lst <- sdg[[gen]]$syn_data_lst
      if (length(syn_lst) == 0 & gen_name == "replica_seq"){
        error <- "MemoryIssue"
        generator <- gen_name
        filename <- sdg_filename
      }else if (length(syn_lst) == 0 & gen_name == "synthcity_nflow") {
        error <- "AssertionError"
        generator <- gen_name
        filename <- sdg_filename
      }else if (length(syn_lst) == 0 & gen_name == "synthcity_rtvae") {
        error <- "SoftmaxNaNError"
        generator <- gen_name
        filename <- sdg_filename
      }else{
        error <- NA
        generator <- gen_name
        filename <- sdg_filename
      }
      result <- c(filename=filename, generator=generator, error=error)
      return(result)
    })
    error_df <- do.call(rbind, error_lst)
    error_df <- error_df[complete.cases(error_df),]
    
    # rename real to real_training
    if ("real" %in% names(sdg)){
      names(sdg)[names(sdg) == "real"] <- "real_training"
      saveRDS(sdg, paste0(path_synth, sdg_filename))
    }
    
    # returning error_df per sdg
    return(error_df)
    }, cl = cl)
  parallel::stopCluster(cl)
  error <- do.call(rbind, error_df_lst)
  if (nrow(error) != 0){
    saveRDS(error, paste0("PATH-TO-SD", data_name, "/", 
                          "lp_", data_name, "_sdg_errors.rds"))
  }
  return(error)
}

error <- synth.postproc(data_name)
