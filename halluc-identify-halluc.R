# this script is to calculate the hallucination rate
data_name_lst <- c("born", "california", "cchs", "covid", "faers", "florida", "mimic", "newyork", "nexoid", "texas", "washington", "washington2008")
data_name <- "washington"

## calculation of HR
calc.halluc <- function(syn_data, pop_data, num_vars, datetime_vars){
  pop_data_discr <- pop_data
  syn_data_discr <- syn_data
  pop_data_discr$origin <- "R"
  syn_data_discr$origin <- "S"
  data_combined <- rbind(pop_data_discr, syn_data_discr)
  
  # transform datetime variables
  data_combined[datetime_vars] <- lapply(data_combined[datetime_vars], 
                                         function(x) as.numeric(as.Date(x) - as.Date("1900-01-01")))
  num_vars <- c(num_vars, datetime_vars)
  
  # transform numerical variables
  data_combined[num_vars] <- lapply(num_vars, function(x){
    if (length(unique(data_combined[[x]])) > 20){
      data_combined[[x]] <- arules::discretize(data_combined[[x]], method = "interval", breaks = 20)
    } else {
      data_combined[[x]] <- round(data_combined[[x]])
    }
    data_combined[[x]] <- as.character(data_combined[[x]])
    return(data_combined[[x]])
  })
  pop_data_discr <- data_combined[data_combined$origin == "R", ]
  syn_data_discr <- data_combined[data_combined$origin == "S", ]
  pop_data_discr$origin <- NULL
  syn_data_discr$origin <- NULL
  
  # isolate hallucinations while considering NA as a category
  unique_antijoin <- dplyr::anti_join(syn_data_discr, pop_data_discr, 
                                      by = colnames(syn_data_discr)[-ncol(syn_data_discr)], 
                                      na_matches = "na")
  
  # calculate hallucination rate
  hr <- nrow(unique_antijoin)/nrow(syn_data)
  
  return(syn_halluc_rate = hr)
}

## process function
halluc.idx <- function(data_name){
  
  # retrieving input
  path_main <- paste0("PATH-TO-REAL-DATA", data_name, "/")
  path_synth <- paste0("PATH-TO-SD", data_name, "/", "data_synth/")
  path_results <- paste0("PATH-TO-RESULTS", data_name, "/")
  assign("path_results", path_results, envir = .GlobalEnv)
  files <- list.files(path = path_synth, pattern = "\\_sdg.rds$")
  sdg_no <- length(files)
  rm(files)
  
  # per population variant
  lapply(1:sdg_no, function(idx){
    
    # define input data
    sdg_filename <- list.files(path = path_synth, pattern = paste0(data_name, "_", idx, "_v\\d+_sdg.rds$"))
    sdg <- readRDS(paste0(path_synth, sdg_filename))
    pop_data <- sdg$real_population
    datetime_vars <- colnames(pop_data)[sapply(pop_data, 
                                               function(x) inherits(x, c("POSIXct", "POSIXlt", "Date")))]
    num_vars <-  colnames(pop_data)[sapply(pop_data, is.numeric)]
    v_add <- sdg$parameters$v_add
    
    # calculate complexity
    compl_vc <- prod(sapply(colnames(training_data), function(colname){
      var <- training_data[, colname]
      if (colname %in% c(num_vars, datetime_vars) & length(unique(var)) > 20){
        unique_values <- 20
      }
      else{
        unique_values <- length(unique(var))
      }
      return(unique_values)
    }))
    
   # try all generators
      lapply(1:7, function(gen){
        gen_name <- sdg[[gen]]$generator
        syn_lst <- sdg[[gen]]$syn_data_lst
        path_output_gen <- paste0(path_results, data_name, "_", idx, "_v", v_add, "_", gen_name,
                                  "_halluc-avg.rds")
        
        # if file is already available, jump to next one
        if(!file.exists(path_output_gen)){
          
          # if synthetic data is available calculate hallucinations
          if (!length(syn_lst) == 0){
            
            results <- lapply(1:10, function (i){
              syn_data <- syn_lst[[i]]
              syn_halluc <- calc.halluc(syn_data, pop_data, num_vars, datetime_vars)
              return(list(syn_halluc = syn_halluc))
            })
            rm(syn_lst)
      
            # average across datasets
            syn_halluc_avg <- mean(sapply(results, function(i) i$syn_halluc), na.rm = TRUE)
            rm(results)
            
            # add complexity
            halluc <- list(compl_vc = compl_vc, syn_halluc_avg = syn_halluc_avg)
            
            # save per generator
            saveRDS(halluc, path_output_gen)
          }}
      })
      # empty storage
      rm(list = c("sdg", "pop_data", "datetime_vars", "num_vars"))
      gc()
})}

## run function
time_halluc <- system.time({
  halluc.idx(data_name)
})
