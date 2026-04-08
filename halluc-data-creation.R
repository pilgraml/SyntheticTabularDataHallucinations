# this script is to create populations with varying number of variables
data_name <- "born"

create.variants <- function(data_name){
  
  # loading source population and shuffling rows
  path_main <- paste0("PATH-TO-REAL-DATA", data_name, "/")
  assign("path_main", path_main, envir = .GlobalEnv)
  path_temp <- paste0("PATH-TO-SD/z-temporary-storage/")
  assign("path_temp", path_temp, envir = .GlobalEnv)
  path_real <- paste0("PATH-TO-SD", data_name, "/", "data_real/")
  assign("path_real", path_real, envir = .GlobalEnv)
  pop_max <- read.csv(paste0(path_main, data_name, "_max.csv")) 
  df_max <- pop_max[sample(nrow(pop_max)), ] 
  row.names(df_max) <- NULL
  saveRDS(df_max, paste0(path_real, data_name, "_max.rds"))
  assign("df_max", df_max, envir = .GlobalEnv)
  
  # loading fixed variables (vars_min)
  vars_min <- readRDS(paste0(path_main, data_name, "_code/", 
                             data_name, "_model-vars.rds"))
  assign("vars_min", vars_min, envir = .GlobalEnv)
  
  # v: number of variables to add
  # v_max: max. potential number of variables to add
  v_max <- ncol(df_max) - length(vars_min)
  
  # calculating max. combinations given any v out of v_max for that dataset
  vars_comb <- 2^v_max
  
  # defining max. potential variables that can be added
  vars_diff <- setdiff(colnames(df_max), vars_min)
  
  # if max. combinations < 600: create all possible combinations, add v = 0
  if (vars_comb < 600){ 
    vars_add_lst <- lapply(1:v_max, function(x) combn(vars_diff, x, simplify = FALSE))
    
    # if max. combinations >= 600: create multiple (n_v) combinations for each v with a weight (w_v) favoring small v
  } else { 
    d <- sum(sapply(1:v_max, function(i) {log ( choose(v_max, i) )}))
    
    # ceiling number of added variables at 120
    if (v_max <= 120){
      v_add <- v_max
      w_v_lst <- sapply(1:v_add, function(v) {log(choose(v_max, v))/d})
      n_v_lst <- sapply(w_v_lst, function(w_v) {max(5, ceiling(w_v * 600))})
      n_v_lst[[v_max]] <- 1
    } else {
      v_add <- 120
      w_v_lst <- sapply(1:v_add, function(v) {log(choose(v_max, v))/d})
      n_v_lst <- sapply(w_v_lst, function(w_v) {max(5, ceiling(w_v * 600))})
    }
    vars_add_lst <- lapply(seq_along(n_v_lst), function(v){
      vars_add_v <- replicate(n_v_lst[[v]], sample(vars_diff, v), simplify = FALSE)
    })
  }
  
  # create list of variables that defines for each sub-population its variables including the fixed variables
  vars_add_lst <- unlist(vars_add_lst, recursive = FALSE)
  vars_lst <- lapply(vars_add_lst, function(x) c(vars_min, x))
  vars_lst <- c(list(vars_min), vars_lst)
  vars_lst <- lapply(vars_lst, sample) # shuffling variables
  
  # temporary store vars_lst and assign to global environment
  assign("vars_lst", vars_lst, envir = .GlobalEnv)
  saveRDS(vars_lst, paste0(path_temp, "vars/", data_name, "_vars.rds"))
  
  # defining datatypes
  df_max_info <- jsonlite::fromJSON(paste0(path_main, data_name, "_max.json"))
  assign("df_max_info", df_max_info, envir = .GlobalEnv)
  cnt_idxs <- unlist(df_max_info$cnt_idxs) + 1
  cnt_vars <- colnames(df_max[cnt_idxs])
  assign("cnt_vars", cnt_vars, envir = .GlobalEnv)
  dscrt_idxs <- unlist(df_max_info$dscrt_idxs) + 1
  dscrt_vars <- colnames(df_max[dscrt_idxs])
  assign("dscrt_vars", dscrt_vars, envir = .GlobalEnv)
  cat_idxs <- unlist(df_max_info$cat_idxs) + 1
  cat_vars <- colnames(df_max[cat_idxs])
  assign("cat_vars", cat_vars, envir = .GlobalEnv)
  datetime_idxs <- unlist(df_max_info$datetime_idxs) + 1
  datetime_vars <- colnames(df_max[datetime_idxs])
  assign("datetime_vars", datetime_vars, envir = .GlobalEnv)
  quasi_idxs <- unlist(df_max_info$quasi_idxs) + 1
  quasi_vars <- colnames(df_max[quasi_idxs])
  assign("quasi_vars", quasi_vars, envir = .GlobalEnv)
  downstream_idxs <- unlist(df_max_info$downstream_idxs) + 1
  downstream_vars <- colnames(df_max[downstream_idxs])
  assign("downstream_vars", downstream_vars, envir = .GlobalEnv)
  miss_val <- df_max_info$miss_vals
  assign("miss_val", miss_val, envir = .GlobalEnv)
  
  # apply to each variable selection the following steps: parallel execution
  
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  invisible(parallel::clusterExport(cl, varlist = c("data_name", "path_real", "vars_lst", "vars_min", "df_max_info", "df_max", 
                                                    "cnt_vars", "dscrt_vars", "cat_vars", "datetime_vars", "quasi_vars", 
                                                    "downstream_vars", "miss_val")))
  pbapply::pblapply(seq_along(vars_lst), function(idx){
    vars <- vars_lst[[idx]]
    v_add <- length(vars) - length(vars_min)
    
    # create corresponding json files
    df_json <- df_max_info
    df_json$cnt_idxs <- sort(match(intersect(cnt_vars, vars), vars)) - 1
    df_json$dscrt_idxs <- sort(match(intersect(dscrt_vars, vars), vars)) - 1
    df_json$cat_idxs <- sort(match(intersect(cat_vars, vars), vars)) - 1
    df_json$datetime_idxs <- sort(match(intersect(datetime_vars, vars), vars)) - 1
    df_json$quasi_idxs <- sort(match(intersect(quasi_vars, vars), vars)) - 1
    df_json$downstream_idx <- sort(match(intersect(downstream_vars, vars), vars)) - 1
    json_data <- jsonlite::toJSON(df_json)
    writeLines(json_data, paste0(path_real, "lp_", data_name, "_", idx, "_v", v_add, ".json"))
    
    # create corresponding sub-population 
    pop <- df_max[, vars]
    saveRDS(pop, paste0(path_real, "lp_", data_name, "_", idx, "_v", v_add, "_pop.rds"))
    
    # create corresponding training dataset (note: population records have been shuffled beforehand)
    train <- pop[1:10000,]
    saveRDS(train, paste0(path_real, "lp_", data_name, "_", idx, "_v", v_add, "_train.rds"))
    
    # create corresponding test dataset (note: population records have been shuffled beforehand)
    test <- pop[10001:20000,]
    saveRDS(test, paste0(path_real, "lp_", data_name, "_", idx, "_v", v_add, "_test.rds"))
  }, cl = cl)
  
  # remove temporary stored vars_lst after successful storage of dataframes
  parallel::stopCluster(cl)
  file.remove(paste0(path_temp, "vars/", data_name, "_vars.rds")) 
}

time_variants <- system.time({
  create.variants(data_name)
})

saveRDS(time_variants, paste0(path_temp, data_name, "_timing_variants_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Synthetic data generation
rm(list = setdiff(ls(), "data_name"))



generate.synth <- function(data_name){
  
  # retrieving number of sub-populations
  path_main <- paste0("PATH-TO-REAL-DATA", data_name, "/")
  assign("path_main", path_main, envir = .GlobalEnv)
  path_temp <- paste0("PATH-TO-SD/temporary-storage/data/")
  assign("path_temp", path_temp, envir = .GlobalEnv)
  path_real <- paste0("PATH-TO-SD", data_name, "/", "data_real/")
  assign("path_real", path_real, envir = .GlobalEnv)
  path_synth <- paste0("PATH-TO-SD", data_name, "/", "data_synth/")
  assign("path_synth", path_synth, envir = .GlobalEnv)
  files <- list.files(path = path_real, pattern = "\\_train.rds$")
  train_no <- length(files)
  assign("train_no", train_no, envir = .GlobalEnv)
  
  # apply to each variable selection the following steps: parallel execution
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  invisible(parallel::clusterExport(cl, varlist = c("data_name", "train_no", "path_main", "path_real", 
                                                    "path_synth", "path_temp")))
  invisible(parallel::clusterEvalQ(cl, expr = {
    reticulate::use_condaenv("pysdg-R", required = TRUE)
    synth.generate <- reticulate::import("pysdg.synth.generate")
    synth.load <- reticulate::import("pysdg.synth.load")
    pandas <- reticulate::import("pandas")
  }))
  pbapply::pblapply(1:train_no, function(idx){
    
    # temporary store training data as csv file 
    train_filename <- list.files(path = path_real, pattern = paste0(data_name, "_", idx, "_v\\d+_train.rds$"))
    real_rds <- readRDS(paste0(path_real, train_filename))
    path_data <- paste0(path_temp, "lp_", data_name, "_", idx, "_train.csv")
    write.csv(real_rds, path_data, row.names = FALSE)
    json_filename <- list.files(path = path_real, pattern = paste0(data_name, "_", idx, "_v\\d+.json$"))
    path_json <- paste0(path_real, json_filename)
    
    # generate synthetic data using 7 generators, 10 iterations and 10,000 records each
    gen_lst <- c("synthcity_ctgan", "synthcity_bayesian_network", "replica_seq", 
                 "synthcity_tvae", "synthcity_rtvae", "synthcity_nflow", "synthcity_arf")
    
    sdg_lst <- lapply(gen_lst, function(gen_name){
      
      # excluding Sequential Tree by Replica because of running issues
      if (gen_name == "replica_seq"){
        #setwd("~/personal")
        sdg <- list(syn_data_lst = list(), generator = gen_name)
      }else{
      
        # in case of AssertionError: retry one time, otherwise return empty list
        sdg <- tryCatch({
          gen <- synth.generate$Generator(gen_name)
          real <- gen$load(path_data, path_json)
          gen$train()
          gen$gen(no_obsvs=10000, no_synths=10)
          synths <- gen$unload()
          sdg <- list(syn_data_lst = synths, generator = gen_name)
          return(sdg)
        }, error = function(e) {
          if (grepl("AssertionError", e$message)) {
            message("Caught AssertionError, retrying...")
            sdg <- NULL
            return(sdg)  
          }
          stop(e)  
        })
        if (is.null(sdg)) {
          sdg <- tryCatch({
            gen <- synth.generate$Generator(gen_name)
            real <- gen$load(path_data, path_json)
            gen$train()
            gen$gen(no_obsvs=10000, no_synths=10)
            synths <- gen$unload()
            sdg <- list(syn_data_lst = synths, generator = gen_name)
            return(sdg)
          }, error = function(e) {
            sdg <- list(syn_data_lst = list(), generator = paste0(gen_name, ": AssertionError"))
            return(sdg)  
          })  
        }}
      })
    
      # storing soul (enforced json alignment), test (enforced json alignment) and additional metadata
      gen <- synth.generate$Generator(gen_lst[[2]])
      real <- gen$load(path_data, path_json)
      sdg_lst$real_training <- real
      test_filename <- list.files(path = path_real, pattern = paste0(data_name, "_", idx, "_v\\d+_test.rds$"))
      test_rds <- readRDS(paste0(path_real, test_filename))
      path_test <- paste0(path_temp, "lp_", data_name, "_", idx, "_test.csv")
      write.csv(test_rds, path_test, row.names = FALSE)
      test <- gen$load(path_test, path_json)
      sdg_lst$real_holdout <- test
      pop_filename <- list.files(path = path_real, pattern = paste0(data_name, "_", idx, "_v\\d+_pop.rds$"))
      pop_rds <- readRDS(paste0(path_real, pop_filename))
      path_pop <- paste0(path_temp, "lp_", data_name, "_", idx, "_pop.csv")
      write.csv(pop_rds, path_pop, row.names = FALSE)
      population <- gen$load(path_pop, path_json)
      sdg_lst$real_population <- population
      vars_min <- readRDS(paste0(path_main, data_name, "_code/", 
                                 data_name, "_model-vars.rds"))
      v_add <- ncol(real) - length(vars_min)
      info <- jsonlite::fromJSON(path_json)
      population_size <- info$population_size
      sdg_lst$parameters <- list(data_name = data_name, unique_id = idx, v_total = ncol(real), 
                                 v_add = v_add, population_size = population_size,json_file = info)
    
      saveRDS(sdg_lst, paste0(path_synth, "lp_", data_name, "_", idx, "_v", v_add, "_sdg.rds"))
      file.remove(path_data)
      file.remove(path_pop)
      file.remove(path_test)
      }, cl = cl)
  parallel::stopCluster(cl)
}

time_sdg <- system.time({
  generate.synth(data_name)
})

saveRDS(time_sdg, paste0(path_temp, data_name, "_timing_sdg_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
