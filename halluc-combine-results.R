# this script is to combine the evaluation results

# required libraries 
library(dplyr)

# paths for each evaluation metric
gen_lst <- c("synthcity_ctgan", "synthcity_bayesian_network", "replica_seq", 
             "synthcity_tvae", "synthcity_rtvae", "synthcity_nflow", "synthcity_arf")
data_name_lst <- c("born", "california", "cchs", "covid", "faers", "florida", "mimic", 
                   "newyork", "nexoid", "texas", "washington", "washington2008")
path_halluc <- "PATH-TO-RESULTS-HALLUC"
path_mlp <- "PATH-TO-RESULTS-MLP"
path_lr <- "PATH-TO-RESULTS-LR"
path_mmbrshp <- "PATH-TO-RESULTS-MMBRSHP"
path_sdg <- "PATH-TO-SD"
path_results <- "PATH-TO-RESULTS"

# read in and combine individual results to summary table
## create basic characteristics: idx, data_name, v_add, generator
results_list <- lapply(data_name_lst, function(data_name){
  files <- list.files(path = paste0(path_sdg, "/", data_name, "/", "data_synth/"))
  results_data <- lapply(files, function(file){
    idx <- sub(paste0(".*_", data_name, "_(\\d+)_v(\\d{1,3})_sdg\\.rds$"), "\\1", file)
    v_add <- sub(paste0(".*_", data_name, "_(\\d+)_v(\\d{1,3})_sdg\\.rds$"), "\\2", file)
    results <- c(data_name = data_name, idx = idx, v_add = v_add)
    return(results)
  })
  results_data <- do.call(rbind, results_data)
  return(results_data)
})
results_df <- as.data.frame(do.call(rbind, results_list))
results_df <- results_df[rep(1:nrow(results_df), each = 7), ]
results_df$generator <- rep(gen_lst, times = nrow(results_df)/7)

## complexity and hallucination rate
halluc_list <- lapply(data_name_lst, function(data_name){
  files <- list.files(path = paste0(path_halluc, "/", data_name, "/"))
  halluc_lst <- lapply(files, function(file){
    halluc <- readRDS(paste0(path_halluc, "/", data_name, "/", file))
    compl_vc <- halluc$compl_vc
    syn_halluc_avg <- halluc$syn_halluc_avg
    generator <- sub("_halluc-avg.rds$", "", sub(paste0("^", data_name, "_\\d+_"), "", file))
    idx <- sub(paste0("^", data_name, "_(\\d+)_", generator, "_.*$"), "\\1", file)
    results <- c(idx = idx, data_name = data_name, generator = generator, syn_halluc_avg = syn_halluc_avg)
    return(results)
  })
  halluc_df <- do.call(rbind, halluc_lst)
  return(halluc_df)
})
halluc_df <- as.data.frame(do.call(rbind, halluc_list))
results_compl_halluc <- merge(results_df, halluc_df, by = c("data_name", "idx", "generator"), all.x = T)

## downstream prediction utility: lgbm 
lgbm_list <- lapply(data_name_lst, function(data_name){
  files <- list.files(path = paste0(path_lgbm, "/", data_name, "/"), pattern = "downstream-lgbm-results.rds$")
  lgbm_lst <- lapply(files, function(file){
    syn_lgbm_avg <- readRDS(paste0(path_lgbm, "/", data_name, "/", file))
    generator <- sub("_downstream-lgbm-results.rds$", "", sub(paste0("^", data_name, "_\\d+_"), "", file))
    idx <- sub(paste0("^", data_name, "_(\\d+)_", generator, "_.*$"), "\\1", file)
    results <- c(idx = idx, data_name = data_name, generator = generator, syn_lgbm_avg = syn_lgbm_avg)
    return(results)
  })
  lgbm_df <- do.call(rbind, lgbm_lst)
  return(lgbm_df)
})
lgbm_df <- as.data.frame(do.call(rbind, lgbm_list))

lgbm_trtr_lst <- lapply(data_name_lst, function(data_name){
  file <- list.files(path = paste0(path_lgbm, "/", data_name, "/"), pattern = "downstream-lgbm-trtr-results.rds$")
  trtr_lgbm <- readRDS(paste0(path_lgbm, "/", data_name, "/", file))[[1]]
  results <- c(data_name = data_name, train_lgbm = trtr_lgbm)
  return(results)
})
lgbm_trtr_df <- as.data.frame(do.call(rbind, lgbm_trtr_lst))
lgbm_trtr_df <- merge(lgbm_df, lgbm_trtr_df, by = c("data_name"), all.x = T)
results_lgbm <- merge(results_compl_halluc, lgbm_trtr_df, by = c("idx", "data_name", "generator"), all.x = T)

## downstream prediction utility: mlp 
mlp_list <- lapply(data_name_lst, function(data_name){
  files <- list.files(path = paste0(path_mlp, "/", data_name, "/"), pattern = "downstream-mlp-results.rds$")
  mlp_lst <- lapply(files, function(file){
    syn_mlp_avg <- readRDS(paste0(path_mlp, "/", data_name, "/", file))
    generator <- sub("_downstream-mlp-results.rds$", "", sub(paste0("^", data_name, "_\\d+_"), "", file))
    idx <- sub(paste0("^", data_name, "_(\\d+)_", generator, "_.*$"), "\\1", file)
    results <- c(idx = idx, data_name = data_name, generator = generator, syn_mlp_avg = syn_mlp_avg)
    return(results)
  })
  mlp_df <- do.call(rbind, mlp_lst)
  return(mlp_df)
})
mlp_df <- as.data.frame(do.call(rbind, mlp_list))

mlp_trtr_lst <- lapply(data_name_lst, function(data_name){
  file <- list.files(path = paste0(path_mlp, "/", data_name, "/"), pattern = "downstream-mlp-trtr-results.rds$")
  trtr_mlp <- readRDS(paste0(path_mlp, "/", data_name, "/", file))[[1]]
  results <- c(data_name = data_name, train_mlp = trtr_mlp)
  return(results)
})
mlp_trtr_df <- as.data.frame(do.call(rbind, mlp_trtr_lst))
mlp_trtr_df <- merge(mlp_df, mlp_trtr_df, by = c("data_name"), all.x = T)
results_mlp <- merge(results_lgbm, mlp_trtr_df, by = c("idx", "data_name", "generator"), all.x = T)

## specify datatypes
results_halluc <- results_mlp
results_halluc[] <- lapply(colnames(results_halluc), function(var){
  if (var == "data_name" | var == "generator"){
    col <- results_halluc[,var]
  } else {
    col <- as.numeric(results_halluc[,var])
  }
  return(col)
})
saveRDS(results_halluc, paste0(path_results, "halluc_results.rds"))