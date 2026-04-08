# this script is for downstream task MLP
data_name_lst <- c("born", "california", "cchs", "covid", "faers", "florida", "mimic", "newyork", "nexoid", "texas", "washington", "washington2008")
data_name <- "texas"

## calculation of downstream utility
### pre-processing
born.preproc <- function(data){
  library(dplyr)
  born_data <- subset(data, pregnancy_outcome == "Live" | pregnancy_outcome == "Stillbirth")
  born_data$birth_weight_cat1 <- ifelse(born_data$birth_weight_cat == "<2500g", 1,
                                        ifelse(born_data$birth_weight_cat == "2500-3999g", 0, 
                                               ifelse(born_data$birth_weight_cat == "4000g+", 0, NA)))
  born_data1 <- born_data[which(as.character(born_data$birth_outcome) == as.character(born_data$pregnancy_outcome)),]
  born_data2 <- born_data1[ , !names(born_data1) %in% c("pregnancy_outcome")]
  vars_select <- c("birth_weight_cat1", 
                   "ga_cat", 
                   "MatAgeAtBirthYears_cat",  
                   "mat_bmi_cat", 
                   "parity_cat", 
                   "num_of_prev_pretbirths", 
                   "num_of_prev_abortions",
                   "mat_smoking", 
                   "alc_expos", 
                   "prenatal_screening", 
                   "mental_health_Addiction", 
                   "mental_health_Anxiety", 
                   "mental_health_Depression",
                   "maternal_health_Diabetes", 
                   "maternal_health_Genetics", 
                   "drug_expos_Cocaine", 
                   "drug_expos_Hallucinogens", 
                   "drug_expos_Opioids")
  born_data3 <- subset(born_data2, select = vars_select)
  born_data4 <- born_data3 %>% tidyr::drop_na(all_of("birth_weight_cat1"))
  return(born_data4)
}

bsa.preproc <- function(data){
  library(dplyr)
  data <- data %>% tidyr::drop_na(all_of("IP_CLM_DAYS_CD"))
  data$status <- ifelse(data$IP_CLM_DAYS_CD == "1", 0,
                        ifelse(data$IP_CLM_DAYS_CD == "2", 1, 
                               ifelse(data$IP_CLM_DAYS_CD == "3", 1, 
                                      ifelse(data$IP_CLM_DAYS_CD == "4", 1, NA))))
  vars_select <- c("status",  # Length of stay (binary)
                   "BENE_AGE_CAT_CD",  # Age (categorical)
                   "BENE_SEX_IDENT_CD",  # Gender (binary)
                   "IP_CLM_BASE_DRG_CD",  # DRG (categorical)
                   "IP_CLM_ICD9_PRCDR_CD",  # ICD-9-PC (categorical)
                   "IP_DRG_QUINT_PMT_CD")  # Payment (categorical)
  
  bsa_subset <- subset(data, select = vars_select)
  return(bsa_subset)
}

california.preproc <- function(data){
  library(dplyr)
  california <- data %>% tidyr::drop_na(all_of("LOS"))
  california$status <- ifelse(california$LOS >= 3, 1, 0)
  col_select <- c("status",    # Length of stay (binary)
                  "AGE",       # Age (numeric)
                  "FEMALE",    # Gender (binary)
                  "RACE",      # Race (categorical)
                  "AWEEKEND",  # Weekend admission (binary)
                  "DRG",       # DRG (categorical)
                  "DX1",       # ICD-9 (categorical)
                  "CHRON1",    # Chronic condition (binary)
                  "CHRONB1",   # Body system (categorical)
                  "PCLASS1",   # Procedure class (categorical)
                  "CM_ALCOH",  # Alcohol abuse (binary)
                  "CM_DEPRE",  # Depression (binary)
                  "CM_HTN_C",  # Hypertension (binary)
                  "CM_OBESE",  # Obesity (binary)
                  "PAY1")      # Primary payer (categorical)
  data_subset <- subset(california, select = col_select)
  return (data_subset)
}

cchs.preproc <- function(data){
  library(dplyr)
  library(arules)
  data$smoking <- ifelse(data$SMKDSTY==4 | data$SMKDSTY==5 | data$SMKDSTY==6, 1, 
                         ifelse(data$SMKDSTY==1 | data$SMKDSTY==2 | data$SMKDSTY==3, 0, NA))
  data$bmi <- data$HWTGBMI
  data$obesity <- ifelse(is.na(data$bmi), NA, ifelse(data$bmi<25, 1, 0))
  data$hypertension <- ifelse(data$CCC_071==1, 0, ifelse(data$CCC_071==2, 1, NA))
  data$diabetes <- ifelse(data$CCC_101==1, 0, ifelse(data$CCC_101==2, 1, NA))
  data$physicalactivity <- ifelse(is.na(data$PACDEE), NA, ifelse(data$PACDEE>=1.5, 1, 0))
  data$fveg <- ifelse(is.na(data$FVCDTOT), NA, ifelse(data$FVCDTOT>=5, 1, 0))
  data$CANHEARTindex <- as.integer(data$smoking) + as.integer(data$obesity) + as.integer(data$hypertension) + 
    as.integer(data$diabetes) + as.integer(data$physicalactivity) + as.integer(data$fveg)
  data$CANHEARTbin <- ifelse(data$CANHEARTindex>=3, 1, 0)
  data <- data %>% tidyr::drop_na(all_of("CANHEARTindex"))
  
  # undo the continuous handling of age in the generator ST by re-imposing the defined categories
  if(length(unique(data$DHHGAGE_cont)) > 17){
    data$DHHGAGE_cont <- as.numeric(as.character(data$DHHGAGE_cont))
    data$DHHGAGE_cont <- ifelse(data$DHHGAGE_cont < 14.5, 13, 
                                ifelse(data$DHHGAGE_cont >= 14.5 & data$DHHGAGE_cont < 16.5, 16,
                                       ifelse(data$DHHGAGE_cont >= 16.5 & data$DHHGAGE_cont < 17.75, 17,
                                              ifelse(data$DHHGAGE_cont >= 17.75 & data$DHHGAGE_cont < 20.25, 18.5,
                                                     ifelse(data$DHHGAGE_cont >= 20.25 & data$DHHGAGE_cont < 24.5, 22,
                                                            ifelse(data$DHHGAGE_cont >= 24.5 & data$DHHGAGE_cont < 29.5, 27,
                                                                   ifelse(data$DHHGAGE_cont >= 29.5 & data$DHHGAGE_cont < 34.5, 32,
                                                                          ifelse(data$DHHGAGE_cont >= 32.5 & data$DHHGAGE_cont < 39.5, 37,
                                                                                 ifelse(data$DHHGAGE_cont >= 39.5 & data$DHHGAGE_cont < 44.5, 42,
                                                                                        ifelse(data$DHHGAGE_cont >= 44.5 & data$DHHGAGE_cont < 49.5, 47,
                                                                                               ifelse(data$DHHGAGE_cont >= 49.5 & data$DHHGAGE_cont < 54.5, 52,
                                                                                                      ifelse(data$DHHGAGE_cont >= 54.5 & data$DHHGAGE_cont < 59.5, 57,
                                                                                                             ifelse(data$DHHGAGE_cont >= 59.5 & data$DHHGAGE_cont < 64.5, 62,
                                                                                                                    ifelse(data$DHHGAGE_cont >= 64.5 & data$DHHGAGE_cont < 69.5, 67,
                                                                                                                           ifelse(data$DHHGAGE_cont >= 69.5 & data$DHHGAGE_cont < 74.5, 72,
                                                                                                                                  ifelse(data$DHHGAGE_cont >= 74.5 & data$DHHGAGE_cont < 79.5, 77,
                                                                                                                                         ifelse(data$DHHGAGE_cont >= 79.5, 85, NA)))))))))))))))))
    data$DHHGAGE_cont <- as.factor(data$DHHGAGE_cont)
  } 
  
  col_select <- c("CANHEARTbin",  # CANHEART Index (binary)
                  "DHHGAGE_cont",  # Age (categorical)
                  "DHH_SEX",       # Gender (binary)
                  "EDUDR04",       # Education (categorical)
                  "DHHGMS",        # Marital status (categorical)
                  "INCGHH_cont",   # Household income (categorical)
                  "DHHGHSZ",       # Household size (categorical)
                  "SDCFIMM")       # Immigration status (binary)
  data_subset <- subset(data, select = col_select)
  return(data_subset)
}


covid.preproc <- function(data){
  library(dplyr)
  data$status <- ifelse(data$case_status == "Deceased",  1, 
                        ifelse(data$case_status == "Recovered", 0, NA))
  covid <- data %>% tidyr::drop_na(all_of("status"))
  covid$date_reported1 <- as.numeric(as.Date("2021/12/31") - as.Date(covid$date_reported))
  col_select <- c("status",            # Deceased (binary)
                  "age_group",         # Age (categorical)
                  "gender",            # Gender (binary)
                  "date_reported1",    # Date (datetime converted to numeric)
                  "province_abbr",     # Province (categorical)
                  "exposure")          # Exposure (categorical)
  
  data_subset <- subset(covid, select = col_select)
  return(data_subset)
}

faers.preproc <- function(data){
  library(dplyr)
  data$status <- ifelse(data$outc_cod_0 == "DE", 1, 
                        ifelse(data$outc_cod_0 %in% c("CA", "DS", "HO", "LT", "OT", "RI"), 0, NA))
  faers <- data
  faers$date <- as.Date(sapply(as.character(faers$event_dt), function(x){
    x <- trimws(x)
    if (is.na(x)){
      return(NA)
    } else if (nchar(x) == 4) {
      return(as.Date(paste0(x, "-01-01")))
    } else if (nchar(x) == 6) {
      return(as.Date(paste0(x, "01"), format = "%Y%m%d"))
    } else if (nchar(x) == 8) {
      return(as.Date(x, format = "%Y%m%d"))
    } else {
      return(NA)
    }}))
  faers$days <- as.numeric(faers$date - as.Date("1900-01-01"))
  faers$weight <- ifelse(faers$wt_cod == "LBS", faers$wt*0.45359237, faers$wt)
  faers$age_yr <- ifelse(faers$age_cod == "DEC", faers$age * 10, 
                         ifelse(faers$age_cod == "DY", faers$age / 365, 
                                ifelse(faers$age_cod == "HR", faers$age / (24*365),
                                       ifelse(faers$age_cod == "MON", faers$age / 12, 
                                              ifelse(faers$age_cod == "WK", faers$age / 52, faers$age)))))
  faers <- faers %>% tidyr::drop_na(all_of("status"))
  col_select <- c("status",         # Death (binary)
                  "age_yr",         # Age (numeric)
                  "sex",            # Gender (categorical)
                  "days",           # Date (datetime converted to days)
                  "weight",         # Weight (numeric)
                  "drugname_0",     # Drug (categorical)
                  "indi_pt_0")      # Indication (categorical)
  data_subset <- subset(faers, select = col_select)
  return(data_subset)
}

florida.preproc <- function(data){
  library(dplyr)
  florida <- data %>% tidyr::drop_na(all_of("LOS"))
  florida$status <- ifelse(florida$LOS >= 3, 1, 0)
  col_select <- c("status",           # Length of stay (binary)
                  "AGE",              # Age (numeric)
                  "FEMALE",           # Gender (binary)
                  "RACE",             # Race (categorical)
                  "ZIP",              # ZIP (categorical)
                  "ATYPE",            # Admission type (categorical)
                  "AWEEKEND",         # Weekend admission (binary)
                  "DRG",              # DRG (categorical)
                  "DX1",              # ICD-9 (categorical)
                  "PAY1")             # Primary payer (categorical)
  data_subset <- subset(florida, select = col_select)
  return(data_subset)
}

mimic.preproc <- function(data){
  library(dplyr)
  data$READMISSION <- ifelse(data$READMISSION == "0", 0, 1)
  col_select <- c("READMISSION",      # Readmission (binary)
                  "FIRST_ADMIT_AGE",  # Age (numeric)
                  "ETHNICITY",        # Ethnicity (categorical)
                  "ADMISSION_TYPE",   # Admission type (categorical)
                  "HEART_RATE",       # Heart rate (numeric)
                  "SYSBP",
                  "DIASBP", 
                  "RESP_RATE",
                  "NTPROBNP",     # NT-proBNP (numeric)
                  "CREATININE",   # Creatinine (numeric)
                  "BUN",          # BUN (numeric)
                  "POTASSIUM",    # Potassium (numeric)
                  "CHOLESTEROLE") # Cholesterol (numeric)
  data_subset <- subset(data, select = col_select)
  return(data_subset)
}

newyork.preproc <- function(data){
  library(dplyr)
  newyork <- data %>% tidyr::drop_na(all_of("LOS"))
  newyork$status <- ifelse(newyork$LOS >= 3, 1, 0)
  col_select <- c("status", "AGE", "FEMALE", "RACE", "ZIP", "ATYPE", "AWEEKEND", "DRG", 
                  "DX1", "CHRON1", "CHRONB1", "PCLASS1", "PAY1")
  data_subset <- subset(newyork, select = col_select)
  return(data_subset)
}

nexoid.preproc <- function(data){
  library(dplyr)
  nexoid <- data %>% tidyr::drop_na(all_of("risk_infection"))
  nexoid$status <- ifelse(nexoid$risk_infection >= 12.56, 1, 0)
  col_select <- c("status", "age", "sex", "race", "smoking", 
                  "bmi", "house_count", "public_transport_count", "nursing_home", 
                  "covid19_symptoms", "covid19_contact", "health_worker", "asthma", "kidney_disease", 
                  "liver_disease", "heart_disease", "lung_disease", "diabetes", "hypertension")
  
  data_subset <- subset(nexoid, select = col_select)
  return(data_subset)
}

texas.preproc <- function(data){
  library(dplyr)
  texas <- data %>% tidyr::drop_na(all_of("LENGTH_OF_STAY"))
  texas$status <- ifelse(texas$LENGTH_OF_STAY >= 3, 1, 0)
  texas$ethnicity <- as.factor(ifelse(texas$ETHNICITY == "1" | texas$ETHNICITY == "1.0", 1,
                                      ifelse(texas$ETHNICITY == "2" | texas$ETHNICITY == "2.0", 2, NA)))
  col_select <- c("status", "PAT_AGE", "SEX_CODE", "RACE", "ethnicity", "PAT_STATE", "ADMIT_WEEKDAY", 
                  "RISK_MORTALITY", "ILLNESS_SEVERITY", "APR_DRG")
  
  data_subset <- subset(texas, select = col_select)
  return(data_subset)
}

washington.preproc <- function(data){
  library(dplyr)
  washington <- data %>% tidyr::drop_na(all_of("LOS"))
  washington$status <- ifelse(washington$LOS >= 3, 1, 0)
  col_select <- c("status", "AGE", "ZIP", "ATYPE", "AWEEKEND", "DRG", "DX1", "DIED")
  data_subset <- subset(washington, select = col_select)
  return(data_subset)
}

washington2008.preproc <- function(data){
  library(dplyr)
  washington2008 <- data %>% tidyr::drop_na(all_of("LOS"))
  washington2008$status <- ifelse(washington2008$LOS >= 3, 1, 0)
  col_select <- c("status", "AGE", "FEMALE", "RACE", "ZIP", "ATYPE", "AWEEKEND", "DRG", "DX1", "CHRON1", 
                  "CHRONB1", "PCLASS1", "CM_ALCOHOL", "CM_DEPRESS", "CM_HTN_C", "CM_OBESE", "PAY1")
  data_subset <- subset(washington2008, select = col_select)
  return(data_subset)
}

## preprocessing for neural network
data.encoding <- function(data, holdout_data, core_vars, outcome_var, data_name){
  library(recipes)
  library(embed)
  library(fastDummies)
  library(dplyr)
  
  # reducing to core_vars
  data_encod <- eval(parse(text = paste0(data_name, ".preproc(data)")))
  holdout_data_encod <- eval(parse(text = paste0(data_name, ".preproc(holdout_data)")))
  
  # retrieve categorical variables
  cat_vars <- colnames(data_encod)[sapply(data_encod, is.factor)]
  
  # define target and one hot encoding variables based on holdout data to ensure uniform treatment
  cat_vars_onehot <- cat_vars[sapply(holdout_data_encod[cat_vars], function(x) length(levels(na.omit(x))) <= 2)]
  cat_vars_target <- cat_vars[sapply(holdout_data_encod[cat_vars], function(x) length(levels(na.omit(x))) > 2)]
  
  # ensure uniform factor level to holdout data
  data_encod[cat_vars] <- lapply(cat_vars, function(cat){
    levels_pop <- levels(holdout_data_encod[, cat])
    data_encod[, cat] <- factor(data_encod[, cat], levels = levels_pop)
    return(data_encod[, cat])
  })
    
  # target encoding: drop variables if target encoding is not possible
  data_encod[cat_vars_target] <- lapply(cat_vars_target, function(var){
    data_encod[,var] <- tryCatch({
      encoder <- recipes::recipe(as.formula(sprintf("%s ~ %s", outcome_var, var)), data=data_encod) %>%
        embed::step_lencode_mixed(all_of(var), outcome=dplyr::vars(all_of(outcome_var))) %>%
        recipes::prep()
      data_encod[,var] <- (encoder %>%
                             recipes::bake(new_data=data_encod) %>%
                             data.frame)[,var]
      return(data_encod[,var])
    }, error = function(e) {
      # when encoding is not possible return constant
      data_encod[,var] <- 0
      return(data_encod[,var])
    })
  })
    
  # one hot encoding, NA handled as category, remove constant variables
  if(length(cat_vars_onehot) > 0){
    data_encod <- fastDummies::dummy_cols(data_encod, 
                                             select_columns = cat_vars_onehot, 
                                             remove_first_dummy = T, ignore_na = F,
                                             remove_selected_columns = T)
  }
    
  # impute missing values with median
  data_encod[] <- lapply(data_encod[], function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    return(x)
  })
  
  # re-scale variables between 0 and 1 to align with one-hot encoding
  data_encod[] <- lapply(data_encod[], function(x){
    x_min <- min(x,na.rm = TRUE)
    x_max <- max(x,na.rm = TRUE)
    if(x_max - x_min != 0){
      x <- (x - x_min)/(x_max - x_min)
    }else{
      x <- x
    }
    return(x)
  })
  
  # return encoded data
  return(data_encod)
}

### calculation: neural network model
downstream.mlp <- function(data_encod, holdout_encod, outcome_var){
  library(dplyr)
  library(keras3)
  library(tensorflow)
  library(pROC)
  
  # put encoded data in input shape
  X_train <- as.matrix(data_encod %>% select(-all_of(outcome_var)))
  y_train <- as.matrix(data_encod[[outcome_var]])
  X_test <- as.matrix(holdout_encod %>% select(-all_of(outcome_var)))
  y_test <- as.numeric(holdout_encod[[outcome_var]])
  
  # define deep learning model for binary classification
  model_mlp <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu", input_shape = c(dim(X_train)[[2]])) %>%
    layer_dropout(0.3) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model_mlp %>% compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
  
  # train model with callback
  perform_dl <- tryCatch({
    model_mlp %>% fit(
    X_train,
    y_train,
    epochs = 50,
    batch_size = 128,
    validation_split = 0.2,
    callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 2, min_delta = 0.01)),
    verbose = 2)
    
    # get AUC
    y_predict_prob <- as.numeric(model_mlp %>% predict(X_test))
    perform_dl <- pROC::auc(pROC::roc(y_test, y_predict_prob))
    
    # remove model 
    rm(model_mlp)
    return(perform_dl)
  }, error = function(e){
    perform_dl <- NA
    return(perform_dl)
  })
  return(tstr_performance = perform_dl)
}

## process function
mlp.idx <- function(data_name){
  
  # retrieving input
  path_main <- paste0("PATH-TO-REAL-DATA", data_name, "/")
  path_synth <- paste0("PATH-TO-SD", data_name, "/", "data_synth/")
  path_results <- paste0("PATH-TO-RESULTS", data_name, "/")
  assign("path_results", path_results, envir = .GlobalEnv)
  files <- list.files(path = path_synth, pattern = "\\_sdg.rds$")
  sdg_no <- length(files)
  rm(files)
  
  # per population variant
  lapply(sdg_no:1, function(idx){
    
    # define input data
    assign("idx", idx, envir = .GlobalEnv)
    sdg_filename <- list.files(path = path_synth, pattern = paste0(data_name, "_", idx, "_v\\d+_sdg.rds$"))
    sdg <- readRDS(paste0(path_synth, sdg_filename))
    assign("sdg", sdg, envir = .GlobalEnv)
    rm(sdg_filename)
    holdout_data <- sdg$real_holdout
    assign("holdout_data", holdout_data, envir = .GlobalEnv)
    training_data <- sdg$real_training
    outcome_var <- readRDS(paste0(path_main, data_name, "_code/", 
                                  data_name, "_model-outcome.rds"))
    assign("outcome_var", outcome_var, envir = .GlobalEnv)
    core_vars <- readRDS(paste0(path_main, data_name, "_code/", 
                                data_name, "_model-vars.rds"))
    assign("core_vars", core_vars, envir = .GlobalEnv)
    holdout_encod <- data.encoding(holdout_data, holdout_data, core_vars, outcome_var, data_name)
    assign("holdout_encod", holdout_encod, envir = .GlobalEnv)
    
    # try all generators: in parallel
    cl_gen <- parallel::makeCluster(7)
    doParallel::registerDoParallel(cl_gen)
    invisible(parallel::clusterExport(cl_gen, varlist = c("idx", "path_results", "sdg", "holdout_data", 
                                                          "holdout_encod", "core_vars", "outcome_var",
                                                          "data_name", "data.encoding", "downstream.mlp", 
                                                          paste0(data_name, ".preproc"))))
    pbapply::pblapply(1:7, function(gen){
      gen_name <- sdg[[gen]]$generator
      syn_lst <- sdg[[gen]]$syn_data_lst
      assign("syn_lst", syn_lst, envir = .GlobalEnv)
      path_output_gen_results <- paste0(path_results, data_name, "_", idx, "_", gen_name,
                                        "_downstream-mlp-results.rds")
      
      # if file is already available, jump to next one
      if(!file.exists(path_output_gen_results)){
        
        # if synthetic data is available calculate dcr
        if (!length(syn_lst) == 0){
          
          # cluster per synthetic dataset parallelized
          cl <- parallel::makeCluster(10)
          doParallel::registerDoParallel(cl)
          invisible(parallel::clusterExport(cl, varlist = c("syn_lst", "holdout_data", "holdout_encod", "core_vars", "outcome_var",
                                                            "data_name", "data.encoding", "downstream.mlp", 
                                                            paste0(data_name, ".preproc"))))
          results <- pbapply::pblapply(1:10, function (i){
            syn_data <- syn_lst[[i]]
            
            # encod data for mlp
            syn_data_encod <- data.encoding(syn_data, holdout_data, core_vars, outcome_var, data_name)
            rm(syn_data)
            
            # run mlp
            syn_dl <- downstream.mlp(syn_data_encod, holdout_encod, outcome_var)
            rm(syn_data_encod)
            
            # return auc
            return(syn_dl = syn_dl)
          }, cl = cl)
          parallel::stopCluster(cl)
          rm(syn_lst)
          
          # average across datasets
          syn_dl_avg <- mean(unlist(results), na.rm = TRUE)
          
          # if there is no synthetic data available return NA  
        }else{
          syn_dl_avg <- NA
        }
        
        # save model and results
        saveRDS(syn_dl_avg, path_output_gen_results)
        rm(list = c("results", "syn_dl_avg"))
      }
    }, cl = cl_gen)
    parallel::stopCluster(cl_gen)
    
    # encod and run model for training data: once for each healthcare dataset
    path_output_trtr_results <- paste0(path_results, data_name,
                                       "_downstream-mlp-trtr-results.rds")
    if(!file.exists(path_output_trtr_results)){
      training_encod <- data.encoding(training_data, holdout_data, core_vars, outcome_var, data_name)
      train_dl <- downstream.mlp(training_encod, holdout_encod, outcome_var)
      saveRDS(train_dl, path_output_trtr_results)
    }
    # empty storage
    rm(list = c("results", "train_dl", "sdg", "holdout_data", "training_data", "holdout_encod", "training_encod"))
    gc()
  })
}


# run utility 
time_utility <- system.time({
  mlp.idx(data_name)
})

saveRDS(time_utility, paste0(path_results, data_name, "_results_timing_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

