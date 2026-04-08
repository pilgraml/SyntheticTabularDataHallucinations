# this script is for effect estimation via mixed models

# required libraries  
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(ggeffects)

# define parameter and read in simulation results
gen_lst <- c("synthcity_ctgan", "synthcity_bayesian_network", "replica_seq", 
             "synthcity_tvae", "synthcity_rtvae", "synthcity_nflow", "synthcity_arf")
data_name_lst <- c("born", "california", "cchs", "covid", "faers", "florida", "mimic", "newyork", "nexoid", "texas", "washington", "washington2008")
path_output <- "PATH-TO-RESULTS"
results <- readRDS(paste0(path_output, "halluc_results.rds"))

# mixed effect modeling per generator
model_gen_lst <- lapply(gen_lst, function(gen){
  results_gen <- results[results$generator == gen, ]
  
  # outcome = HR (treated as counts), fixed effect = complexity, random effect = dataset
  results_gen$halluc <- round(results_gen$hr_avg*10000)
  results_gen$non_halluc <- round(10000 - results_gen$halluc)
  model_hr <- lme4::glmer(cbind(halluc, non_halluc) ~ compl_vc_log + (1 + compl_vc_log | data_name), data = results_gen, 
                          family = binomial(link="logit"), control = glmerControl(optimizer = "Nelder_Mead")) # optimizer adjusted as otherwise no model in TVAE
  saveRDS(model_hr, paste0(path_output, gen, "_hr_compl_model.rds"))
  
  # for TSTR: modeling only real-world data combination with sufficient spread of HR across variants
  # identify valid synthetic variants where HR is sufficiently spread (for TSTR modeling) 
  tstr_valid_gen <- results_gen %>%
    group_by(data_name, generator) %>%
    filter(quantile(hr_avg, probs = 0.9, na.rm = T) - quantile(hr_avg, probs = 0.1, na.rm = T) >= 0.25) %>%
    ungroup()
  
  # some SDG models may only have one real-world data: no mixed effect model required
  if(length(unique(tstr_valid_gen$data_name)) > 1){
    
    # outcome = TSTR(lgbm), fixed effect = HR, random effect = dataset
    model_lgbm <- lmerTest::lmer(syn_lgbm_avg ~ hr_avg + (1 + hr_avg | data_name), data = tstr_valid_gen)
    
    # outcome = TSTR(mlp), fixed effect = HR, random effect = dataset
    model_mlp <- lmerTest::lmer(syn_mlp_avg ~ hr_avg + (1 + hr_avg | data_name), data = tstr_valid_gen)
  } else {
    
    # outcome = TSTR(lgbm), predictor = HR
    model_lgbm <- lm(syn_lgbm_avg ~ hr_avg, data = tstr_valid_gen)
    
    # outcome = TSTR(mlp), predictor = HR
    model_mlp <- lm(syn_mlp_avg ~ hr_avg, data = tstr_valid_gen)
  }
  saveRDS(model_lgbm, paste0(path_output, gen, "_lgbm_hr_model.rds"))
  saveRDS(model_mlp, paste0(path_output, gen, "_mlp_hr_model.rds"))
})

# model parameters
parameter_gen_lst <- lapply(gen_lst, function(gen){
  results_gen <- results[results$generator == gen, ]
  
  # outcome = HR, predictor = complexity
  model_hr <- readRDS(paste0(path_output, gen, "_hr_compl_model.rds"))
  
  # saving parameter estimates and variance for fixed and random effect
  hr_ci <- tryCatch({
    
    # preferred CI calculation via likelihood profile
    hr_ci <- as.data.frame(confint(model_hr, oldNames = F))
    return(hr_ci)
  }, error = function(e){
    
    # alternative CI calculation via Wald
    hr_ci <- as.data.frame(confint(model_hr, oldNames = F, method = "Wald"))
    return(hr_ci)
  })
  hr_parameters <- data.frame(
    outcome = rep("hr",5), 
    generator = rep(gen, 5),
    parameter = c("re_intercept", "re_slope", "re_intercept_slope", "fe_intercept", "fe_slope"),
    estimate = c(NA, NA, NA, exp(summary(model_hr)$coefficients[1,1]), exp(summary(model_hr)$coefficients[2,1])),
    CI_low = exp(hr_ci$`2.5 %`),
    CI_up = exp(hr_ci$`97.5 %`), 
    p_value = c(NA, NA, NA, summary(model_hr)$coefficients[1,4], summary(model_hr)$coefficients[2,4]) 
  )
  saveRDS(hr_parameters, paste0(path_output, gen, "_compl_hr_parameters.rds"))
  
  # saving parameter estimates for fixed effect for each real world dataset
  hr_parameters_re <- exp(ranef(model_hr)$data_name)
  saveRDS(hr_parameters_re, paste0(path_output, gen, "_compl_hr_parameters_re.rds"))
  
  # outcome = TSTR(lgbm), predictor = HR
  model_lgbm <- readRDS(paste0(path_output, gen, "_lgbm_hr_model.rds"))
  
  # saving parameter estimates and variance for fixed and random effect
  lgbm_ci <- tryCatch({
    lgbm_ci <- as.data.frame(confint(model_lgbm, oldNames = F))
    return(lgbm_ci)
  }, error = function(e){
    lgbm_ci <- as.data.frame(confint(model_lgbm, oldNames = F, method = "Wald"))
    return(lgbm_ci)
  })
  
  # model summaries differ between mixed effect models and "vanilla" models
  if (length(summary(model_lgbm)$coefficients) == 10){
    lgbm_parameters <- data.frame(
      outcome = rep("lgbm",6), 
      generator = rep(gen, 6),
      parameter = c("re_intercept", "re_slope", "re_intercept_slope", "var_residual", "fe_intercept", "fe_slope"),
      estimate = c(NA, NA, NA, NA, summary(model_lgbm)$coefficients[1,1], summary(model_lgbm)$coefficients[2,1]),
      CI_low = lgbm_ci$`2.5 %`,
      CI_up = lgbm_ci$`97.5 %`, 
      p_value = c(NA, NA, NA, NA, summary(model_lgbm)$coefficients[1,5], summary(model_lgbm)$coefficients[2,5]) 
    )
    
    # saving parameters estimates and variance for fixed effect for each dataset
    lgbm_parameters_re <- ranef(model_lgbm)$data_name
    saveRDS(lgbm_parameters_re, paste0(path_output, gen, "_hr_lgbm_parameters_re.rds"))
    
  }else{
  lgbm_parameters <- data.frame(
    outcome = rep("lgbm",6), 
    generator = rep(gen, 6),
    parameter = c("re_intercept", "re_slope", "re_intercept_slope", "var_residual", "fe_intercept", "fe_slope"),
    estimate = c(NA, NA, NA, NA, summary(model_lgbm)$coefficients[1,1], summary(model_lgbm)$coefficients[2,1]),
    CI_low = c(NA, NA, NA, NA, lgbm_ci$`2.5 %`),
    CI_up = c(NA, NA, NA, NA, lgbm_ci$`97.5 %`), 
    p_value = c(NA, NA, NA, NA, summary(model_lgbm)$coefficients[1,4], summary(model_lgbm)$coefficients[2,4]) 
  )}
  saveRDS(lgbm_parameters, paste0(path_output, gen, "_hr_lgbm_parameters.rds"))
  
  # outcome = TSTR(mlp), predictor = HR
  model_mlp <- readRDS(paste0(path_output, gen, "_mlp_hr_model.rds"))
  # saving parameter estimates and variance for fixed and random effect
  mlp_ci <- tryCatch({
    mlp_ci <- as.data.frame(confint(model_mlp, oldNames = F))
    return(mlp_ci)
  }, error = function(e){
    mlp_ci <- as.data.frame(confint(model_mlp, oldNames = F, method = "Wald"))
    return(mlp_ci)
  })
  # model summaries differ between mixed effect models and "vanilla" models
  if (length(summary(model_mlp)$coefficients) == 10){
    mlp_parameters <- data.frame(
      outcome = rep("mlp",6), 
      generator = rep(gen, 6),
      parameter = c("re_intercept", "re_slope", "re_intercept_slope", "var_residual", "fe_intercept", "fe_slope"),
      estimate = c(NA, NA, NA, NA, summary(model_mlp)$coefficients[1,1], summary(model_mlp)$coefficients[2,1]),
      CI_low = mlp_ci$`2.5 %`,
      CI_up = mlp_ci$`97.5 %`, 
      p_value = c(NA, NA, NA, NA, summary(model_mlp)$coefficients[1,5], summary(model_mlp)$coefficients[2,5]) 
    )
    
    # saving parameters estimates and variance for fixed effect for each dataset
    mlp_parameters_re <- ranef(model_mlp)$data_name
    saveRDS(mlp_parameters_re, paste0(path_output, gen, "_hr_mlp_parameters_re.rds"))
    
  }else{
    mlp_parameters <- data.frame(
      outcome = rep("mlp",6), 
      generator = rep(gen, 6),
      parameter = c("re_intercept", "re_slope", "re_intercept_slope", "var_residual", "fe_intercept", "fe_slope"),
      estimate = c(NA, NA, NA, NA, summary(model_mlp)$coefficients[1,1], summary(model_mlp)$coefficients[2,1]),
      CI_low = c(NA, NA, NA, NA, mlp_ci$`2.5 %`),
      CI_up = c(NA, NA, NA, NA, mlp_ci$`97.5 %`), 
      p_value = c(NA, NA, NA, NA, summary(model_mlp)$coefficients[1,4], summary(model_mlp)$coefficients[2,4]) 
    )}
  saveRDS(mlp_parameters, paste0(path_output, gen, "_hr_mlp_parameters.rds"))
})

# model plotting
plot_gen_lst <- lapply(gen_lst, function(gen){
  
  results_gen <- results[results$generator == gen, ]
  
  # outcome = HR, predictor = complexity
  model_hr <- readRDS(paste0(path_output, gen, "_hr_compl_model.rds"))
  
  # plotting model prediction against true values
  model_hr_preds <- ggeffects::ggpredict(model_hr, terms = c("compl_vc_log", "data_name"), type = "random") 
  colnames(model_hr_preds) <- c("compl_vc_log", "hr_pred", "std.error", "conf.low", "conf.high", "data_name")
  model_hr_plot <- ggplot(results_gen) +
    geom_line(data = model_hr_preds, aes(x = compl_vc_log, y = hr_pred), linewidth = 0.5, color = "#3D7D6B") +
    geom_point(data = results_gen, aes(x = compl_vc_log, y = hr_avg), size = 0.2, color = "#66C2A5") +
    labs(x = "Complexity", y = "HR [%]", title = gen, color = "") + 
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 450)) +
    theme_minimal()+
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12)) +
    facet_wrap(~ data_name)
  ggsave(paste0(path_output, "a_", gen, "_hr_compl_model.pdf"), plot = model_hr_plot, dpi = 300)
  
  # TSTR
  # identify valid synthetic variants where HR is sufficiently spread (for TSTR modeling) 
  tstr_valid_gen <- results_gen %>%
    group_by(data_name, generator) %>%
    filter(quantile(hr_avg, probs = 0.9, na.rm = T) - quantile(hr_avg, probs = 0.1, na.rm = T) >= 0.25) %>%
    ungroup()
  
  # outcome = TSTR(lgbm), predictor = HR
  model_lgbm <- readRDS(paste0(path_output, gen, "_lgbm_hr_model.rds"))
  lgbm_valid_gen_train <- tstr_valid_gen %>%
    group_by(data_name) %>%
    summarise(train_lgbm_avg = mean(train_lgbm, na.rm = TRUE))
  
  # model predictions differ between mixed effect models and "vanilla" models
  if (length(unique(tstr_valid_gen$data_name)) > 1){
    model_lgbm_preds <- ggeffects::ggpredict(model_lgbm, terms = c("hr_avg", "data_name"), type = "random") 
    colnames(model_lgbm_preds) <- c("hr_avg", "lgbm_pred", "std.error", "conf.low", "conf.high", "data_name")
  
    # plotting model prediction against true values
    model_lgbm_plot <- ggplot(tstr_valid_gen) +
      geom_line(data = model_lgbm_preds, aes(x = hr_avg, y = lgbm_pred), linewidth = 0.5, color = "#3D7D6B") +
      geom_point(data = tstr_valid_gen, aes(x = hr_avg, y = syn_lgbm_avg), size = 0.2, color = "#66C2A5") +
      geom_hline(data = lgbm_valid_gen_train, aes(yintercept = train_lgbm_avg), linetype = "dashed", color = "black", linewidth = 0.2) +
      labs(x = "HR [%]", y = "AUC (lgbm)", title = gen, color = "") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, 1)) +
      theme_minimal()+
      theme(
        legend.position = "right",
        legend.text = element_text(size = 12)) +
      facet_wrap(~ data_name)
  }else{
    model_lgbm_preds <- ggeffects::ggpredict(model_lgbm, terms = c("hr_avg")) 
    colnames(model_lgbm_preds) <- c("hr_avg", "lgbm_pred", "std.error", "conf.low", "conf.high", "data_name")
    
    # plotting model prediction against true values
    model_lgbm_plot <- ggplot(tstr_valid_gen) +
      geom_line(data = model_lgbm_preds, aes(x = hr_avg, y = lgbm_pred), linewidth = 0.5, color = "#3D7D6B") +
      geom_point(data = tstr_valid_gen, aes(x = hr_avg, y = syn_lgbm_avg), size = 0.2, color = "#66C2A5") +
      geom_hline(data = lgbm_valid_gen_train, aes(yintercept = train_lgbm_avg), linetype = "dashed", color = "black", linewidth = 0.2) +
      labs(x = "HR [%]", y = "AUC (lgbm)", title = gen, color = "") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, 1)) +
      theme_minimal()+
      theme(
        legend.position = "right",
        legend.text = element_text(size = 12)) 
  }
  ggsave(paste0(path_output, "a_", gen, "_lgbm_hr_model.pdf"), plot = model_lgbm_plot, dpi = 300)
  
  # outcome = TSTR(mlp), predictor = HR
  model_mlp <- readRDS(paste0(path_output, gen, "_mlp_hr_model.rds"))
  mlp_valid_gen_train <- tstr_valid_gen %>%
    group_by(data_name) %>%
    summarise(train_mlp_avg = mean(train_mlp, na.rm = TRUE))
  
  # model predictions differ between mixed effect models and "vanilla" models
  if (length(unique(tstr_valid_gen$data_name)) > 1){
    model_mlp_preds <- ggeffects::ggpredict(model_mlp, terms = c("hr_avg", "data_name"), type = "random") 
    colnames(model_mlp_preds) <- c("hr_avg", "mlp_pred", "std.error", "conf.low", "conf.high", "data_name")
    
    # plotting model prediction against true values
    model_mlp_plot <- ggplot(tstr_valid_gen) +
      geom_line(data = model_mlp_preds, aes(x = hr_avg, y = mlp_pred), linewidth = 0.5, color = "#3D7D6B") +
      geom_point(data = tstr_valid_gen, aes(x = hr_avg, y = syn_mlp_avg), size = 0.2, color = "#66C2A5") +
      geom_hline(data = mlp_valid_gen_train, aes(yintercept = train_mlp_avg), linetype = "dashed", color = "black", linewidth = 0.2) +
      labs(x = "HR [%]", y = "AUC (mlp)", title = gen, color = "") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, 1)) +
      theme_minimal()+
      theme(
        legend.position = "right",
        legend.text = element_text(size = 12)) +
      facet_wrap(~ data_name)
  }else{
    model_mlp_preds <- ggeffects::ggpredict(model_mlp, terms = c("hr_avg")) 
    colnames(model_mlp_preds) <- c("hr_avg", "mlp_pred", "std.error", "conf.low", "conf.high", "data_name")
    
    # plotting model prediction against true values
    model_mlp_plot <- ggplot(tstr_valid_gen) +
      geom_line(data = model_mlp_preds, aes(x = hr_avg, y = mlp_pred), linewidth = 0.5, color = "#3D7D6B") +
      geom_point(data = tstr_valid_gen, aes(x = hr_avg, y = syn_mlp_avg), size = 0.2, color = "#66C2A5") +
      geom_hline(data = mlp_valid_gen_train, aes(yintercept = train_mlp_avg), linetype = "dashed", color = "black", linewidth = 0.2) +
      labs(x = "HR [%]", y = "AUC (mlp)", title = gen, color = "") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_x_continuous(limits = c(0, 1)) +
      theme_minimal()+
      theme(
        legend.position = "right",
        legend.text = element_text(size = 12)) 
  }
  ggsave(paste0(path_output, "a_", gen, "_mlp_hr_model.pdf"), plot = model_mlp_plot, dpi = 300)
})