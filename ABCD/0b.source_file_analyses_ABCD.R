#######################
# 0B. Source file - functions - ABCD
#######################

##################### Step 1: observed associations #############################

####
# TTE steps (sensitivity analyses) 
####

### cross sectional association #### 
# here the outcome is int T1 (not T2)
perform_regression_CS <- function(baselinevars, pred_vars) {
  # empty dataframe to store results
  results_df <- data.frame(Variable = character(),
                           Coefficient = numeric(),
                           Standard_Error = numeric(),
                           P_Value = numeric(),
                           row.names = NULL)
  
  # loop through each predictor variable (screen time, pa, social support, sleep)
  for (var in pred_vars) {
    # extract the variable name without the splines::ns() part to extract results later 
    var_cleaned <- gsub("splines::ns\\((.*?),.*", "\\1", var)
    
    # regression formula
    formula_string <- paste("int_t1_centered ~", paste(c(baselinevars), collapse = "+"), "+", var)
    formula <- as.formula(formula_string)
    
    # run linear regression
    lm_result <- lm(formula, data = dd)
    
    # extract coefficients, standard errors, and p-values
    coef_data <- coef(lm_result)
    std_err_data <- summary(lm_result)$coef[, "Std. Error"]
    p_value_data <- summary(lm_result)$coef[, "Pr(>|t|)"]
    
    # use regular expression to find the index for the variable of interest
    var_regex <- paste0("\\b", gsub("[-.*+^]", "\\\\\\0", var_cleaned), "\\b")
    var_index <- grep(var_regex, names(coef_data), perl = TRUE, ignore.case = TRUE)
    
    # check if the variable is found in the coefficients
    if (length(var_index) > 0) {
      # extract the coefficients, standard errors, and p-values
      result_row <- data.frame(Variable = names(coef_data)[var_index],
                               Coefficient = coef_data[var_index],
                               Standard_Error = std_err_data[var_index],
                               P_Value = p_value_data[var_index])
      
      # add the result row to the dataframe
      results_df <- rbind(results_df, result_row)
    } else {
      print(paste("Variable not found in coefficients:", var))
    }
  }
  
  return(results_df)
}


#### longitudinal regression ####
perform_regression_longi <- function(baselinevars, pred_vars) {
  # Initialize an empty dataframe to store results
  results_df <- data.frame(Variable = character(),
                           Coefficient = numeric(),
                           Standard_Error = numeric(),
                           P_Value = numeric(),
                           row.names = NULL)

  for (var in pred_vars) {
    var_cleaned <- gsub("splines::ns\\((.*?),.*", "\\1", var)
    
    # create a formula for the current predictor variable
    formula_string <- paste("int_t2_centered ~", paste(c(baselinevars), collapse = "+"), "+", var)
    formula <- as.formula(formula_string)
    
    # run linear regression
    lm_result <- lm(formula, data = dd)
    
    # extract coefficients, standard errors, and p-values
    coef_data <- coef(lm_result)
    std_err_data <- summary(lm_result)$coef[, "Std. Error"]
    p_value_data <- summary(lm_result)$coef[, "Pr(>|t|)"]
    
    # use regular expression to find the index for the variable of interest
    var_regex <- paste0("\\b", gsub("[-.*+^]", "\\\\\\0", var_cleaned), "\\b")
    var_index <- grep(var_regex, names(coef_data), perl = TRUE, ignore.case = TRUE)
    
    # check if the variable is found in the coefficients
    if (length(var_index) > 0) {
      # Extract the coefficients, standard errors, and p-values
      result_row <- data.frame(Variable = names(coef_data)[var_index],
                               Coefficient = coef_data[var_index],
                               Standard_Error = std_err_data[var_index],
                               P_Value = p_value_data[var_index])
      
      # add the result row to the dataframe
      results_df <- rbind(results_df, result_row)
    } else {
      print(paste("Variable not found in coefficients:", var))
    }
  }
  
  return(results_df)
}



##########
# Under TTE conditions, run analyses for all prevention scenarios and all modifiable factors 
##########

perform_regression <- function(data, baselinevars, pred_vars) {
  # create an empty dataframe to store results
  results_df <- data.frame(Variable = character(),
                           Coefficient = numeric(),
                           Standard_Error = numeric(),
                           P_Value = numeric(),
                           row.names = NULL)
  
  # loop through each predictor variable
  for (var in pred_vars) {
    # extract the variable name without the splines::ns() part (applicable in this case for the screen time measure)
    var_cleaned <- gsub("splines::ns\\((.*?),.*", "\\1", var)
    
    # formula
    formula_string <- paste("int_t2_centered ~", paste(c(baselinevars), collapse = "+"), "+", var)
    formula <- as.formula(formula_string)
    
    # run linear regression
    lm_result <- lm(formula, data = data)
    
    # extract coefficients, standard errors, and p-values
    coef_data <- coef(lm_result)
    std_err_data <- summary(lm_result)$coef[, "Std. Error"]
    p_value_data <- summary(lm_result)$coef[, "Pr(>|t|)"]
    
    # use regex to find the index for the variable of interest
    var_regex <- paste0("\\b", gsub("[-.*+^]", "\\\\\\0", var_cleaned), "\\b")
    var_index <- grep(var_regex, names(coef_data), perl = TRUE, ignore.case = TRUE)
    
    # check if the variable is found in the coefficients
    if (length(var_index) > 0) {
      # put the extracted coefficients SE and p values in a results row
      result_row <- data.frame(Variable = names(coef_data)[var_index],
                               Coefficient = coef_data[var_index],
                               Standard_Error = std_err_data[var_index],
                               P_Value = p_value_data[var_index])
      
      # add the result row to the dataframe
      results_df <- rbind(results_df, result_row)
    } else {
      # if not found, give a warning
      print(paste("Variable not found in coefficients:", var))
    }
  }
  # show results
  return(results_df)
}


#################### Step 2: Counterfactuals with g-comp #######################

#####
# G-formula with hard assignment 
#####


#### G-formula for PA #### 
# in the function we specify the datasets, i.e., prevention scenarios (universal, selective (x2), indicated)
# the levels of the interventions we want to test (e.g., 1 hour of ST, 
# 2 hours of ST, 3 hours of ST)
# the baseline variables, i.e., the covariates
# n of bootstrapping 
# and seed for randomization

predict_and_contrast_pa_boot <- function(datasets, levels, baselinevars, n_boot = 1000, seed = 123) {
  all_results_combined <- data.frame()
  set.seed(seed)
  
  # loop through each dataset
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    # create the formula string for the linear model
    formula_string <- paste("int_t2_centered ~ pa +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    
    # store factor levels to ensure consistency
    factor_levels <- lapply(data[, sapply(data, is.factor)], levels)
    
    # prepare storage for bootstrap results
    bootstrap_results <- matrix(NA, nrow = n_boot, ncol = length(levels) - 1)
    colnames(bootstrap_results) <- paste0("contrast_", levels[levels != 7], "_ref7")
    
    for (b in 1:n_boot) {
      # bootstrap sample for fitting the model
      indices <- sample(1:nrow(data), replace = TRUE)
      bootstrap_sample <- data[indices, ]
      
      # ensure factor levels are maintained correctly
      for (var in names(factor_levels)) {
        if (var %in% names(bootstrap_sample)) {
          bootstrap_sample[[var]] <- factor(bootstrap_sample[[var]], levels = factor_levels[[var]])
        }
      }
      
      # fit the model on the bootstrap sample
      fit.boot <- lm(out_formula, data = bootstrap_sample)
      
      # make predictions on the original data
      pred_list <- list()
      for (level in levels) {
        newdata <- data.frame(pa = as.numeric(level), dplyr::select(data, -pa))
        for (var in names(factor_levels)) {
          if (var %in% names(newdata)) {
            newdata[[var]] <- factor(newdata[[var]], levels = factor_levels[[var]])
          }
        }
        pred_list[[as.character(level)]] <- predict(fit.boot, newdata = newdata, type = "response")
      }
      
      # calculate contrasts between each level and the reference level (e.g., 7 days of PA)
      contrasts <- sapply(levels[levels != 7], function(level) {
        mean(pred_list[[as.character(level)]]) - mean(pred_list[[as.character(7)]])
      })
      
      # store the contrasts for this bootstrap iteration
      bootstrap_results[b, ] <- contrasts
    }
    
    # calculate summary statistics for each contrast
    summary_data <- data.frame(Dataset = names(datasets)[i])
    for (j in seq_along(levels[levels != 7])) {
      contrast_col <- colnames(bootstrap_results)[j]
      mean_contrast <- mean(bootstrap_results[, j])
      lower_CI <- quantile(bootstrap_results[, j], probs = 0.025, type = 6)
      upper_CI <- quantile(bootstrap_results[, j], probs = 0.975, type = 6)
      summary_data[[contrast_col]] <- paste0(
        round(mean_contrast, 3), 
        " (", 
        round(lower_CI, 3), 
        ", ", 
        round(upper_CI, 3), 
        ")"
      )
    }
    
    # combine results for all datasets
    all_results_combined <- rbind(all_results_combined, summary_data)
  }
  
  # save combined results to CSV
  combined_results_filename <- paste0(res, "results_gcomputation_pa_abcd_withboot.csv")
  write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
}




### G-formula for screen time ###
predict_and_contrast_screen_boot <- function(datasets, levels, baselinevars, n_boot = 1000, seed = 123) {
  all_results_combined <- data.frame()
  set.seed(seed)
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    formula_string <- paste("int_t2_centered ~ splines::ns(screen,2) +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    factor_levels <- lapply(data[, sapply(data, is.factor)], levels)
    
    # prepare storage for bootstrap results
    bootstrap_results <- matrix(NA, nrow = n_boot, ncol = length(levels) - 1)
    colnames(bootstrap_results) <- paste0("contrast_", levels[levels != 2], "_ref2")
    
    for (b in 1:n_boot) {
      # bootstrap sample for fitting the model
      indices <- sample(1:nrow(data), replace = TRUE)
      bootstrap_sample <- data[indices, ]
      
      # ensure factor levels are maintained correctly
      for (var in names(factor_levels)) {
        if (var %in% names(bootstrap_sample)) {
          bootstrap_sample[[var]] <- factor(bootstrap_sample[[var]], levels = factor_levels[[var]])
        }
      }
      
      # fit the model on the bootstrap sample
      fit.boot <- lm(out_formula, data = bootstrap_sample)
      
      # make predictions on the original data
      pred_list <- list()
      for (level in levels) {
        newdata <- data.frame(screen = as.numeric(level), dplyr::select(data, -screen))
        for (var in names(factor_levels)) {
          if (var %in% names(newdata)) {
            newdata[[var]] <- factor(newdata[[var]], levels = factor_levels[[var]])
          }
        }
        pred_list[[as.character(level)]] <- predict(fit.boot, newdata = newdata, type = "response")
      }
      
      # calculate contrasts between each level and the reference level (e.g., 2)
      contrasts <- sapply(levels[levels != 2], function(level) {
        mean(pred_list[[as.character(level)]]) - mean(pred_list[[as.character(2)]])
      })
      
      # store the contrasts for this bootstrap iteration
      bootstrap_results[b, ] <- contrasts
    }
    
    # calculate summary statistics for each contrast
    summary_data <- data.frame(Dataset = names(datasets)[i])
    for (j in seq_along(levels[levels != 2])) {
      contrast_col <- colnames(bootstrap_results)[j]
      mean_contrast <- mean(bootstrap_results[, j])
      lower_CI <- quantile(bootstrap_results[, j], probs = 0.025, type = 6)
      upper_CI <- quantile(bootstrap_results[, j], probs = 0.975, type = 6)
      summary_data[[contrast_col]] <- paste0(
        round(mean_contrast, 3), 
        " (", 
        round(lower_CI, 3), 
        ", ", 
        round(upper_CI, 3), 
        ")"
      )
    }
    
    all_results_combined <- rbind(all_results_combined, summary_data)
  }
  
  combined_results_filename <- paste0(res, "results_gcomputation_screen_abcd_withboot.csv")
  write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
}


#####
# G-formula for natural course scenario 
#####
# this is the natural course scenario, meaning that the causal contrast
# are created in comparison to the predicted values observed if people
# were to keep their extant behavior (instead of if they were to adhere to intervention)

### for screen time ###
predict_and_contrast_screen_nat <- function(datasets, levels, baselinevars) {
  all_results_combined <- data.frame()
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    # formula
    formula_string <- paste("int_t2_centered ~ splines::ns(screen,2) +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    model <- lm(out_formula, data = data)
    
    data$natural_course_screen <- predict(model, data, 
                                          type = "response")
    
    # contrasts
    for (level in levels) {
      pred_col <- paste0("pred_", level, "hr")
      data[[pred_col]] <- predict(model, newdata = data.frame(screen = as.numeric(level), dplyr::select(data, !screen)), type = "response")
    }
    contrast_cols <- paste0("contrast_", levels, "_refNat")
    for (level in levels) {
      contrast_col <- paste0("contrast_", level, "_refNat")
      data[[contrast_col]] <- data[[paste0("pred_", level, "hr")]] - data$natural_course_screen
    }
    
    # summarize contrasts
    summary_data <- data %>% summarize(across(starts_with("contrast_"), mean, na.rm = TRUE))

    # combine results for all datasets
    all_results_combined <- rbind(all_results_combined, data.frame(Dataset = names(datasets)[i], summary_data))
  }
  
  # save combined results to CSV
  combined_results_filename <- paste0(res, "results_gcomputation_naturalcourse_screen_abcd_jan2024.csv")
  write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)

}


### pa natural course scenario ### 

predict_and_contrast_pa_nat <- function(datasets, levels, baselinevars) {
  all_results_combined <- data.frame()
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    formula_string <- paste("int_t2_centered ~ pa +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    model <- lm(out_formula, data = data)
    
    data$natural_course_pa <- predict(model, data, 
                                      type = "response")

    for (level in levels) {
      pred_col <- paste0("pred_", level, "day")
      data[[pred_col]] <- predict(model, newdata = data.frame(pa = as.numeric(level), dplyr::select(data, !pa)), type = "response")
    }
    
    contrast_cols <- paste0("contrast_", levels, "_NatCourse")
    for (level in levels) {
      contrast_col <- paste0("contrast_", level, "_NatCourse")
      data[[contrast_col]] <- data[[paste0("pred_", level, "day")]] - data$natural_course_pa
    }
    
    summary_data <- data %>% summarize(across(starts_with("contrast_"), mean, na.rm = TRUE))
    
    all_results_combined <- rbind(all_results_combined, data.frame(Dataset = names(datasets)[i], summary_data))
  }
  combined_results_filename <- paste0(res, "results_gcomputation_pa_naturalcourse_abcd_jan2024.csv")
  write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
}



####
# G formula - model misspecification check
####
# to check for the potential extent of model misspecification
# this is tested with the predicted values of internalizing under the natural course
# vs the observed values of internalizing

### screen time ###

screen_modelmisp <- function(datasets, baselinevars, filepath) {
  all_results_combined <- data.frame()
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    # create predictive model
    formula_string <- paste("int_t2_centered ~ splines::ns(screen,2) +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    model <- lm(out_formula, data = data)
    
    # predict for natural course
    data$natural_course_screen <- predict(model, data, type = "response")
    
    # calculate contrast for predicted internalizing under the natural course scenario 
    # vs observed internalizing 
    data$contrast <- data$natural_course_screen - data$int_t2_centered
    
    # summarize contrasts
    summary_data <- mean(data$contrast, na.rm = TRUE)
    
    # combine results for all datasets
    all_results_combined <- rbind(all_results_combined, data.frame(Dataset = names(datasets)[i], summary_data))
  }
  
  # save combined results to csv
  combined_results_filename <- paste0(filepath, "results_gcomputation_screen_ModelMisp_ABCD.csv")
 # message for whether results were successfully saved 
   tryCatch({
    write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
    cat("Results successfully saved to", combined_results_filename, "\n")
  }, error = function(e) {
    cat("Error in saving file:", e$message, "\n")
  })
}


### physical activity ###
pa_modelmisp <- function(datasets, baselinevars, filepath) {
  all_results_combined <- data.frame()
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    # create predictive model
    formula_string <- paste("int_t2_centered ~ pa +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    model <- lm(out_formula, data = data)
    
    # predict for natural course
    data$natural_course_pa <- predict(model, data, type = "response")

    # calculate contrast
    data$contrast <- data$natural_course_pa - data$int_t2_centered
    
    # summarize contrasts
    summary_data <- mean(data$contrast, na.rm = TRUE)
    
    # combine results for all datasets
    all_results_combined <- rbind(all_results_combined, data.frame(Dataset = names(datasets)[i], summary_data))
  }
  
  # save combined results to csv
  combined_results_filename <- paste0(filepath, "results_gcomputation_pa_ModelMisp_ABCD.csv")
  tryCatch({
    write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
    cat("Results successfully saved to", combined_results_filename, "\n")
  }, error = function(e) {
    cat("Error in saving file:", e$message, "\n")
  })
}



