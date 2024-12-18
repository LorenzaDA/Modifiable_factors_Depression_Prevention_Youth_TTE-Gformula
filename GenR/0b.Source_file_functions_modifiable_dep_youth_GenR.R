##################################################
# 0B. Source file functions modifiable factors depression youth - GenR
##################################################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT:functions for analyses
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)

################################  Variable preparation  #####################################

# like mean.n function of spss 
# df = df with cols you need for calculating the mean, n= min number of valid values for calculating the mean
mean.n   <- function(df, n) {
  means <- apply(as.matrix(df), 1, mean, na.rm = TRUE)
  nvalid <- apply(as.matrix(df), 1, function(df) sum(!is.na(df)))
  ifelse(nvalid >= n, means, NA)
}


# to sum values allowing for missingness in certain cols 
sum_across_columns <- function(data, vars_to_sum, new_var_name, max_missing_percent) {
  require(dplyr)
  
  total_columns <- ncol(data[, names(data) %in% vars_to_sum])
  max_missing_columns <- round(total_columns * max_missing_percent)
  
  data <- data %>%
    mutate({{ new_var_name }} := rowSums(across({{ vars_to_sum }}, na.rm = TRUE), na.rm = TRUE))
  
  return(data)
}


#############################  Step 1: Observed associations  ###################################

####
# Perform regression across all prevention strategies
####
# this is like the longitudinal reg function but across all datasets 
# data is the list you input with all your datasets for each prevention strategy (universal, selective (x2), indicated)
# baselinevars = covariates you need to include in the model
# pred_vars = your main predictors, i.e., each modifiable factor
# in this way the analysis is performed for each modifiable factor within each prevention strategy

perform_regression <- function(data, baselinevars, pred_vars) {
  # empty dataframe to store results
  results_df <- data.frame(Variable = character(),
                           Coefficient = numeric(),
                           Standard_Error = numeric(),
                           P_Value = numeric(),
                           row.names = NULL)
  
  # loop through each modifiable factor
  for (var in pred_vars) {
    # extract the variable name without the splines::ns() part (relevant for the screen time variable)
    var_cleaned <- gsub("splines::ns\\((.*?),.*", "\\1", var)
    
    # formula for the current predictor variable
    formula_string <- paste("int_t2_centered ~", paste(c(baselinevars), collapse = "+"), "+", var)
    formula <- as.formula(formula_string)
    
    # linear regression
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
      # extract the coefficients, standard errors, and p-values
      result_row <- data.frame(Variable = names(coef_data)[var_index],
                               Coefficient = coef_data[var_index],
                               Standard_Error = std_err_data[var_index],
                               P_Value = p_value_data[var_index])
      
      # add the result row to the dataframe
      results_df <- rbind(results_df, result_row)
    } else {
      # if can't find the variable among the predictors, give a warning
      print(paste("Variable not found in coefficients:", var))
    }
  }
  # show results
  return(results_df)
}



#####
# Regressions for each TTE step (sensitivity analyses)
#####

### cross-sectional regression ###
# function for cross-sectional regression (outcome = int t1) just within one dataset

perform_regression_CS <- function(baselinevars, pred_vars) {
  results_df <- data.frame(Variable = character(),
                           Coefficient = numeric(),
                           Standard_Error = numeric(),
                           P_Value = numeric(),
                           row.names = NULL)
  for (var in pred_vars) {
    var_cleaned <- gsub("splines::ns\\((.*?),.*", "\\1", var)
    
    # NB here the outcome is int T1 (because it's the cross-sectional analysis)
    formula_string <- paste("int_t1_centered ~", paste(c(baselinevars), collapse = "+"), "+", var)
    formula <- as.formula(formula_string)
    
    lm_result <- lm(formula, data = dd)

    coef_data <- coef(lm_result)
    std_err_data <- summary(lm_result)$coef[, "Std. Error"]
    p_value_data <- summary(lm_result)$coef[, "Pr(>|t|)"]
    var_regex <- paste0("\\b", gsub("[-.*+^]", "\\\\\\0", var_cleaned), "\\b")
    var_index <- grep(var_regex, names(coef_data), perl = TRUE, ignore.case = TRUE)
    
    if (length(var_index) > 0) {
      result_row <- data.frame(Variable = names(coef_data)[var_index],
                               Coefficient = coef_data[var_index],
                               Standard_Error = std_err_data[var_index],
                               P_Value = p_value_data[var_index])
      results_df <- rbind(results_df, result_row)
    } else {
      print(paste("Variable not found in coefficients:", var))
    }
  }
  return(results_df)
}



### longitudinal regression ###
# function for longitudinal regressions just within one dataset 

perform_regression_longi <- function(baselinevars, pred_vars) {
  results_df <- data.frame(Variable = character(),
                           Coefficient = numeric(),
                           Standard_Error = numeric(),
                           P_Value = numeric(),
                           row.names = NULL)
    for (var in pred_vars) {
    var_cleaned <- gsub("splines::ns\\((.*?),.*", "\\1", var)
    formula_string <- paste("int_t2_centered ~", paste(c(baselinevars), collapse = "+"), "+", var)
    formula <- as.formula(formula_string)
    lm_result <- lm(formula, data = dd)
    coef_data <- coef(lm_result)
    std_err_data <- summary(lm_result)$coef[, "Std. Error"]
    p_value_data <- summary(lm_result)$coef[, "Pr(>|t|)"]
    var_regex <- paste0("\\b", gsub("[-.*+^]", "\\\\\\0", var_cleaned), "\\b")
    var_index <- grep(var_regex, names(coef_data), perl = TRUE, ignore.case = TRUE)
    if (length(var_index) > 0) {
      result_row <- data.frame(Variable = names(coef_data)[var_index],
                               Coefficient = coef_data[var_index],
                               Standard_Error = std_err_data[var_index],
                               P_Value = p_value_data[var_index])
      
      results_df <- rbind(results_df, result_row)
    } else {
      print(paste("Variable not found in coefficients:", var))
    }
  }
  
  return(results_df)
}




################################  Step 2: Counterfactuals with g-comp ################################

##########
# G formula with hard assignment
##########
# adaptation from https://ehsanx.github.io/TMLEworkshop/g-computation.html

# screen time 
predict_and_contrast_screen_boot <- function(datasets, levels, baselinevars, n_boot = 1000, seed = 123) {
  all_results_combined <- data.frame()
  set.seed(seed)
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    formula_string <- paste("int_t2_centered ~ splines::ns(screen,2) +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    
    factor_levels <- lapply(data[, sapply(data, is.factor)], levels)
    
    bootstrap_results <- matrix(NA, nrow = n_boot, ncol = length(levels) - 1)
    colnames(bootstrap_results) <- paste0("contrast_", levels[levels != 2], "_ref2")
    
    for (b in 1:n_boot) {
      indices <- sample(1:nrow(data), replace = TRUE)
      bootstrap_sample <- data[indices, ]
      for (var in names(factor_levels)) {
        if (var %in% names(bootstrap_sample)) {
          bootstrap_sample[[var]] <- factor(bootstrap_sample[[var]], levels = factor_levels[[var]])
        }
      }

      fit.boot <- lm(out_formula, data = bootstrap_sample)

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
      
      contrasts <- sapply(levels[levels != 2], function(level) {
        mean(pred_list[[as.character(level)]]) - mean(pred_list[[as.character(2)]])
      })
      
      bootstrap_results[b, ] <- contrasts
    }
    
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
  
  combined_results_filename <- paste0(res, "results_gcomputation_screen_genr_withboot_jan2024.csv")
  write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
}




######
# G-formula for natural course scenario
######
# this is the natural course scenario, meaning that the causal contrast
# are created in comparison to the predicted values observed if people
# were to keep their extant behavior (instead of if they were to adhere to intervention)

predict_and_contrast_screen_nat <- function(datasets, levels, baselinevars) {
  all_results_combined <- data.frame()
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]
    
    formula_string <- paste("int_t2_centered ~ splines::ns(screen,2) +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    model <- lm(out_formula, data = data)
    
    data$natural_course_screen <- predict(model, data, 
                                          type = "response")
    
    for (level in levels) {
      pred_col <- paste0("pred_", level, "hr")
      data[[pred_col]] <- predict(model, newdata = data.frame(screen = as.numeric(level), dplyr::select(data, !screen)), type = "response")
    }
    
    contrast_cols <- paste0("contrast_", levels, "_refNat")
    for (level in levels) {
      contrast_col <- paste0("contrast_", level, "_refNat")
      data[[contrast_col]] <- data[[paste0("pred_", level, "hr")]] - data$natural_course_screen
    }
    
    summary_data <- data %>% summarize(across(starts_with("contrast_"), mean, na.rm = TRUE))

    all_results_combined <- rbind(all_results_combined, data.frame(Dataset = names(datasets)[i], summary_data))
  }
  
  combined_results_filename <- paste0(res, "results_gcomputation_naturalcourse_screen_genr_jan2024.csv")
  write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
  
}



######
# G formula - model misspecification check
######
# to check for the potential extent of model misspecification
# this is tested with the predicted values of internalizing under the natural course
# vs the observed values of internalizing

screen_modelmisp <- function(datasets, baselinevars, filepath) {
  all_results_combined <- data.frame()
  
  for (i in seq_along(datasets)) {
    data <- datasets[[i]]

    formula_string <- paste("int_t2_centered ~ splines::ns(screen,2) +", paste(baselinevars, collapse = "+"))
    out_formula <- as.formula(formula_string)
    model <- lm(out_formula, data = data)

    data$natural_course_screen <- predict(model, data, type = "response")
    
    data$contrast <- data$natural_course_screen - data$int_t2_centered
    
    summary_data <- mean(data$contrast, na.rm = TRUE)
    
    all_results_combined <- rbind(all_results_combined, data.frame(Dataset = names(datasets)[i], summary_data))
  }
  
  combined_results_filename <- paste0(filepath, "results_gcomputation_screen_ModelMisp_GenR_jan2024.csv")
  tryCatch({
    write.csv(all_results_combined, file = combined_results_filename, row.names = FALSE)
    cat("Results successfully saved to", combined_results_filename, "\n")
  }, error = function(e) {
    cat("Error in saving file:", e$message, "\n")
  })
}

