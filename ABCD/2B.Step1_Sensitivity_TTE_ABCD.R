##################
# 2B. Steps leading to implementation of all TTE conditions
##################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT:Sensitivity analysis to observe how associational results change
# when utilizing a cross-sectional design, vs longitudinal, vs under all TTE conditions
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


####
# set environment
####
# NB here we are loading the sample that is not filtered for those with clinical 
# levels of int problems. This is because that is one of the TTE conditions, that is 
# generally not implemented in the field. 

rm(list = ls())
source("/PATH-WHERE-SOURCE-FILE-IS/0a.source_file_packages_paths_ABCD.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.source_file_analyses_ABCD.R")
dd <- readRDS(paste0(indata, "modif_dep_abcd_youth_sensitivity_withClinical_Jan2024.rds"))


### prepare the dataset for the analysis ###
# we need to deal with missing data on covariates and impute using multiple chained equations (mice R package)
# we cannot reuse the other dataset because it has less participants (i.e., does not 
# include those with clinical levels)

predictors_for_imputation <- c("parent_edu", "mat_age", "income", "par_psych", "sex", "puberty", "site")
impvars <- c("ethn", predictors_for_imputation)  

dd$id <- factor(dd$id)
ini <- mice(dd, maxit = 0, printF = FALSE)

pred <- ini$pred
pred[] <- 0

pred[rownames(pred) %in% impvars, colnames(pred) %in% impvars] <- 1

diag(pred) <- 0

meth <- ini$meth

meth[!names(meth) %in% impvars] <- "" 

visit <- ini$visit
visit <- visit[visit %in% impvars]
visit2 <- c("parent_edu", "par_psych", "puberty", 'ethn', "mat_age", "income")

imp <- mice::mice(dd, 
                  m = 30, 
                  maxit = 30, 
                  seed = 2023, 
                  pred = pred, 
                  meth = meth, 
                  visit = visit2)

# save dataset 
saveRDS(imp, paste0(indata, "modif_dep_abcd_youth_sensitivity_withClinical_Jan2024_IMPUTED.rds"))

# merge imputed dataset and original dataset
dd <- readRDS(paste0(indata, "modif_dep_abcd_youth_sensitivity_withClinical_Jan2024_IMPUTED.rds"))
original_df <- readRDS(paste0(indata, "modif_dep_abcd_youth_sensitivity_withClinical_Jan2024.rds"))

dd2 <- merge_imputations(
  original_df,
  dd,
  ori = original_df
)


# select only relevant variables from the newly merged dataset
# rename variables 
dd3 <- dd2 %>% select(id, int_t1, int_tscore_t1, int_t2, 
                      parent_edu_imp, 
                      site, sleep, pa, par_psych_imp, age, 
                      sex, puberty_imp, friends,
                      screen, prs, trauma, iq, mat_age_imp, income_imp,
                      ethn_imp
) %>% rename(
  parent_edu = parent_edu_imp, 
  par_psych = par_psych_imp, 
  puberty = puberty_imp,
  income = income_imp,
  mat_age = mat_age_imp,
  ethn = ethn_imp
)


# recode factors that warrant recoding for positivity 
dd <- dd3 %>% mutate(puberty = recode_factor(puberty, 
                                                           "pre-puberty" = "pre&early", 
                                                           "early-puberty" = "pre&early",
                                                           "mid-puberty" = "mid&late", 
                                                           "late-post-puberty" = "mid&late"), 
                                   sleep = recode_factor(sleep, 
                                                         "less_7_hrs" = "less_8hrs", 
                                                         "7_8_hrs" = "less_8hrs", 
                                                         "8_9_hrs" = "8_9_hrs", 
                                                         "9_11_hrs" = "9_11_hrs"))



# save 
saveRDS(dd, paste0(indata, "modif_dep_abcd_youth_sensitivity_withClinical_Jan2024_IMPUTED_FINAL_MERGED.rds"))


#####
# STEP 1: cross-sectional association
#####
# this is to test the relationship between each modifiable factor with internalizing at T1
# N.B. here adjusting for ethn and site in covariates because int at T1 was not previously residualized for these variables
# this cross-sectional approach is generally leveraged in the modifiable factor - depression literature

# set covariates
baselinevars <- c("sex", "ethn", "site", "puberty", "age", "parent_edu", "par_psych", "mat_age", "income")

# set your main predictors (modifiable factors)
pred_vars <- c("pa", "friends", "sleep", "splines::ns(screen,2)")

# make sure to scale the outcomes for interpretability 
dd$int_t2_centered <- scale(dd$int_t2)
dd$int_t1_centered <- scale(dd$int_t1)

# perform regression 
results_cs <- perform_regression_CS(baselinevars, pred_vars)
  
# filter results and select relevant statistics (rounded to 3 decimals)
results_cs[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results_cs[, c("Coefficient", "Standard_Error", "P_Value")], 3)


#####
# STEP 2: longitudinal without baseline adjustments 
#####
# this is to test the relationship between each modifiable factor with internalizing at T2
# (no baseline adjustments nor pre-selection of participants for levels of internalizing problems)
# NB the baseline vars are not changed here - ethn and site still included because in this dataset (which includes individuals with clinically-relevant symptoms
# of depression) the internalizing problems had not been previously residualized for these variables. This was done in the main dataset for running g-computation,
# which would fail otherwise. 

# linear regression
results_longi <- perform_regression_longi(baselinevars, pred_vars)

# Round all numeric columns to 3 decimals
results_longi[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results_longi[, c("Coefficient", "Standard_Error", "P_Value")], 3)


######
# Step 3: under all TTE conditions
######

# add to the covariates the baseline internalizing symptoms 
baselinevars <- c(baselinevars, "int_t1")

# filter for those without clinically-relevant symptoms of internalizing problems 
# this is because when we focus on those with clinically-relevant problems, we 
# are outside of the primary prevention realm 
dd <- dd[dd$int_tscore_t1 < 65, ] # 65 = clinical cut-off CBCL


# linear regression
results_TTE <- perform_regression_longi(baselinevars, pred_vars)

# select relevant results and round to 3 decimals
results_TTE[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results_TTE[, c("Coefficient", "Standard_Error", "P_Value")], 3)


#####
# Step 4: Merge outputs
#####

# bind the results from the cross-sectional, longitudinal and TTE analyses
all_res <- rbind("cross-sectional", results_cs, "longitudinal", results_longi, "TTE", results_TTE)

# save
write.csv(all_res, paste0(res, "TTE_ABCD.csv"), row.names = F)
 

