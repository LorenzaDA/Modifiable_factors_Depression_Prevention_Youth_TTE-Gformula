######################
# 3B. Sensitivity analyses with adjustments for family conflict
######################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: are results similar when we additionally adjust for the potential confounding effect of 
# family conflict on the relationship between modifiable factors and internalizing problems? [analysis suggested at peer review stage]
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)

rm(list=ls())
source("/PATH-WHERE-SOURCE-FILE-IS/0a.source_file_packages_paths_ABCD.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.source_file_analyses_ABCD.R")
load(paste0(indata, "modifiable_dep_youth_SENS_ADJ.RData"))

# set covs (NB ethn and site residualized for - see rationale in script 1.)
# compared to other models, here the variable composite_fam was added. This represents family conflict
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "int_t1", "mat_age", "income", "composite_fam")

# list and names of datasets
datasets <- list(sens_adj_dd4, sens_adj_hGr, sens_adj_hEr, sens_adj_indicated)
names(datasets) <- c("universal", "selective_PGS", "selective_LE", "indicated") 

# screen time (universal) analyses with additional adjustments for family conflict
model <- lm(paste("int_t2_centered ~ ns(screen, 2) +", paste(baselinevars, collapse = "+")), data = sens_adj_dd4)
summary(model)

# pa (indicated) analyses with additional adjustments for family conflict
model <- lm(paste("int_t2_centered ~ pa +", paste(baselinevars, collapse = "+")), data = sens_adj_indicated)
summary(model)


## g-formula screen time with additional adj. for family conflict

screen_levels <- c("0", "1", "2", "3", "4")

predict_and_contrast_screen_boot(datasets = datasets, 
                                 levels = screen_levels, 
                                 baselinevars = baselinevars,
                                 n_boot = 1000, 
                                 seed = 2023)



## g-formula pa with additional adj. for family conflict 

pa_levels <- c("0", "1", "2", "3", "4", "5", "6", "7")

predict_and_contrast_pa_boot(datasets = datasets, 
                             levels = pa_levels, 
                             baselinevars = baselinevars, 
                             n_boot = 1000, 
                             seed = 2023)

