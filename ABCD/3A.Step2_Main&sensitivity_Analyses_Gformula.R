################
# 3A. Step 2 - g formula for screen time and PA & sensitivity analyses - ABCD
################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: conduct g-formula analyses for the potential effect of hypothetical interventions 
# on screen time and PA on internalizing problems (main analyses, natural course sensitivity analyses, 
# model misspecification sensitivity analyses)
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)

### set environment ###

rm(list=ls())
source("PATH-WHERE-SOURCE-FILE-IS/0a.source_file_packages_paths_ABCD.R")
source("PATH-WHERE-SOURCE-FILE-IS/0b.source_file_analyses_ABCD.R")

load(paste0(indata, "modifiable_dep_youth_alldata_inclSens.RData"))

# set covariates
# NB site and ethn were previously residualized for, to be able to loop across the different prevention scenario
# that is because the high genetic liability group is restricted to individuals of genetically European ancestry
# meaning that the loop would fail if ethn was included 
# this is similarly for site. the different datasets have different individuals
# so some sites which are present in the universal prevention setting, are not 
# in the other prevention setting. This limits our ability to loop across the various prevention settings (datasets)
# for these reasons, we residualized for ethn and site. 
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "int_t1", "mat_age", "income")


# prep list with all datasets (universal prevention, high genetic liability, high life stress events, indicated)
# NB here we include also the datasets for sensitivity analyses: 
# the one with individuals at low genetic liability for depression and those 
# at with low internalizing symptoms at baseline 
datasets <- list(dd4, hGr, hEr, indicated, low_hGr, low_indicated)
names(datasets) <- c("universal", "selective_PGS", "selective_LE", "indicated", "low_PGS", "low_indicated") 


#####
# screen time 
#####

# set the interventions you want to test for screen time
# i.e., set screen levels to 0, 1, 2, 3, and 4 in everyone
# to then see what their predicted internalizing problems would be under each of these interventions
screen_levels <- c("0", "1", "2", "3", "4")

# Hard assignment
# i.e., everyone is set to a given screen time level (0, 1, 3, 4) 
# compared to everyone is set to existing guidelines for screen time (2 hrs/day max) 
predict_and_contrast_screen_boot(datasets = datasets, 
                            levels = screen_levels, 
                            baselinevars = baselinevars,
                            n_boot = 1000, # n of bootstrapping for deriving the CIs
                            seed = 2023) # for randomization boot


# Natural course scenario
# i.e., everyone is set to a given screen time level (0, 1, 3, 4)
# compared to everyone keeps their existing screen time levels (no behavioral change)
predict_and_contrast_screen_nat(datasets = datasets, 
                            levels = screen_levels, 
                            baselinevars = baselinevars)

# Model misspecification
# i.e., predicted internalizing problems under natural course (no behavioral change)
# compared to observed internalizing problems
screen_modelmisp(datasets = datasets, 
             baselinevars = baselinevars,
             filepath = res)



########
# for PA 
#######

# set the PA levels for each intervention 
# 0 to7 days of PA with at least 1 hour of MVPA
pa_levels <- c("0", "1", "2", "3", "4", "5", "6", "7")


# Hard assignment 
predict_and_contrast_pa_boot(datasets = datasets, 
                        levels = pa_levels, 
                        baselinevars = baselinevars, 
                        n_boot = 1000, 
                        seed = 2023)


# Natural course scenario
predict_and_contrast_pa_nat(datasets = datasets, 
                            levels = pa_levels, 
                            baselinevars = baselinevars)


# Model misspecification 
pa_modelmisp(datasets = datasets, 
             baselinevars = baselinevars,
             filepath = res)


