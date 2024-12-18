#############################
# 1. Data prep GenR - modifiable factors depression prevention
#############################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT: prepare the dataset for analyses
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)

####
# Prep environment
####

# load source file
rm(list=ls())

source("/PATH-WHERE-SOURCE-FILE-IS/0a.Source_file_modifiable_dep_youth_GenR.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.Source_file_functions_modifiable_dep_youth_GenR.R")


# load data

files_genr <- c("CHILD-ALLGENERALDATA_24102022.sav","GR1084_F1_24012017.sav", "GR1095_F-Child-YSR_18062020.sav", 
                "CHILDCBCL9_incl_Tscores_20201111.sav", "GR1093-E1_CBCL_18062020.sav", 
                "GR1081_H5-8_21122020.sav", "20201117_L5onset.sav", "sleep_IDC_TOTAL SAMPLE_withandwithoutdiary191003.sav",
                "GR1084-B2_09082017.sav", "GR1081_I1-9_08082016.sav", "GR1081_I12-13_08082016.sav",
                 "CHILDPUBERTY13_14012022.sav", 
                "03052021_WISC.sav",  
                "CHILDSONIQ5_06042012.sav", "GR1075-D1-25_17072015.sav", 
                "CHILDCBCL_6_incl_Tscores_20201111.sav")


# read in all files into a list

paths_genr <- file.path(indata, files_genr)

data <- lapply(paths_genr, function(x)
  x %>%
    foreign::read.spss(to.data.frame = TRUE) %>%
    data.table::as.data.table() %>%
    {data.table::setnames(., tolower(names(.)))}
)


#######
# Merge the data
#######

### merge data ### 
# first for all those with IDC (id of the child) info  

data4 <- lapply(data, function (x) {x[order(x$idc), ]}) # order by participant ID

dd <- Reduce(function(d1, d2) merge(d1, d2, by = intersect(names(d1), names(d2)), all.x = TRUE), data4)  # merge 

dd2 <- as.data.frame(dd) # turn into dataframe


## merge with IDM
# there are some datasets that do not have the ID of the child because the data is 
# on the parent of the child
# so they have just the IDM as identification variable (ID mother)

dd2 <- dd2[order(dd2$idm), ] # order all prior datasets by IDM

mat_psych <- foreign::read.spss(paste0(indata, "/GR1003-BSI_D1_22112016.sav"), to.data.frame = TRUE) # load the dataset we need on the parent
mat_psych <- mat_psych[order(mat_psych$idm), ] # order by IDM

dd3 <- merge(dd2, mat_psych, by = "idm", all.x = T) # merge by IDM

dd3 <- dd3[order(dd3$idc), ] # reorder by IDC


#######
# Select the variables we need 
#######
# inclusion criteria: having data on modifiable factors at age 10 
# exclusion criteria: T score above 93rd percentile (i.e. clinically relevant symptoms)
# covariates: age, sex, national origin/ethnicity, maternal education, pubertal stage, maternal psych
# for subsamples: PGS genetics, life stress
# other key variables: non-response, siblings/twins var

vars <- c("idc", "idm", "mother", # id vars
        #  "sum_int_9m",  "sum_int_14", 
          "sum_internalizing_13s", "sum_int_9s", "internalizing_problems_tscore_9m", # output 
          "b0200184_cleaned", "b0200284_cleaned", "b0200384_cleaned", "b0200484_cleaned",
          "b0200584_cleaned", "b0200684_cleaned", "b0200784_cleaned", "b0200884_cleaned", "b0200984_cleaned", "b0201084_cleaned",  # friendship
          "i0300181_cleaned", "i0300281_cleaned", "i0400181_cleaned", "i0400281_cleaned", 
          "i0400381_cleaned", "i0400481_cleaned", "i0400581_cleaned", "i0400681_cleaned", 
          "i0800181_cleaned", "i0800281_cleaned", "i0900181_cleaned", "i0900281_cleaned", "i0900381_cleaned", 
          "i0900481_cleaned", "i0900581_cleaned", "i0900681_cleaned", 
          "i1200181_cleaned", "i1200281_cleaned", "i1300181_cleaned", "i1300281_cleaned", # screen time 
          "h0500181_cleaned", "h0500281_cleaned", "h0600181_cleaned", "h0700181_cleaned", 
          "h0700281_cleaned", "h0700381_cleaned", "h0700381_cleaned_recode", "h0800181_cleaned", "h0800281_cleaned", # sports 
          "acc_dur_noc_ad_t5a5_mn", "agechild", "acc_onset_ad_t5a5_mn", "acc_timeinbed_ad_t5a5_mn",  # sleep and its qc vars (sleep onset latency must be >0, onset of sleep > 6pm)
          "startfase3_9", "ethninfv3", "gender", "educm", "puberty13", "gsi", "agechild_cbcl9m",
          "age_m_v2", "income5", # covariates and non-response (startfase3_9)
        "wisc13_fsiq", "f0300178", # sensitivity IQ
        "d1300175", "d1300275", # For age 5 sensitivity analyses - days of tv watching - weekdays and weekends
        "d1400175", "d1400275", # mornings - tv - weekdays and weekends 
        "d1400375", "d1400475", # afternoon - tv - weekdays and weekends
        "d1400575", "d1400675", # evenings - tv - weekdays and weekends 
        "d2100175", "d2100275", # computer games - weekdays and weekends 
        "d2200175", "d2200275", # cg - mornings - weekdays and weekends
        "d2200375", "d2200475", # cg - afternoons - weekdays and weekends 
        "d2200575", "d2200675", # cg - evenings - weekdays and weekends ) 
        "sum_int_5", # internalizing at 5 for sensitivity analyses
)


# filter the df for the variables we need (specified in vars vector)
dd3 <- dd3[ , names(dd3) %in% vars] 

# merge with the PGS files
# we have two, one per genetic data release
# more info can be found in the work of Dr. Nicole Creasy)
prs_genr4 <- read.csv("mdd_GENR4.all_score.csv") 
prs_genr3 <- read.csv("mdd_GENR3.all_score.csv")

# order by ID
prs_genr3 <- prs_genr3[order(prs_genr3$IID), ]
prs_genr4 <- prs_genr4[order(prs_genr4$IID), ]

# select european ancestry 
selection_genr3 <- foreign::read.spss("/PATH-WHERE-DATA-IS/PCA_Selection GWAv3_revised def_European-October2022.sav", to.data.frame = T)
selection_genr3 <- selection_genr3[ , c("IDC", "GWAv3European")]

selection_genr4 <- foreign::read.spss("/PATH-WHERE-DATA-IS/PCA_Selection GWAv4_revised def_European-October2022.sav", to.data.frame = T)
selection_genr4 <- selection_genr4[ , c("IDC", "GWAv4European")]

selection_all <- c(selection_genr3$IDC, selection_genr4$IDC) 

# get z-scores for the PGS (in line with guidelines from the GenR group for handling data from the two releases)
prs_genr3 <- prs_genr3 %>% 
  mutate(zscore = (Pt_5e.08 - mean(Pt_5e.08))/sd(Pt_5e.08))

prs_genr4 <- prs_genr4 %>% 
  mutate(zscore = (Pt_5e.08 - mean(Pt_5e.08))/sd(Pt_5e.08))


# merge datasets 
prs <- merge(prs_genr3, prs_genr4, by = intersect(names(prs_genr3), names(prs_genr4)), all = T)

# select relevant IDs and columns
prs_selected <- prs[prs$IID %in% selection_all, c("IID", "Pt_5e.08")]  

# merge data
dd3 <- merge(dd3, prs_selected, by.x = "idc", by.y = "IID", all.x = T)

# load stressful life events data (work by Dr. Isabel Schuurmans)
els <- readRDS("postnatal_stress_harmonizedwithABCD_5Jan2024.rds")

els <- els[order(els$IDC), ] 

keep <- c("IDC", "postnatal_stress") # we need postnatal stress data (not prenatal too)

els <- els[ , names(els) %in% keep]

dd3 <- merge(dd3, els, by.x = "idc", by.y = "IDC", all.x = T) # merge with the other datasets


#####
# Compute exposure vars
#####

### physicial activity ### 
# based on Dr Fernando Estevez Lopez's work in GenR for computing overall PA
# reference: doi:10.1001/jamanetworkopen.2023.33157

summary(dd3[ , c("h0500181_cleaned",
                 "h0500281_cleaned",
                 "h0600181_cleaned",
                 "h0700181_cleaned",
                 "h0700281_cleaned",
                 "h0700381_cleaned",
                 "h0700381_cleaned_recode",
                 "h0800181_cleaned",
                 "h0800281_cleaned")])


# 1. SPORTs (MIN/WEEK)
# H0700281_cleaned : (GR1081 H7-b) How many hours per week does your child spend doing sports (training and compete together)?
# 1 = Less than 1 hour per week / 2 = 1 to 2 hours per week / 3 = 2 to 4 hours per week / 4 = More than 4 hours per week
# Recode: The score of the maximum value (i.e., more than 240 min) is increased in a 12.5% (as done for all the variables)

dd3$sports_maternal_report_9 <- ifelse(dd3$h0700281_cleaned== "Less than 1 hour per week", 30,
                                      ifelse(dd3$h0700281_cleaned== "1 to 2 hours per week", 90,
                                             ifelse(dd3$h0700281_cleaned== "2 to 4 hours per week", 180,
                                                    ifelse(dd3$h0700281_cleaned== "More than 4 hours per week", 270, NA))))


dd3$sports_maternal_report_9 <- as.numeric(dd3$sports_maternal_report_9)


# 2. PLAY OUTSIDE (MIN/WEEK)
# H0800181_cleaned (GR1081 H8-a) On average how many days per week does your child play outside?
# 1 = Never / 2 = 1 or 2 days per week / 3 = 3 or 4 days per week / 4 = 5 or more days per week

dd3$play_maternal_report_days_9 <-  ifelse(dd3$h0800181_cleaned== "Never, continue to question 9", 0,
                                          ifelse(dd3$h0800181_cleaned== "1 or 2 days per week", 1.5,
                                                 ifelse(dd3$h0800181_cleaned== "3 or 4 days per week", 3.5,
                                                        ifelse(dd3$h0800181_cleaned== "5 or more days per week", 6, NA))))



# H0800281_cleaned: (GR1081 H8-b) Approximately how long does your child approximately play outside per day? Only consider the days that your child plays outside.
# 1 = Less than 30 minutes per day / 2 = 30 minutes to 1 hour per day / 3 = 1 to 2 hours per day / 4 = 2 to 3 hours per day / 5 = 3 to 4 hours per day / 6 = More than 4 hours per day
# Recode: The score of the maximum value (i.e., more than 240 min) is increased in a 12.5% (as done for all the variables)

dd3$play_maternal_report_min_9 <- ifelse(dd3$h0800281_cleaned== "Less than 30 minutes per day", 15,
                                        ifelse(dd3$h0800281_cleaned== "30 minutes to 1 hour per day", 45,
                                               ifelse(dd3$h0800281_cleaned== "1 to 2 hours per day", 90,
                                                      ifelse(dd3$h0800281_cleaned== "2 to 3 hours per day", 150,
                                                             ifelse(dd3$h0800281_cleaned== "3 to 4 hours per day", 210,
                                                                    ifelse(dd3$h0800281_cleaned=="More than 4 hours per day", 270, NA))))))


# total play time (min/week)
dd3$play_maternal_report_9 = dd3$play_maternal_report_days_9 * dd3$play_maternal_report_min_9


# 4. TOTAL PHYSICAL ACTIVITY

dd3$pa = dd3$sports_maternal_report_9 + dd3$play_maternal_report_9


# get a daily average and in hours

dd3$pa_hrs <- dd3$pa/24
dd3$pa_final <- dd3$pa_hrs/7 # average each day 



#### total friendship quality ####
# computed based on prior study in GenR
# https://link.springer.com/article/10.1007/s10802-019-00550-5 
# weighted sum - allowing for 20% missingness 
# when data was missing, filled in with the average of the remaining items

vars_to_sum <- c("b0200184_cleaned", "b0200284_cleaned", 
          "b0200384_cleaned", "b0200484_cleaned", 
          "b0200584_cleaned", "b0200684_cleaned", 
          "b0200784_cleaned", "b0200884_cleaned", 
          "b0200984_cleaned", "b0201084_cleaned")


# you need a loop here to make every col with not true = 1, 
# 1.5 = 1.5 (in between values meaning that both true and somewhat true were crossed)
# somewhat true = 2, 2.5. = 2.5 (same as for 1.5 but children rated in between 
# somewhat true and very true); very true = 3


dd4 <- dd3 %>% dplyr::mutate(b0200184_cleaned = as.numeric(as.character(recode_factor(b0200184_cleaned, 
                                                      `Not true` = "1",
                                                      `1.5` = "1.5",
                                                      `Somewhat true` = "2",
                                                      `2.5` = "2.5", 
                                                      `Very true` = "3"))), 
                      b0200284_cleaned = as.numeric(as.character(recode_factor(b0200284_cleaned, 
                                                       `Not true` = "1",
                                                       `1.5` = "1.5",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))),
                      b0200384_cleaned = as.numeric(as.character(recode_factor(b0200384_cleaned, 
                                                       `Not true` = "1",
                                                       `1.5` = "1.5",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))),
                      b0200484_cleaned = as.numeric(as.character(recode_factor(b0200484_cleaned, 
                                                       `Not true` = "1",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))), 
                      b0200584_cleaned = as.numeric(as.character(recode_factor(b0200584_cleaned, 
                                                       `Not true` = "1",
                                                       `1.5` = "1.5",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))), 
                      b0200684_cleaned = as.numeric(as.character(recode_factor(b0200684_cleaned, 
                                                       `Not true` = "1",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))), 
                      b0200784_cleaned = as.numeric(as.character(recode_factor(b0200784_cleaned, 
                                                       `Not true` = "1",
                                                       `1.5` = "1.5",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))), 
                      b0200884_cleaned = as.numeric(as.character(recode_factor(b0200884_cleaned, 
                                                       `Not true` = "1",
                                                       `1.5` = "1.5",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))), 
                      b0200984_cleaned = as.numeric(as.character(recode_factor(b0200984_cleaned, 
                                                       `Not true` = "1",
                                                       `1.5` = "1.5",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3"))), 
                      b0201084_cleaned = as.numeric(as.character(recode_factor(b0201084_cleaned, 
                                                       `Not true` = "1",
                                                       `Somewhat true` = "2",
                                                       `2.5` = "2.5", 
                                                       `Very true` = "3")))
)
             


# Applying the sum_across_columns function and saving the result in a new variable 
dd5 <- sum_across_columns(dd4, vars_to_sum = vars_to_sum, new_var_name = "friends", max_missing_percent = 0.25)


##### screen time ######
# need an overall variable for total screen time - and make it into daily hours for 
# interpretability 
# this is following Dr Maria Rodriguez-Ayllon's work, where data on video games and TV watching was combined
# https://www.sciencedirect.com/science/article/pii/S1053811919308493 

# weekdays TV watching: 
# I0300181_cleaned --> how many weekdays 
# I0400181_cleaned (how long on mornings in weekdays)
# I0400381_cleaned (how long on afternoons in weekdays)
# I0400581_cleaned (how long on evenings in weekdays)


# weekend TV watching: 
# I0300281_cleaned --> how many weekend days
# I0400281_cleaned (how long on mornings in weekends)
# I0400481_cleaned (how long on afternoons in weekends)
# I0400681_cleaned (how long on evenings in weekends)


vars_screen <- c("i0300181_cleaned", "i0400181_cleaned", 
                "i0400381_cleaned", "i0400581_cleaned", 
                "i0300281_cleaned", "i0400281_cleaned", 
                "i0400481_cleaned", "i0400681_cleaned")


dd5 <- dd5 %>% dplyr::mutate(
  ### tv ####
  days_weekday = as.numeric(as.character(recode_factor(i0300181_cleaned, 
                                                          "Never on weekdays" = "0", 
                                                          "1 day per week" = "1", 
                                                          "2 days per week" = "2", 
                                                          "3 days per week" = "3", 
                                                          "4 days per week" = "4", 
                                                          "Every weekday" = "5"))), 
                             days_weekend = as.numeric(as.character(recode_factor(i0300281_cleaned, 
                                                          "Never in the weekend" = "0", 
                                                          "1 day in the weekend" = "1", 
                                                          "2 days in the weekend" = "2"))), 
                             mornings_wd = as.numeric(as.character(recode_factor(i0400181_cleaned,
                                                                                "Never" = "0", 
                                                                                "Less than 30 minutes" = "15", 
                                                                                "30-60 minutes" = "45", 
                                                                                "1-2 hours" = "90", 
                                                                                "2-3 hours" = "150", 
                                                                                "3-4 hours" = "210"
                                                         ))),
                             afternoon_wd = as.numeric(as.character(recode_factor(i0400381_cleaned, 
                                                                                 "Never" = "0", 
                                                                                 "Less than 30 minutes" = "15", 
                                                                                 "30-60 minutes" = "45", 
                                                                                 "1-2 hours" = "90", 
                                                                                 "2-3 hours" = "150", 
                                                                                 "3-4 hours" = "210"
                                                         ))), 
                            evening_wd = as.numeric(as.character(recode_factor(i0400581_cleaned, 
                                                                               "Never" = "0", 
                                                                               "Less than 30 minutes" = "15", 
                                                                               "30-60 minutes" = "45", 
                                                                               "1-2 hours" = "90", 
                                                                               "2-3 hours" = "150", 
                                                                               "3-4 hours" = "210"
                                                         ))), 
                            morning_we = as.numeric(as.character(recode_factor(i0400281_cleaned, 
                                                                               "Never" = "0", 
                                                                               "Less than 30 minutes" = "15", 
                                                                               "30-60 minutes" = "45", 
                                                                               "1-2 hours" = "90", 
                                                                               "2-3 hours" = "150", 
                                                                               "3-4 hours" = "210"
                                                        ))), 
                            afternoon_we = as.numeric(as.character(recode_factor(i0400481_cleaned, 
                                                                                 "Never" = "0", 
                                                                                 "Less than 30 minutes" = "15", 
                                                                                 "30-60 minutes" = "45", 
                                                                                 "1-2 hours" = "90", 
                                                                                 "2-3 hours" = "150", 
                                                                                 "3-4 hours" = "210"
                                                        ))),
                             evening_we = as.numeric(as.character(recode_factor(i0400681_cleaned, 
                                                                                "Never" = "0", 
                                                                                "Less than 30 minutes" = "15", 
                                                                                "30-60 minutes" = "45", 
                                                                                "1-2 hours" = "90", 
                                                                                "2-3 hours" = "150", 
                                                                                "3-4 hours" = "210"))), 
                            tv_wd = ((days_weekday * (mornings_wd + afternoon_wd + evening_wd))),
                            # for each day of the week in which tv is watched how many min across the whole day
                            tv_we = ((days_weekend * (morning_we + afternoon_we + evening_we))), 
                            # sum tot weekend and week and divide by n days 
                            tv_tot_min = (tv_wd + tv_we)/7,
                            # transform into hours
                            tv_tot = tv_tot_min/60, # daily hours of tv
  
                            ### computer games ###
                            
                            weekdays_cg = as.numeric(as.character(recode_factor(i0800181_cleaned, 
                                                                                "Never on weekdays" = "0", 
                                                                                "1 day per week" = "1", 
                                                                                "2 days per week" = "2", 
                                                                                "3 days per week" = "3", 
                                                                                "4 days per week" = "4", 
                                                                                "Every weekday" = "5"))), 
                            
                           morning_wd_cg = as.numeric(as.character(recode_factor(i0900181_cleaned, 
                                                                                 "Never" = "0",
                                                                                 "Less than 30 minutes" = "15", 
                                                                                 "30-60 minutes" = "45", 
                                                                                 "1-2 hours" = "90", 
                                                                                 "2-3 hours" = "150", 
                                                                                 "3-4 hours" = "210"))), 
                          afternoon_wd_cg = as.numeric(as.character(recode_factor(i0900381_cleaned, 
                                                                                  "Never" = "0",
                                                                                  "Less than 30 minutes" = "15", 
                                                                                  "30-60 minutes" = "45", 
                                                                                  "1-2 hours" = "90", 
                                                                                  "2-3 hours" = "150", 
                                                                                  "3-4 hours" = "210"))),
                          evening_wd_cg = as.numeric(as.character(recode_factor(i0900581_cleaned, 
                                                                                "Never" = "0",
                                                                                "Less than 30 minutes" = "15", 
                                                                                "30-60 minutes" = "45", 
                                                                                "1-2 hours" = "90", 
                                                                                "2-3 hours" = "150", 
                                                                                "3-4 hours" = "210"))),   
                        weekends_cg = as.numeric(as.character(recode_factor(i0800281_cleaned, 
                                                                            "Never in the weekend" = "0", 
                                                                            "1 day in the weekend" = "1", 
                                                                            "2 days in the weekend" = "2" 
                                                                            ))), 
                        morning_we_cg = as.numeric(as.character(recode_factor(i0900281_cleaned, 
                                                                              "Never" = "0",
                                                                              "Less than 30 minutes" = "15", 
                                                                              "30-60 minutes" = "45", 
                                                                              "1-2 hours" = "90", 
                                                                              "2-3 hours" = "150", 
                                                                              "3-4 hours" = "210"))), 
                        afternoon_we_cg = as.numeric(as.character(recode_factor(i0900481_cleaned, 
                                                                                "Never" = "0",
                                                                                "Less than 30 minutes" = "15", 
                                                                                "30-60 minutes" = "45", 
                                                                                "1-2 hours" = "90", 
                                                                                "2-3 hours" = "150", 
                                                                                "3-4 hours" = "210"))), 
                        evening_we_cg = as.numeric(as.character(recode_factor(i0900681_cleaned, 
                                                                              "Never" = "0",
                                                                              "Less than 30 minutes" = "15", 
                                                                              "30-60 minutes" = "45", 
                                                                              "1-2 hours" = "90", 
                                                                              "2-3 hours" = "150", 
                                                                              "3-4 hours" = "210"))), 
                      cg_wd = weekdays_cg * (morning_wd_cg + afternoon_wd_cg + evening_wd_cg),  
                      cg_we = weekends_cg * (morning_we_cg + afternoon_we_cg + evening_we_cg), 
                      cg_tot_min = (cg_wd + cg_we)/7, # daily min 
                      cg_tot = cg_tot_min/60, # daily hours of computer games
    
                      ### put everything together ####
                      screen_tot = tv_tot + cg_tot # daily hours of tv and computer games
)




### screen time for age 5 (for sensitivity analyses) #####
#"d1300175", "d1300275", # For age 5 sensitivity analyses - days of tv watching - weekdays and weekends
#"d1400175", "d1400275", # mornings - tv - weekdays and weekends 
#"d1400375", "d1400475", # afternoon - tv - weekdays and weekends
#"d1400575", "d1400675", # evenings - tv - weekdays and weekends 
#"d2100175", "d2100275", # computer games - weekdays and weekends 
#"d2200175", "d2200275", # cg - mornings - weekdays and weekends
#"d2200375", "d2200475", # cg - afternoons - weekdays and weekends 
#"d2200575", "d2200675") # cg - evenings - weekdays and weekends ) 


dd5 <- dd5 %>% dplyr::mutate(
  ### tv ####
  days_weekday_5 = as.numeric(as.character(recode_factor(d1300175, 
                                                       "Never on weekdays" = "0", 
                                                       "1 day per week" = "1", 
                                                       "2 days per week" = "2", 
                                                       "3 days per week" = "3", 
                                                       "4 days per week" = "4", 
                                                       "Every weekday" = "5"))), 
  days_weekend_5 = as.numeric(as.character(recode_factor(d1300275, 
                                                       "Never in the weekend" = "0", 
                                                       "1 day in the week-end" = "1", 
                                                       "2 days in the weekend" = "2"))), 
  mornings_wd_5 = as.numeric(as.character(recode_factor(d1400175,
                                                      "Never" = "0", 
                                                      "Less than 30 minutes" = "15", 
                                                      "30-60 minutes" = "45", 
                                                      "1-2 hours" = "90", 
                                                      "2-3 hours" = "150", 
                                                      "3-4 hours" = "210"
  ))),
  afternoon_wd_5 = as.numeric(as.character(recode_factor(d1400275, 
                                                       "Never" = "0", 
                                                       "Less than 30 minutes" = "15", 
                                                       "30-60 minutes" = "45", 
                                                       "1-2 hours" = "90", 
                                                       "2-3 hours" = "150", 
                                                       "3-4 hours" = "210"
  ))), 
  evening_wd_5 = as.numeric(as.character(recode_factor(d1400375, 
                                                     "Never" = "0", 
                                                     "Less than 30 minutes" = "15", 
                                                     "30-60 minutes" = "45", 
                                                     "1-2 hours" = "90", 
                                                     "2-3 hours" = "150", 
                                                     "3-4 hours" = "210"
  ))), 
  morning_we_5 = as.numeric(as.character(recode_factor(d1400475, 
                                                     "Never" = "0", 
                                                     "Less than 30 minutes" = "15", 
                                                     "30-60 minutes" = "45", 
                                                     "1-2 hours" = "90", 
                                                     "2-3 hours" = "150", 
                                                     "3-4 hours" = "210"
  ))), 
  afternoon_we_5 = as.numeric(as.character(recode_factor(d1400575, 
                                                       "Never" = "0", 
                                                       "Less than 30 minutes" = "15", 
                                                       "30-60 minutes" = "45", 
                                                       "1-2 hours" = "90", 
                                                       "2-3 hours" = "150", 
                                                       "3-4 hours" = "210"
  ))),
  evening_we_5 = as.numeric(as.character(recode_factor(d1400675, 
                                                     "Never" = "0", 
                                                     "Less than 30 minutes" = "15", 
                                                     "30-60 minutes" = "45", 
                                                     "1-2 hours" = "90", 
                                                     "2-3 hours" = "150", 
                                                     "3-4 hours" = "210"))), 
  tv_wd_5 = ((days_weekday_5 * (mornings_wd_5 + afternoon_wd_5 + evening_wd_5))),
  # for each day of the week in which tv is watched how many min across the whole day
  tv_we_5 = ((days_weekend_5 * (morning_we_5 + afternoon_we_5 + evening_we_5))), 
  # sum tot weekend and week and divide by n days 
  tv_tot_min_5 = (tv_wd_5 + tv_we_5)/7,
  # transform into hours
  tv_tot_5 = tv_tot_min_5/60, # daily hours of tv
  
  ### computer games ###
  
  weekdays_cg_5 = as.numeric(as.character(recode_factor(d2100175, 
                                                      "Never on weekdays" = "0", 
                                                      "1 day per week" = "1", 
                                                      "2 days per week" = "2", 
                                                      "3 days per week" = "3", 
                                                      "4 days per week" = "4", 
                                                      "Every weekday" = "5"))), 
  
  morning_wd_cg_5 = as.numeric(as.character(recode_factor(d2200375, 
                                                        "Never" = "0",
                                                        "Less than 30 minutes" = "15", 
                                                        "30-60 minutes" = "45", 
                                                        "1-2 hours" = "90", 
                                                        "2-3 hours" = "150", 
                                                        "3-4 hours" = "210"))), 
  afternoon_wd_cg_5 = as.numeric(as.character(recode_factor(d2200175, 
                                                          "Never" = "0",
                                                          "Less than 30 minutes" = "15", 
                                                          "30-60 minutes" = "45", 
                                                          "1-2 hours" = "90", 
                                                          "2-3 hours" = "150", 
                                                          "3-4 hours" = "210"))),
  evening_wd_cg_5 = as.numeric(as.character(recode_factor(d2200275, 
                                                        "Never" = "0",
                                                        "Less than 30 minutes" = "15", 
                                                        "30-60 minutes" = "45", 
                                                        "1-2 hours" = "90", 
                                                        "2-3 hours" = "150", 
                                                        "3-4 hours" = "210"))),   
  weekends_cg_5 = as.numeric(as.character(recode_factor(d2100275,
                                                      "Never in the weekend" = "0", 
                                                      "1 day in the week-end" = "1", 
                                                      "2 days in the weekend" = "2" 
  ))), 
  morning_we_cg_5 = as.numeric(as.character(recode_factor(d2200475, 
                                                        "Never" = "0",
                                                        "Less than 30 minutes" = "15", 
                                                        "30-60 minutes" = "45", 
                                                        "1-2 hours" = "90", 
                                                        "2-3 hours" = "150", 
                                                        "3-4 hours" = "210"))), 
  afternoon_we_cg_5 = as.numeric(as.character(recode_factor(d2200575, 
                                                          "Never" = "0",
                                                          "Less than 30 minutes" = "15", 
                                                          "30-60 minutes" = "45", 
                                                          "1-2 hours" = "90", 
                                                          "2-3 hours" = "150", 
                                                          "3-4 hours" = "210"))), 
  evening_we_cg_5 = as.numeric(as.character(recode_factor(d2200675, 
                                                        "Never" = "0",
                                                        "Less than 30 minutes" = "15", 
                                                        "30-60 minutes" = "45", 
                                                        "1-2 hours" = "90", 
                                                        "2-3 hours" = "150", 
                                                        "3-4 hours" = "210"))), 
  cg_wd_5 = weekdays_cg_5 * (morning_wd_cg_5 + afternoon_wd_cg_5 + evening_wd_cg_5),  
  cg_we_5 = weekends_cg_5 * (morning_we_cg_5 + afternoon_we_cg_5 + evening_we_cg_5), 
  cg_tot_min_5 = (cg_wd_5 + cg_we_5)/7, # daily min 
  cg_tot_5 = cg_tot_min_5/60, # daily hours of computer games
  
  ### put everything together ####
  screen_tot_5 = tv_tot_5 + cg_tot_5 # daily hours of tv and computer games
)


### sleep ### 
# based on Dr. Elisabet Blok's work 
# doi: 10.1186/s13034-022-00447-0

dd5$sleep <- NA 

for(i in 1:nrow(dd5)){
  if(!is.na(dd5$acc_dur_noc_ad_t5a5_mn[i]) & (dd5$acc_onset_ad_t5a5_mn[i] < 0 | dd5$acc_timeinbed_ad_t5a5_mn[i] < 6 | dd5$agechild[i] > 12.5)){
    dd5$sleep[i] <- NA
  }else{
      dd5$sleep[i] <- dd5$acc_dur_noc_ad_t5a5_mn[i]
  }
}



#########
# Variable recoding & naming
#########

dd6 <- dd5 %>% dplyr::mutate(mat_edu = recode_factor(educm, "no education finished" = 'low_int', 
                                                   "primary" = "low_int",  "secondary, phase 1" = "low_int", 
                                                   "secondary, phase 2" = "low_int", "higher, phase 1"= "high",
                                                   "higher, phase 2" = "high"),
                             idc = as.character(idc),
                             idm = as.character(idm), 
                             mother = as.character(mother),
                             income = recode_factor(income5, "Less than € 800" = "low_int", "€ 800-1200" = "low_int", 
                                                    "€ 1200-1600" = "low_int", "€ 1600-2000" = "low_int", 
                                                    "€ 2000-2400" = "low_int", "€ 2400-2800" = "low_int", 
                                                    "€ 2800-3200" = "low_int", "€ 3200-4000" = "low_int", 
                                                    "€ 4000-4800" = "high", "€ 4800-5600" = "high", "More than € 5600" = "high"),
                             mat_age = age_m_v2, 
                             ethn = recode_factor(ethninfv3, "Dutch" = "dutch", "Moroccan" = "other", 
                                                  "Indonesian" = "other", "Cape Verdian" = "other", "Dutch Antilles" = "other", 
                                                  "Surinamese" = "other", "Turkish" = "european", "Surinamese-Creole" = "other",
                                                  "Surinamese-Hindustani" = "european", "Surinamese-Unspecified" = "other", 
                                                  "African" = "other", "American,western" = "other", "American, non western" = "other", "Asian, western" = "other",
                                                  "Asian, non western" = "other", "European" = "european", "Oceanie" = "other"
                                                  ))  %>% dplyr::rename(
                      int_c_14 = sum_internalizing_13s, 
                      int_c_9 = sum_int_9s, 
                      int_5 = sum_int_5,
                      age = agechild_cbcl9m, 
                      puberty = puberty13, 
                      mat_psych = gsi,
                      sex = gender, 
                      screen = screen_tot, 
                      prs = Pt_5e.08, # threshold most predictive of outcome in this case
                      iq_t2 = wisc13_fsiq, 
                      iq_t1 = f0300178, 
                      screen_5 = screen_tot_5
                      
                      
                                                  )%>% dplyr::select(
                                                   int_c_14, int_c_9, age, sex, puberty,
                                                    mat_edu, mat_psych, idc, mother, startfase3_9, ethn, sleep,
                                                    pa_final, friends, screen, prs, postnatal_stress, mother,
                                                   income, mat_age, iq_t2, iq_t1, screen_5, int_5
                                                  )


####
# Flow chart
#### 

# 93rd percentile for clinically-relevant symptoms in population-based samples
percentile_93 <- quantile(dd6$int_c_9, 0.93, na.rm = T) # value 6 in internalizing symptoms

# 80th percentile for subclinical symptoms in population-based samples
percentile_80 <- quantile(dd6$int_c_9, 0.80, na.rm = T) # value of 4 in internalizing symptoms 


# Participant selection
# Inclusion: has data on internalizing symptoms
# & has data on any of the modifiable factors at baseline 
# Exclusion: clinically-relevant internalizing symptoms at baseline
# and randomly include on sibling from each family set

final_p <- dd6 %>% 
  subset((!is.na(int_c_9))) %>% 
  subset(!is.na(sleep) | !is.na(pa_final) | !is.na(friends) | !is.na(screen)) %>% 
  subset(int_c_9 < 6) %>% 
  subset(!duplicated(mother)) 

# save
saveRDS(final_p, paste0(indata, "genr_main_data_FINAL.Rds"))

# create dataset for sensitivity analyses where we do not exclude for those with clinically-relevant internalizing symptoms at baseline
sens_tte <- dd6 %>%
  subset((!is.na(int_c_9))) %>% 
  subset(!is.na(sleep) | !is.na(pa_final) | !is.na(friends) | !is.na(screen)) %>% 
  subset(!duplicated(mother)) 

# save
saveRDS(sens_tte, paste0(indata, "genr_sens_tte_withclinical_Jan2024.Rds"))


dd <- final_p

########
# Create table 1
########

label(dd$int_c_9) <- "Internalizing problems (child report) age 10"
label(dd$int_c_14) <- "Internalizing problems (child report) age 14"
label(dd$int_5) <- "Internalizing problems (parent report) age 6"
label(dd$sleep) <- "Sleep duration (hrs)"
label(dd$pa_final) <- "Physical activity (hrs per day)"
label(dd$friends) <- "Quality friendship"
label(dd$screen) <- "Screen time (hrs per day)"
label(dd$mat_edu) <- "Parental education"
label(dd$mat_psych) <- "Parental psychopathology"
label(dd$age) <- "Age"
label(dd$sex) <- "Sex"
label(dd$ethn) <- "Ethnicity"
label(dd$puberty) <- "Self-perceived pubertal stage"
label(dd$postnatal_stress) <- "Stressful life events"
label(dd$prs) <- "Polygenic score"
label(dd$income) <- "Household income"
label(dd$mat_age) <- "Maternal age"

# universal prevention setting
table1 <- table1::table1(~ int_c_9 + int_c_14 + int_5 +
                           pa_final + friends+ sleep + screen +
                           mat_edu + mat_psych + age + sex + ethn + puberty
                           + postnatal_stress + prs + income + mat_age,
                         data = dd)

table1


#########
# Imputation 
#########

out <- c("mother", "startfase3_9")

dd <- dd[ , !names(dd) %in% out]

### Variables 
predictors_for_imputation <- c("mat_age", "mat_edu", "income", "mat_psych", "puberty", "ethn")
impvars <- c("mat_age", "mat_edu", "income", "mat_psych", "puberty", "ethn", "age")  

#dryrun mice (page 35 mice guide)
dd$idc <- factor(dd$idc)
ini <- mice(dd, maxit = 0, printF = FALSE)

### Prediction matrix
# set the prediction matrix completely to 0, i.e. nothing predicts anything
pred <- ini$pred
pred[] <- 0

# set the variables that you want to impute as 1s in the predictor matrix so that they will be imputed 
pred[rownames(pred) %in% impvars, colnames(pred) %in% impvars] <- 1

# diagonal elements need to be 0s (i.e. one variable does not predict itself)
diag(pred) <- 0


### Imputation method

# get the method for imputation
meth <- ini$meth

# put to null "" for every combination that is no in the impvars 
meth[!names(meth) %in% impvars] <- "" 

### Order of imputation 
visit <- ini$visit
visit <- visit[visit %in% impvars]
visit2 <- c("mat_age", "mat_edu", "income", "mat_psych", "age", "puberty", 'ethn')


# run the imputation with m (number of datasets) and maxit (number of iterations)
## here 30 iterations and samples were set 
imp <- mice::mice(dd, 
                  m = 30, 
                  maxit = 30, 
                  seed = 2023, 
                  pred = pred, 
                  meth = meth, 
                  visit = visit2)

# save
saveRDS(imp, paste0(indata, "modifiable_dep_youth_imp3030_July2024_GenR.rds"))

original_df <- readRDS(paste0(indata, "genr_main_data_FINAL.Rds"))

out <- c("mother", "startfase3_9")

original_df <- original_df[ , !names(original_df) %in% out]

# merge imputed data with the original dataframe
dd2 <- sjmisc::merge_imputations(
  original_df,
  dd,
  ori = original_df
)


# select relevant variables and rename them
dd3 <- dd2 %>% select(idc, int_c_14, int_c_9, 
                      sex, mat_edu, mat_psych, 
                      sleep, pa_final, friends, screen, 
                      prs, mat_age, postnatal_stress, age_imp, puberty_imp, 
                      mat_edu_imp, mat_psych_imp, ethn_imp, income_imp, iq_t2, iq_t1,
                      int_5, screen_5
) %>% rename(
  parent_edu = mat_edu_imp, 
  pa = pa_final, 
  par_psych = mat_psych_imp, 
  puberty = puberty_imp,
  int_t2 = int_c_14, 
  int_t1 = int_c_9, 
  stressful_life_events = postnatal_stress,
  age = age_imp,
  ethn = ethn_imp, 
  income = income_imp
)


######
# Create subsamples for the analyses in the other prevention settings
######
# identify those at high genetic liability for depression, with a high exposure to 
# stressful life events, and at indicated risk (subclinical baseline internalizing symptoms)

#### data prep for subsamples ###

# get the decile of risk for each individual based on genetics and stressful life events
dd3$deciles_g <- ntile(dd3$prs, 10) # genetic
dd3$deciles_t <- ntile(dd3$stressful_life_events, 10) # stressful life events

dd4 <- dd3


# residualize for ethnicity 
# this is because the analyses will be ran across all prevention strategy
# but the high genetic liability prevention strategy has been restricted to genetic european ancestry
# so when we loop across prevention strategies the scripts fail because the levels of the ethn variable
# are different in the genetic subsample compared to the other subsamples 
# by residualizing for ethnicity, we adjust for this variable while not encountering such computational problem

dd4 <- as.data.frame(dd4)

m2 <- lm(int_t2 ~ ethn, data=dd4, na.action = na.exclude)

dd4$int_t2_res <- residuals(m2)

dd4$int_t2_centered <- scale(dd4$int_t2_res)

dd4$int_t2_og <- dd4$int_t2 # store original, not-residualized version of internalizing

dd4$int_t2 <- dd4$int_t2_res # int t2 is the final var residualized for ethnicity 


### selective prevention - high genetic liability (top 2 deciles) ###
hGr <- dd4[dd4$deciles_g > 7 & !is.na(dd4$deciles_g),]  

# selection for high exposure to stressful life events prevention setting (top 2 deciles)
hEr <- dd4[dd4$deciles_t > 7 & !is.na(dd4$deciles_t), ] 

# check how many individuals have both high genetic and environmental liability to depression 
hGEr <- dd4[dd4$deciles_g > 7 & dd4$deciles_t > 7 
            & !is.na(dd4$deciles_g) & !is.na(dd4$deciles_t), ] 
# too few to create their own subgroup

### indicated prevention - subclinical symptoms of internalizing at baseline ###

indicated <- dd4 %>% subset(int_t1 >= 4) # based on the 80th percentile


###### save datasets ####
# universal, selective genes, selective trauma, indicated 

# save all datasets together
list_dd <- list(dd4, hGr, hEr, indicated)
names(list_dd) <- c("universal", "selective_genetic", "selective_trauma", "indicated")

save(hGr, hEr, indicated, dd4, list_dd, file = paste0(indata,"/", "modifiable_dep_youth_alldata_genr.RData"))
load("modifiable_dep_youth_alldata_genr.RData")

####
# create dataset for sensitivity analyses too 
####

### sensitivity g-formula ###
# this is an additional two datasets
# one is with low genetic liability to depression (low_hGr)
# one is with low symptoms of depression at baseline (low_indicated)

low_hGr <- dd4[!is.na(dd4$deciles_g) & dd4$deciles_g < 3, ] 
low_indicated <- dd4[dd4$int_t1 < 2, ] 

# save all datasets together
datasets <- list(dd4, hGr, hEr, indicated, low_hGr, low_indicated)
names(datasets) <- c("universal", "selective_genetic", "selective_trauma", "indicated","low genetic risk", "low int at baseline")

save(dd4, hGr, hEr, indicated, low_hGr, low_indicated, datasets, file = paste0(indata,"modifiable_dep_youth_alldata_inclSens_genr.RData"))


#####
# Sample characteristics 
#####

# get descriptive stats
summary_stats <- lapply(list_dd, function(x) psych::describe(x)) 
write.csv(summary_stats, paste0(tab, "descriptive_stats_allsamples_modifiable_dep_youth_genr.csv"))

# get table 1 for each dataset
# universal
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + income + mat_age, 
                         data = dd4)

table1


# selective genetic
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + income + mat_age, 
                         data = hGr)

table1

# selective exposure to stressful life events
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + income + mat_age, 
                         data = hEr)

table1


# indicated
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + income + mat_age, 
                         data = indicated)

table1




### sensitivity TTE analyses ####
# this is for a sensitivity analysis where cross-sectional, longitudinal associations are tested
# as well as associations when all TTE conditions are implemented 
# we need a different sample and to reimputed because here we are not excluding
# youth with clinically-relevant symptoms at baseline

out <- c("mother", "startfase3_9")

sens_tte <- sens_tte[ , !names(sens_tte) %in% out]

### variables 
predictors_for_imputation <- c("mat_age", "mat_edu", "income", "mat_psych", "puberty", "ethn")
impvars <- c("mat_age", "mat_edu", "income", "mat_psych", "puberty", "ethn", "age")  

sens_tte$idc <- factor(sens_tte$idc)
ini <- mice(sens_tte, maxit = 0, printF = FALSE)

### Prediction matrix
pred <- ini$pred
pred[] <- 0
pred[rownames(pred) %in% impvars, colnames(pred) %in% impvars] <- 1
diag(pred) <- 0

### Imputation method
                        
meth <- ini$meth
meth[!names(meth) %in% impvars] <- "" 

### Order of imputation 
visit <- ini$visit
visit <- visit[visit %in% impvars]
visit2 <- c("mat_age", "mat_edu", "income", "mat_psych", "age", "puberty", 'ethn')


# run the imputation with m (number of datasets) and maxit (number of iterations)
imp <- mice::mice(sens_tte, 
                  m = 30, 
                  maxit = 30, 
                  seed = 2023, 
                  pred = pred, 
                  meth = meth, 
                  visit = visit2)

saveRDS(imp, paste0(indata, "modifiable_dep_youth_sens_tte_withClinical_July2024_GenR.rds"))
tmp <- readRDS(paste0(indata, "modifiable_dep_youth_sens_tte_withClinical_July2024_GenR.rds"))


sens_tte <- sens_tte[ , !names(sens_tte) %in% out]

tte <- merge_imputations(
  sens_tte,
  imp,
  ori = sens_tte
)


tte_final <- tte %>% select(idc, int_c_14, int_c_9, 
                      sex, mat_edu, mat_psych, 
                      sleep, pa_final, friends, screen, 
                      prs, mat_age, postnatal_stress, age_imp, puberty_imp, 
                      mat_edu_imp, mat_psych_imp, ethn_imp, income_imp, iq_t2, iq_t1,
                      int_5, screen_5
) %>% rename(
  parent_edu = mat_edu_imp, 
  pa = pa_final, 
  par_psych = mat_psych_imp, 
  puberty = puberty_imp,
  int_t2 = int_c_14, 
  int_t1 = int_c_9, 
  stressful_life_events = postnatal_stress,
  age = age_imp,
  ethn = ethn_imp, 
  income = income_imp
)

saveRDS(tte_final, paste0(indata, "modifiable_dep_youth_sens_tte_withClinical_July2024_GenR.rds"))



