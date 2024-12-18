################
# 1. Data prep - ABCD
################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: prepping the data before use for analyses 
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


####
# Prep env
####

rm(list=ls())

source("/PATH-WHERE-SOURCE-FILE-IS/0a.source_file_packages_paths_ABCD.R")

### load data ###

# outcome 

bpm <- read.delim(paste0(indata, "abcd_yssbpm01.txt"), header = T, na.strings=c("","NA")) 


# predictors 

PA_selfreport <- read.delim(paste0(indata, "abcd_yrb01.txt"), header = T, na.strings=c("","NA")) 
sleep_reports <- read.delim(paste0(indata, "abcd_sds01.txt"), header = T, na.strings=c("","NA")) 
friends_n <- read.delim(paste0(indata, "abcd_ysr01.txt"), header = T, na.strings = c("", "NA")) 
screen_p <- read.delim(paste0(indata, "stq01.txt"), header = T, na.strings = c("", "NA")) 


# covariates

demo <- read.delim(paste0(indata, "abcd_lpds01.txt"), header = T, na.strings=c("","NA"))
ethn_sib <- read.delim(paste0(indata, "acspsw03.txt"), header = T, na.strings=c("","NA")) 
pub_child <- read.delim(paste0(indata, "abcd_ssphy01.txt"), header = T, na.strings=c("","NA")) 
site <- read.delim(paste0(indata, "abcd_lt01.txt"), header = T, na.strings=c("","NA")) 
parpsych <- read.delim(paste0(indata, "abcd_asrs01.txt"), header = T, na.strings=c("","NA")) 
mat_age <- read.csv(paste0(indata, "dhx01.txt")) 
income <- read.csv(paste0(indata, "abcd_p_demo.csv"))

# selection 
prs <- fread(paste0(indata, "ABCD PRS/", "abcd_prs.txt")) 
trauma <- read.delim(paste0(indata, "abcd_mhy02.txt"), header = T, na.strings=c("","NA")) 


############
# Merging
############

# merge demo and psych data
demo <- demo[demo$eventname == "baseline_year_1_arm_1" | demo$eventname == "1_year_follow_up_y_arm_1", c("sex", "demo_prnt_ed_v2_l", "src_subject_id", "eventname", "interview_age", "demo_comb_income_v2_l")]
bpm <- bpm[bpm$eventname == "6_month_follow_up_arm_1" | bpm$eventname == "30_month_follow_up_arm_1", c("eventname", "src_subject_id","bpm_y_scr_internal_r", "bpm_y_scr_internal_t")]

# order
demo <- demo[with(demo, order(src_subject_id, eventname)), ] 
bpm <- bpm[with(bpm, order(src_subject_id, eventname)), ]

# merge 
merged1 <- merge(demo, bpm, by = c("src_subject_id", "eventname"), all = T)


### merge  with risk vars ###
trauma <- trauma[ , c("ple_y_ss_total_bad", "eventname", "src_subject_id")]
trauma <- trauma[with(trauma, order(src_subject_id, eventname)), ] 
merged2 <- merge(merged1, trauma, by = c("src_subject_id", "eventname"), all = T)


### merge with exposure data ###
# physical activity
PA_selfreport <- PA_selfreport[PA_selfreport$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", 
                                                                                     "physical_activity1_y")]
# screen
screen_p <- screen_p[screen_p$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "screentime1_p_hours",
                                                                      "screentime1_p_minutes", "screentime2_p_hours", "screentime2_p_minutes")]

# sleep 
sleep_reports <- sleep_reports[sleep_reports$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", 
                                                                                     "sleepdisturb1_p")]
# friends 
friends_n <- friends_n[friends_n$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", 
                                                                         "resiliency5b_y", "resiliency6b_y", "resiliency7b_y")]

## order data
merged2 <- merged2[with(merged2, order(src_subject_id, eventname)), ]
friends_n <- friends_n[with(friends_n, order(src_subject_id, eventname)), ]
sleep_reports <- sleep_reports[with(sleep_reports, order(src_subject_id, eventname)), ]
screen_p <- screen_p[with(screen_p, order(src_subject_id, eventname)), ]
PA_selfreport <- PA_selfreport[with(PA_selfreport, order(src_subject_id, eventname)), ]

## merge
merged3 <- merge(merged2, friends_n, by = c("src_subject_id", "eventname"), all = T) 
merged4 <- merge(merged3, sleep_reports, by = c("src_subject_id", "eventname"), all = T)
merged5 <- merge(merged4, screen_p, by = c("src_subject_id", "eventname"), all = T)
merged6 <- merge(merged5, PA_selfreport, by = c("src_subject_id", "eventname"), all = T)


###### merge with covs ####
# sibs and ethnicity info 
ethn_sib <- ethn_sib[ethn_sib$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "race_ethnicity", "rel_family_id")]
ethn_sib <- ethn_sib[with(ethn_sib, order(src_subject_id, eventname)), ]
merged7 <- merge(merged6, ethn_sib, by = c("src_subject_id", "eventname"), all = T)

# puberty 
pub_child <- pub_child[pub_child$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "pds_y_ss_female_category_2", "pds_y_ss_male_cat_2")]
pub_child <- pub_child[with(pub_child, order(src_subject_id, eventname)), ]
merged8 <- merge(merged7, pub_child, by = c("src_subject_id", "eventname"), all = T)

# site 
site <- site[site$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "site_id_l")]
site <- site[with(site, order(src_subject_id, eventname)), ]
merged9 <- merge(merged8, site, by = c("src_subject_id", "eventname"), all = T)

# parental psych 
parpsych <- parpsych[parpsych$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "asr_scr_totprob_r")]
parpsych <- parpsych[with(parpsych, order(src_subject_id, eventname)), ]
merged10 <- merge(merged9, parpsych, by = intersect(names(merged9), names(parpsych)), all = T)

# income 
income <- income[income$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "demo_comb_income_v2_l")]
income <- income[with(income, order(src_subject_id, eventname)), ]
merged11 <- merge(merged10, income, by = intersect(names(merged10), names(income)), all.x = T)

# mat age
mat_age <- mat_age[mat_age$eventname == "baseline_year_1_arm_1", c("src_subject_id", "eventname", "devhx_3_p")]
merged12 <- merge(merged11, mat_age, by = intersect(names(merged11), names(mat_age)), all.x = T)

dd <- merged12

#####
# Data-frame checks
#####

# change variable type
dd2 <- dd %>% 
  # recode parent edu
  mutate(parent_edu = recode_factor(demo_prnt_ed_v2_l,
                                    "1" = "low_int",
                                    "2" = "low_int",
                                    "3" = "low_int",
                                    "4" = "low_int",
                                    "5" = "low_int",
                                    "6" = "low_int", 
                                    "7" = "low_int", 
                                    "8" = "low_int", 
                                    "9" = "low_int", 
                                    "10" = "low_int", 
                                    "11" = "low_int", 
                                    "12" = "low_int", 
                                    "13" = "low_int", 
                                    "14" = "low_int", 
                                    "15" = "low_int",
                                    "16" = "low_int", 
                                    "17" = "low_int", 
                                    "18" = "high", 
                                    "19" = "high", 
                                    "20" = "high", 
                                    "21" = "high", 
                                    "777" = "refuse_answer"), 
         # recode sleep into less categories (low N for less than 5 hours sleep)
         sleep = recode_factor(sleepdisturb1_p, 
                               "1" = "9_11_hrs", 
                               "2" = "8_9_hrs", 
                               "3" = "7_8_hrs", 
                               "4" = "less_7_hrs", 
                               "5" = "less_7_hrs"), 
         # recode income into low to intermediate and high - too few participants in low income
         income = recode_factor(demo_comb_income_v2_l, 
                                 "1" = "low_int", 
                                 "2" = "low_int", 
                                 "3" = "low_int", 
                                 "4" = "low_int", 
                                 "5" = "low_int", 
                                 "6" = "low_int", 
                                 "7" = "low_int", 
                                 "8" = "high", 
                                 "9" = "high", 
                                 "10" = "high", 
                                 "999" = "no_answer", 
                                 "777" = "no_answer")
  ) %>%
  mutate(t = factor(eventname), 
         sex = factor(sex),
         parent_edu = factor(parent_edu, ordered = T),
         ethn = factor(race_ethnicity, levels = c(1,2,3,4,5), labels = c("white",  "black",  "hispanic", "asian", "other")),
         age = as.numeric(interview_age),
         int = as.numeric(bpm_y_scr_internal_r), 
         int_t = as.numeric(bpm_y_scr_internal_t), 
         pub_female = factor(pds_y_ss_female_category_2, levels = c(1,2,3,4,5),labels = c("pre-puberty", "early-puberty", "mid-puberty", "late-puberty", "post-puberty")),
         pub_male = factor(pds_y_ss_male_cat_2, levels = c(1,2,3,4,5), labels = c("pre-puberty", "early-puberty", "mid-puberty", "late-puberty", "post-puberty")),
         site = factor(site_id_l),
         id = src_subject_id,
         family_id = rel_family_id, 
         friends_boys = as.numeric(resiliency5b_y),
         friends_girls = as.numeric(resiliency6b_y), 
         friends_nonbin = as.numeric(resiliency7b_y), 
         screentime1_p_hours = as.numeric(screentime1_p_hours),
         screentime1_p_minutes = as.numeric(screentime1_p_minutes), 
         screentime2_p_hours = as.numeric(screentime2_p_hours), 
         screentime2_p_minutes = as.numeric(screentime2_p_minutes), 
         sleep = factor(sleep, ordered = T),
         sleep = fct_rev(sleep),
         pa = as.numeric(physical_activity1_y), 
         par_psych = as.numeric(asr_scr_totprob_r),
         mat_age = as.numeric(devhx_3_p), 
  ) %>%
  select(id, t, parent_edu, sleep, int_t, int, pub_male, pub_female, 
         site, family_id, friends_boys, friends_girls, friends_nonbin, pa, par_psych,
         screentime2_p_minutes, screentime2_p_hours, screentime1_p_minutes, screentime1_p_hours, 
         age, sex, ethn, ple_y_ss_total_bad, mat_age, income)


# clean up from 777/refuse_answer ###
dd2 <- dd2 %>% replace_with_na(replace = list(parent_edu = c("refuse_answer"), 
                                              income = c("no_answer"))) %>%
                                 mutate(parent_edu = factor(parent_edu), 
                                        income = factor(income))
                                 
dd2$parent_edu <- factor(dd2$parent_edu, ordered = T) # re-code as factor (so it drops the empty level "refuse_answer")
dd2$income <- factor(dd2$income, ordered = T)


#####
# Fix site information - this was wrong for some children as specified in the ABCD release 4.0 notes
#####
# issues specified in release 4.0, document 3a. NDA 4.0 Changes and Known Issues (title demographics, subtitle incorrect site_id_l reported)
# this document is publicly available at https://nda.nih.gov/study.html?id=1299
# change incorrect values to correct ones as specified in the guide

# this part of the code is not publicly shared because it contains sensitive information
# an example of how this was done is below
# dd2[dd2$id == "PARTICIPANT-ID" & dd2$t == "RELEVANT-TIMEPOINT", "site"] <- "RELEVANT-SITE-NUMBER"


#######
# Reshape from long to wide
######

dd2 <- as_tibble(dd2)

df.wide <- dd2 %>% 
  pivot_wider(
    names_from = t, # the variable you want to use for the name of the vars
    id_cols = id, # variable identifying the ids  
    values_from = c(int_t, int, parent_edu, sleep,
                    pub_male, pub_female, site, family_id, friends_boys,
                    friends_girls, friends_nonbin, pa, par_psych, screentime2_p_minutes, 
                    screentime2_p_hours, screentime1_p_minutes, screentime1_p_hours, 
                    age, sex, ethn, ple_y_ss_total_bad, income, mat_age), # vars that we need to move to wide format 
    names_sep = "." # separator for the name of the vars 
  )



### keep only the needed columns ###
# delete all columns that have only NAs
# this happens because we have two timepoints (1y and 3y FU which have no data for most vars.)

dd2 <- df.wide[ , colSums(is.na(df.wide)) < nrow(df.wide)] 

# delete any cols not relevant
dd2$parent_edu.baseline_year_1_arm_1 <- NULL
dd2$age.1_year_follow_up_y_arm_1 <- NULL
dd2$sex.1_year_follow_up_y_arm_1 <- NULL
dd2$`ple_y_ss_total_bad.The event name for which the data was collected` <- NULL

names(dd2) <- gsub(x = names(dd2), pattern = "baseline_year_1_arm_1", replacement = "t1")
names(dd2) <- gsub(x = names(dd2), pattern = "3_year_follow_up_y_arm_1", replacement = "t2")
names(dd2) <- gsub(x = names(dd2), pattern = "1_year_follow_up_y_arm_1", replacement = "1yfu")
names(dd2) <- gsub(x = names(dd2), pattern = "30_month_follow_up_arm_1", replacement = "30m")
names(dd2) <- gsub(x = names(dd2), pattern = "6_month_follow_up_arm_1", replacement = "6m")

#### merge with prs data
prs <- prs[order(prs$IID), ]
dd2 <- merge(dd2, prs, by.x = "id", by.y = "IID", all = T)


#####
# Create measures 
#####
#### puberty overall ####
# loop to see whether any participant has data on both puberty for male and females
for(i in 1:nrow(dd2)){
  if(!is.na(dd2$pub_male.t1[i]) & !is.na(dd2$pub_female.t1[i])){
    stop("both values present!"[i])} 
} 

# create new puberty var 
dd2$puberty <- as.numeric(NA)

# fill in the puberty var with puberty info from males and females values 
# or leave it NA if there is neither data
for(i in 1:nrow(dd2)){ 
  if(!is.na(dd2$pub_male.t1[i])){
    dd2$puberty[i] <- dd2$pub_male.t1[i]
  }else if(!is.na(dd2$pub_female.t1[i])){
    dd2$puberty[i] <- dd2$pub_female.t1[i]
  }else if(is.na(dd2$pub_male.t1[i]) & is.na(dd2$pub_female.t1[i])){
    dd2$puberty[i] <- dd2$puberty[i]
  }else(stop("there might be an error!"[i]))
}


# make it a factor with 4 levels categorised from pre to post puberty, as done in the original var.  
# but with one cat. collapsed because too few kids in post-puberty (n = 22, 0.2% of the final sample)
dd2 <- dd2 %>% mutate(puberty = recode_factor(puberty,
                                              "1" = "pre-puberty", 
                                              "2" = "early-puberty", 
                                              "3" = "mid-puberty", 
                                              "4" = "late-post-puberty", 
                                              "5" = "late-post-puberty")) 

# make sure it's ordered factor 
dd2$puberty <- factor(dd2$puberty, ordered = T)


######
# exposure measures
######

#### friends measures #####
# NB there's no friend non binary at t1 so all NAs 
# sum up number of close friends who are boys and girls
dd2$friends_tot <- dd2$friends_boys.t1 + dd2$friends_girls.t1

#### overall screen time ####
# calculate total number of minutes per child on screen use 
# transform hours into minutes (screentime_p_hours*60) 
# and sum them with the other minutes collected (screentime_p_minutes)
# divide by 60 so you obtain the value in hours
# multiply by days of the week (i.e., 5 for the screentime1 variables which refer to the weekdays, 
# and 2 for the screentime2 variables which refer to the weekend days)

dd2 <- dd2 %>% mutate(tot_screen=(((screentime1_p_hours.t1*60+screentime1_p_minutes.t1))/60 *5 + 
                                   ((screentime2_p_hours.t1*60+screentime2_p_minutes.t1))/60 *2 
)/7 
)

# winsorize for extreme values
dd2$tot_screen_win <- DescTools::Winsorize(dd2$tot_screen, minval = NULL, maxval = 16,
          na.rm = T, type = 7)


dd2$tot_screen <- dd2$tot_screen_win

#######
# exclude vars you don't need
#######

dd2 <- as_tibble(dd2)

dd3 <- dd2 %>% 
  select(-screentime2_p_minutes.t1, -screentime2_p_hours.t1, 
         -screentime1_p_minutes.t1, -screentime1_p_hours.t1, -friends_boys.t1, -friends_girls.t1, 
         -pub_female.t1, -pub_male.t1)


#######
# Change vars where needed
######
# turn age into interpretable values
dd3$age.t1 <- dd3$age.t1/12

#######
# save data as one general dataset
#######
# inclusion criteria: has baseline data on internalizing 
# and on one of the modifiable factors (at least)
# exclusion criteria: clinically-relevant internalizing problems at baseline 
# one sibling/twin from each family 

general <- dd3 %>% 
  # have psych data at baseline
  subset(!is.na(int.6m)) %>% 
  # have data on either of the modifiable factors
  subset(!is.na(pa.t1) | !is.na(sleep.t1) | !is.na(friends_tot) | !is.na(tot_screen)) %>% 
  # not have clinical levels of internalizing problems
  subset(int_t.6m < 65) %>% 
  # keep one person from the same family
  subset(!duplicated(family_id.t1))


#### sensitvity analysis TTE ####
# here those with clinical levels are included

## inclusion and exclusion
sensitivity <- dd3 %>% 
  subset(!is.na(int.6m)) %>% 
  subset(!is.na(pa.t1) | !is.na(sleep.t1) | !is.na(friends_tot) | !is.na(tot_screen)) %>% 
  subset(!duplicated(family_id.t1))

  
## select relevant variables and rename
general_final <- general %>% select(id, int.6m, int.30m, int_t.6m,
                                    parent_edu.1yfu, sleep.t1, site.t1, 
                                    pa.t1, par_psych.t1, age.t1, sex.t1, 
                                    ethn.t1, puberty, friends_tot, tot_screen, 
                                    SCORESUM, ple_y_ss_total_bad.1yfu, mat_age.t1, income.1yfu, iq.t1
                                    )%>% 
  rename(int_t1 = int.6m, 
         int_t2 = int.30m, 
         int_tscore_t1 = int_t.6m, 
         parent_edu = parent_edu.1yfu, 
         sleep = sleep.t1, 
         site = site.t1, 
         pa = pa.t1, 
         par_psych = par_psych.t1, 
         age = age.t1,
         sex = sex.t1,
         ethn = ethn.t1,
         friends = friends_tot, 
         screen = tot_screen,
         prs = SCORESUM,
         trauma = ple_y_ss_total_bad.1yfu, 
         mat_age = mat_age.t1, 
         income = income.1yfu, 
         iq = iq.t1
         ) %>% mutate(
           trauma = as.numeric(trauma)
         )


# save datasets
saveRDS(general_final, paste0(indata, "modif_dep_abcd_youth_main_Jan2024.rds"))


### for sensitivity ####

sensitivity_final <- sensitivity %>% select(id, int.6m, int.30m, int_t.6m,
                                    parent_edu.1yfu, sleep.t1, site.t1, 
                                    pa.t1, par_psych.t1, age.t1, sex.t1, 
                                    ethn.t1, puberty, friends_tot, tot_screen, 
                                    SCORESUM, ple_y_ss_total_bad.1yfu, mat_age.t1, income.1yfu, iq.t1
)%>% 
  rename(int_t1 = int.6m, 
         int_t2 = int.30m, 
         int_tscore_t1 = int_t.6m, 
         parent_edu = parent_edu.1yfu, 
         sleep = sleep.t1, 
         site = site.t1, 
         pa = pa.t1, 
         par_psych = par_psych.t1, 
         age = age.t1,
         sex = sex.t1,
         ethn = ethn.t1,
         friends = friends_tot, 
         screen = tot_screen,
         prs = SCORESUM,
         trauma = ple_y_ss_total_bad.1yfu, 
         mat_age = mat_age.t1, 
         income = income.1yfu, 
         iq = iq.t1
  ) %>% mutate(
    trauma = as.numeric(trauma)
  )


# save dataset
saveRDS(sensitivity_final, paste0(indata, "modif_dep_abcd_youth_sensitivity_withClinical_Jan2024.rds"))



########
# Create table 1
########

label(dd$int_t1) <- "Internalizing problems (child report) baseline"
label(dd$int_t2) <- "Internalizing problems (child report) follow-up"
label(dd$sleep) <- "Sleep duration (hrs)"
label(dd$pa) <- "Physical activity (days)"
label(dd$friends) <- "Number of close friends"
label(dd$screen) <- "Total screen time (min)"
label(dd$parent_edu) <- "Parental education"
label(dd$par_psych) <- "Parental psychopathology"
label(dd$age) <- "Age"
label(dd$sex) <- "Sex"
label(dd$ethn) <- "Ethnicity"
label(dd$puberty) <- "Self-perceived pubertal stage"
label(dd$prs) <- "Genetic risk"
label(dd$trauma) <- "Negative life events"
label(dd$mat_age) <- "Maternal age at childbirth"
label(dd$income) <- "Household income"

table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + prs + trauma + income + mat_age, 
                         data = dd)

table1





#########
# Imputation 
#########
### Variables 

# specify the variables that can aid the prediction (i.e. help predict the missingness in other variables)
predictors_for_imputation <- c("parent_edu", "mat_age", "income", "par_psych", "sex", "puberty", "site")
impvars <- c("ethn", predictors_for_imputation)  

# dryrun mice (page 35 mice guide)
dd$id <- factor(dd$id)
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
visit2 <- c("parent_edu", "par_psych", "puberty", 'ethn', "mat_age", "income")

# run the imputation with m (number of datasets) and maxit (number of iterations)
## here 30 iterations and samples were set 
imp <- mice::mice(dd, 
                  m = 30, 
                  maxit = 30, 
                  seed = 2023, 
                  pred = pred, 
                  meth = meth, 
                  visit = visit2)

saveRDS(imp, paste0(indata, "modifiable_dep_youth_imp3030_Jan2024.rds"))

dd <- readRDS(paste0(indata, "modifiable_dep_youth_imp3030_Jan2024.rds"))

original_df <- readRDS(paste0(indata, "modif_dep_abcd_youth_main_Jan2024.rds"))

# merge imputed values with original values
dd2 <- merge_imputations(
  original_df,
  dd,
  ori = original_df
)


# select relevant variables & rename
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



dd4 <- dd3

# save dataset - universal prevention (all kids)
saveRDS(dd4, paste0(indata, "modifiable_dep_youth_imp3030_merged_Jan2024_FINAL_UNIVERSAL.rds"))


#########
# create subsamples
#########
# one issue when creating subsamples for other prevention strategies
# is that they include different individuals 
# therefore, some covariates will not have the same levels across datasets
# for site, several levels will not be present for subsamples
# for the race/ethnicity variable, just white individuals will be included
# in the genetic subsample
# to ensure that analyses can be concurrently run across all subsamples, 
# we need to residualize for these covariates instead of including them in the 
# covariate vector (see scripts 2, 3 and 4)

### preparation for the subsamples ###

# retain the original internalizing problems
dd4$int_t2_og <- dd4$int_t2
  
# residualize for site
m1 = lm(int_t2 ~ site, data = dd4, na.action = na.exclude)

dd4$int_t2 <- residuals(m1)

# reisdualize for ethn
m2 <- lm(int_t2 ~ ethn, data=dd4, na.action = na.exclude)
dd4$int_t2 <- residuals(m2)

# center internalizing problems after residualization, to ensure interpretability 
# of the results
dd4$int_t2_centered <- scale(dd4$int_t2)

# use the centered variable as main 
dd4$int_t2 <- dd4$int_t2_centered

# there are too few ppl in he puberty and sleep categories
# we need need to rearrange categories to meet positivity assumptions
dd4 <- dd4 %>% mutate(puberty = recode_factor(puberty, 
               "pre-puberty" = "pre&early", 
               "early-puberty" = "pre&early",
               "mid-puberty" = "mid&late", 
               "late-post-puberty" = "mid&late"), 
               sleep = recode_factor(sleep, 
                                     "less_7_hrs" = "less_8hrs", 
                                     "7_8_hrs" = "less_8hrs", 
                                     "8_9_hrs" = "8_9_hrs", 
                                     "9_11_hrs" = "9_11_hrs"))



### create the subsamples ###
# get decile information for genetic and stressful life events risk
dd4$deciles_g <- ntile(dd4$prs, 10)
dd4$deciles_t <- ntile(dd4$trauma, 10)

# individuals at high genetic risk, i.e., those in the top two 2 deciles
hGr <- dd4[dd4$deciles_g > 7 & !is.na(dd4$deciles_g),]

# individuals at high environmental risk (exposure to stressful life events), i.e., those in the top 2 deciles
hEr <- dd4[dd4$deciles_t > 7 & !is.na(dd4$deciles_t), ] 

# check how many individuals are in the top two deciles for both genetic and stressful life events
hGEr <- dd4[dd4$deciles_g > 7 & dd4$deciles_t > 7 
            & !is.na(dd4$deciles_g) & !is.na(dd4$deciles_t), ] 
# 334 ppl - this is too small of a sample to create a subcategory for analyses

### indicated prevention sample ###

# check what is the internalizing symptom score of those in the top 20% for internalizing symptoms at baseline
percentile_80 <- quantile(dd4$int_t1, 0.80, na.rm = T) # top 20% internalizing symptoms at baseline have a score of 3 or above

# select individuals with a score of 3 or above for the indicated prevention sample
indicated <- dd4[!is.na(dd4$int_t1) & dd4$int_t1 >= 3, ] 

# recode site as factor in all sample because some categories were dropped
hGr$site <- factor(hGr$site)
indicated$site <- factor(indicated$site)
hEr$site <- factor(hEr$site)

# save datasets
saveRDS(hGr, paste0(indata, "modifiable_dep_youth_imp3030_merged_Jan2024_FINAL_SELECTIVE_GENETICS.rds"))
saveRDS(hEr, paste0(indata, "modifiable_dep_youth_imp3030_merged_Jan2024_FINAL_SELECTIVE_ENVIRONMENT.rds"))
saveRDS(indicated, paste0(indata, "modifiable_dep_youth_imp3030_merged_Jan2024_FINAL_INDICATED.rds"))


# combine and save all datasets into one list
list_dd <- list(dd4, hGr, hEr, indicated)
names(list_dd) <- c("universal", "selective_genetic", "selective_trauma", "indicated")

save(hGr, hEr, indicated, dd4, list_dd, file = paste0(indata,"modifiable_dep_youth_alldata.RData"))
load("modifiable_dep_youth_alldata.RData")


#####
# Additional preparation for sensitivity analyses
#####

### sensitivity analysis for adj. for family conflict ####
# peer review request - data added post-hoc
family <- read.delim(paste0(indata, "crpbi01.txt"), header = T, na.strings=c("","NA")) # family conflict
  
family$crpbi_parent1_y <- as.numeric(family$crpbi_parent1_y)
family$crpbi_parent2_y <- as.numeric(family$crpbi_parent2_y)
family$crpbi_parent3_y <- as.numeric(family$crpbi_parent3_y)
family$crpbi_parent4_y <- as.numeric(family$crpbi_parent4_y) 
family$crpbi_parent5_y <- as.numeric(family$crpbi_parent5_y)

# create the composite family conflict score
family$composite_fam <- (family$crpbi_parent1_y + family$crpbi_parent2_y + family$crpbi_parent3_y + family$crpbi_parent4_y + family$crpbi_parent5_y)/5

# select for relevant timepoint (t1)
family2 <- family[family$eventname == "baseline_year_1_arm_1", ]

# create datasets for the sensitivity analysis
sens_adj_dd4 <- merge(dd4, family2, by.x = "id", by.y = "src_subject_id") 
sens_adj_hEr <- merge(hEr, family2, by.x = "id", by.y = "src_subject_id")
sens_adj_hGr <- merge(hGr, family2, by.x = "id", by.y = "src_subject_id")
sens_adj_indicated <- merge(indicated, family2, by.x = "id", by.y = "src_subject_id")

# combine data
datasets <- list(sens_adj_dd4, sens_adj_hGr, sens_adj_hEr, sens_adj_indicated)
names(datasets) <- c("universal", "selective_genetic", "selective_trauma", "indicated")

# save
save(sens_adj_dd4, sens_adj_hGr, sens_adj_hEr, sens_adj_indicated, file = paste0(indata,"modifiable_dep_youth_SENS_ADJ.RData"))


#### sensitivity analysis for those at low genetic risk and those with low baseline symptoms of internalizing ###

# select those at low genetic risk for depression
low_hGr <- dd4[!is.na(dd4$deciles_g) & dd4$deciles_g < 3, ] # selected < 3 so that there is a comparable N

# select those who have low baseline internalizing symptoms
low_indicated <- dd4[dd4$int_t1 < 2, ]

# combine this data with prior data
datasets <- list(dd4, hGr, hEr, indicated, low_hGr, low_indicated)
names(datasets) <- c("universal", "selective_genetic", "selective_trauma", "indicated","low genetic risk", "low int at baseline")

# save
save(dd4, hGr, hEr, indicated, low_hGr, low_indicated, datasets, file = paste0(indata,"modifiable_dep_youth_alldata_inclSens.RData"))


#######
# Descriptive stats for all samples
######

# universal prevention
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + prs + trauma + income + mat_age, 
                         data = dd4)



table1

# high genetic risk
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + prs + trauma + income + mat_age, 
                         data = hGr)

table1

# high exposure to stressful life events
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + prs + trauma + income + mat_age, 
                         data = hEr)

table1


# indicated risk
table1 <- table1::table1(~ int_t1 + int_t2 + 
                           sleep + pa + friends + screen + 
                           parent_edu + par_psych + age + sex + 
                           ethn + puberty + prs + trauma + income + mat_age, 
                         data = indicated)

table1


summary_stats <- lapply(list_dd, function(x) psych::describe(x)) 
write.csv(summary_stats, paste0(tab, "descriptive_stats_allsamples_modifiable_dep_youth.csv"))
                        
