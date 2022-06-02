
library(datapasta)
library(meta)
library(dmetar)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(estmeansd)
library(robvis)
library("RColorBrewer")
library("clubSandwich")
library(esc)
library(naniar)

#====================
# temp data

dfm_test <- tibble::tribble(
  ~study_id, ~n_cont, ~age_mean_cont, ~age_sd_cont, ~male_n_cont, ~female_n_cont, ~n_treat, ~age_mean_treat, ~age_sd_treat, ~male_n_treat, ~female_n_treat, ~cognitive_name, ~cognitive_focus,                                                                       ~cognitive_process, ~cognitive_exclusions, ~cognitive_mean_cont, ~cognitive_sd_cont, ~cognitive_mean_treat, ~cognitive_sd_treat, ~cognitive_median_cont, ~cognitive_iqr1_cont, ~cognitive_iqr2_cont, ~cognitive_median_treat, ~cognitive_iqr1_treat, ~cognitive_iqr2_treat,
  "WEI184",    160L,           73.5,          4.8,          94L,            66L,       163L,            73.6,           5.2,           83L,             80L,          "MMSE",     "incidental",                                                                                       NA,                   "Y",                 28.7,                1.3,                  28.3,                 1.3,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "ALM009",    105L,          35.51,         7.35,          42L,            63L,     100L,           35.31,          6.95,           40L,             60L,          "MOCA",            "key",                                                                         "none described",                    NA,                27.12,               1.33,                 21.92,                4.13,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "APA016",     23L,           71.8,          6.9,          12L,            11L,      39L,            71.1,           6.1,            9L,             30L,          "MOCA",            "key",                                                                         "none described",                   "Y",                 27.9,                1.5,                  27.2,                 2.7,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BAR026",     30L,           48.7,         11.1,           0L,            30L,      30L,              52,           8.9,            0L,             30L,          "MMSE",            "key",                            "Does not describe, but plausible that Spanish versions used",                    NA,                29.73,               0.69,                 29.29,                1.42,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BOL034",    118L,           53.2,         10.4,          11L,            15L,     118L,            50.6,           9.5,           11L,             15L,          "MOCA",            "key",                                                                        "German language",                    NA,                   27,                  4,                  25.5,                 2.7,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BOR37",     18L,           49.5,  7.793285194,           0L,            18L,      18L,     50.38888889,   9.870567591,            0L,             18L,          "MOCA",            "key", "NB: scores are reported incorrectly in paper, however accessed raw scores from author.",                    NA,          27.61111111,        2.913233273,           25.38888889,          2.78945259,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BUC047",      8L,           69.9,          3.9,           3L,             5L,       8L,            74.5,           4.2,            4L,              4L,          "MMSE",     "incidental",                                                                                       NA,                   "Y",                 27.6,                1.2,                 28.25,                0.71,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "CAN053",     40L,          63.73,         9.55,          10L,            30L,      40L,           65.63,          8.59,           10L,             30L,          "MMSE",            "key",                     "Completed by one person - a psychiatrist (GP). In Italian versions",                    NA,                   NA,                 NA,                    NA,                  NA,                  25.25,                 23.5,                 26.2,                   23.35,                  21.1,                  25.2,
  "CHE062",     18L,          39.11,         9.99,           4L,            14L,      16L,           42.44,          8.65,            4L,             12L,          "MOCA",     "incidental",                                                                                       NA,                    NA,                26.89,               2.47,                 22.94,                5.37,                  25.25,                 23.5,                 26.2,                   23.35,                  21.1,                  25.2,
  "COE069",     45L,          40.76,        10.05,           NA,             NA,      45L,           41.07,          9.68,            NA,              NA,          "MMSE",            "key",                                                                         "No information",                    NA,                28.86,               1.44,                 27.86,                2.56,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "COR070",     27L,           56.7,         3.34,           NA,             NA,      31L,            56.9,         14.62,           19L,             12L,          "HVLT",               NA,                                                                                       NA,                   "Y",                27.41,               3.32,                 25.94,                3.15,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "DEM077",     23L,           43.3,          9.1,           1L,            22L,      23L,            47.6,            12,            1L,             22L,          "MMSE",     "incidental",                                                                                       NA,                    NA,                 24.5,                3.2,                  22.7,                 4.5,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA
)

# factorise
dfm_test$cognitive_name = as.factor(dfm_test$cognitive_name)
dfm_test $cognitive_focus = as.factor(dfm_test$cognitive_focus)
dfm_test $cognitive_exclusions = as.factor(dfm_test$cognitive_exclusions)
levels(dfm_test$cognitive_name)
#====================

# ==================
# read in and get variables coherent 

dfm <- read.xlsx("Data_extraction_sheet.xlsx")
glimpse(dfm)

# factorise
dfm2<- dfm %>% 
  mutate(across(c(
                  comp, 
                  matching_education,
                  matching_age,
                  cognitive_name,
                  cognitive_variant,
                  cognitive_focus,
                  cognitive_exclusion_any,
                  cognitive_exclusion_sametest,
                  pain_measure,
                  pain_measured_cont,
                  pain_measured_treat,
                  pain_medication_status,
                  anxiety_measure,
                  anx_diff,
                  depression_measure,
                  dep_diff
                           ), factor))


levels(dfm2$cognitive_name)

# =================================
# Combine study TER167 rows together
# Unlike other studies, the different comparisons provided are not differences of interest to us:
# Pain vs no-pain caregivers | Pain vs no-pain non-caregivers
# So best to combine

ter_only <- filter(dfm2,study_id == "TER167")

# create a single row version to manipulate
ter_combo <- ter_only %>% slice(1)
# replace n
ter_combo$n_cont = ter_only$n_cont[1]+ter_only$n_cont[2]
ter_combo$n_treat = ter_only$n_treat[1]+ter_only$n_treat[2]
ter_combo$female_n_cont = ter_only$female_n_cont[1] + ter_only$female_n_cont[2] 
ter_combo$male_n_cont = ter_only$male_n_cont[1] + ter_only$male_n_cont[2] 
ter_combo$n_treat = ter_only$n_treat[1]+ter_only$n_treat[2]
ter_combo$n_treat = ter_only$n_treat[1]+ter_only$n_treat[2]
ter_combo$female_n_treat = ter_only$female_n_treat[1] + ter_only$female_n_treat[2] 
ter_combo$male_n_treat = ter_only$male_n_treat[1] + ter_only$male_n_treat[2] 
# replace means

mean_function <- function(m1,m2,n1=size1,n2=size2){
  ((n1*m1) + (n2*m2))/(n1+n2)
}
sd_function <- function(m1,m2,s1,s2,n1=size1,n2=size2){
  sqrt(((n1-1)*s1^2 + (n2-1)*s2^2 + n1 * n2 / (n1 + n2) * (m1^2 + m2^2 - 2 * m1 * m2)) / (n1 + n2 -1))
 }



# Do controls first

size1 <- ter_only$n_cont[1]
size2 <- ter_only$n_cont[2] 
# means
ter_combo$age_mean_cont = mean_function(ter_only$age_mean_cont[1],ter_only$age_mean_cont[2])
ter_combo$cognitive_mean_cont = mean_function(ter_only$cognitive_mean_cont[1],ter_only$cognitive_mean_cont[2])
ter_combo$dep_mean_cont = mean_function(ter_only$dep_mean_cont[1],ter_only$dep_mean_cont[2])
# sds
ter_combo$age_sd_cont = sd_function(ter_only$age_mean_cont[1],ter_only$age_mean_cont[2], ter_only$age_sd_cont[1],ter_only$age_sd_cont[2])
ter_combo$cognitive_sd_cont = sd_function(ter_only$cognitive_mean_cont[1],ter_only$cognitive_mean_cont[2],ter_only$cognitive_sd_cont[1],ter_only$cognitive_sd_cont[2])
ter_combo$dep_sd_cont = sd_function(ter_only$dep_mean_cont[1],ter_only$dep_mean_cont[2],ter_only$dep_sd_cont[1],ter_only$dep_sd_cont[2])

# Now treatment groups

size1 <- ter_only$n_treat[1]
size2 <- ter_only$n_treat[2]
ter_combo$age_mean_treat = mean_function(ter_only$age_mean_treat[1],ter_only$age_mean_treat[2])
ter_combo$cognitive_mean_treat = mean_function(ter_only$cognitive_mean_treat[1],ter_only$cognitive_mean_treat[2])
ter_combo$dep_mean_treat = mean_function(ter_only$dep_mean_treat[1],ter_only$dep_mean_treat[2])
ter_combo$pain_mean_treat  = mean_function(ter_only$pain_mean_treat[1],ter_only$pain_mean_treat[2])
ter_combo$pain_duration_mean  = mean_function(ter_only$pain_duration_mean[1],ter_only$pain_duration_mean[2])


ter_combo$age_sd_treat = sd_function(ter_only$age_mean_treat[1],ter_only$age_mean_treat[2], ter_only$age_sd_treat[1],ter_only$age_sd_treat[2])
ter_combo$cognitive_sd_treat = sd_function(ter_only$cognitive_mean_treat[1],ter_only$cognitive_mean_treat[2],ter_only$cognitive_sd_treat[1],ter_only$cognitive_sd_treat[2])
ter_combo$dep_sd_treat = sd_function(ter_only$dep_mean_treat[1],ter_only$dep_mean_treat[2], ter_only$dep_sd_treat[1],ter_only$dep_sd_treat[2])
ter_combo$pain_sd_treat  = sd_function(ter_only$pain_mean_treat[1],ter_only$pain_mean_treat[2], ter_only$pain_sd_treat[1],ter_only$pain_sd_treat[2])
ter_combo$pain_duration_sd  = sd_function(ter_only$pain_duration_mean[1],ter_only$pain_duration_mean[2], ter_only$pain_duration_sd[1],ter_only$pain_duration_sd[2])


# replace others
ter_combo$sample_cont = "Older adults without chronic pain"
ter_combo$sample_treat = "Older adults with chronic pain"
ter_combo$unique_id = "TER167-COMB"

# check it has worked
temp <- bind_rows(ter_only,ter_combo)
  

dfm3 <- dfm2 %>%
  filter(!study_id == "TER167") %>%
  bind_rows(ter_combo)



#### TO HERE####


# ==================
# turn median scores into useable scores
# uses estmeansd library and Box–Cox method from McGrath 2020

# two quick functions - the key is to just extract the one returned value (eg $est.mean)
# (otherwise it doesn't play well with vectors/dfms)

        


unmed<- function(v1,m,v2,n, group) {
  set.seed(1) # for reproducibility so the randomiser isn't affected
  if (is.na(v2)){} else{
  res = bc.mean.sd(q1.val = v1,med.val = m,  q3.val  = v2, n  = n)
  if (group == "c" )
    list("est_mean_cont" = res$est.mean, "est_sd_cont" = res$est.sd)
  else if (group == "t" )
    list("est_mean_treat" = res$est.mean, "est_sd_treat" = res$est.sd)
  }
}

     #owl <-  select(dfm_c,dep_iqr1_treat, dep_median_treat,dep_iqr2_treat,n_treat)
       
       dfm_c <- dfm3 %>%
         rowwise() %>% 
           mutate(k = list(unmed(cognitive_iqr1_treat, cognitive_median_treat,cognitive_iqr2_treat,n_treat,"t"))) %>%
           unnest_wider(k) %>% 
         rowwise() %>% 
           mutate(k = list(unmed(cognitive_iqr1_cont, cognitive_median_cont,cognitive_iqr2_cont,n_cont,"c"))) %>%
           unnest_wider(k) %>%
          mutate(
           cognitive_mean_cont =  ifelse(!is.na(est_mean_cont),est_mean_cont,cognitive_mean_cont),
           cognitive_sd_cont = ifelse(!is.na(est_sd_cont),est_sd_cont,cognitive_sd_cont),
           cognitive_mean_treat = ifelse(!is.na(est_mean_treat), est_mean_treat,cognitive_mean_treat),
           cognitive_sd_treat = ifelse(!is.na(est_sd_treat),est_sd_treat,cognitive_sd_treat) 
          )  %>%
          select(!est_mean_treat:est_sd_cont) 


   
   dfm_cd <- dfm_c %>%
     rowwise() %>% 
     mutate(k = list(unmed(dep_iqr1_treat, dep_median_treat,dep_iqr2_treat,n_treat,"t"))) %>%
     unnest_wider(k)   %>%
      rowwise() %>% 
          mutate(k = list(unmed(dep_iqr1_cont, dep_median_cont,dep_iqr2_cont,n_cont,"c"))) %>%
          unnest_wider(k)  %>% 
      mutate(
          dep_mean_cont = ifelse(!is.na(est_mean_cont),est_mean_cont, dep_mean_cont),
          dep_sd_cont = ifelse(!is.na(est_sd_cont),est_sd_cont,dep_sd_cont),
          dep_mean_treat = ifelse(!is.na(est_mean_treat), est_mean_treat,dep_mean_treat),
          dep_sd_treat = ifelse(!is.na(est_sd_treat),est_sd_treat,dep_sd_treat)
           )%>%
       select(!est_mean_treat:est_sd_cont)

dfm_cda <- dfm_cd %>%
  rowwise() %>% 
  mutate(k = list(unmed(anx_iqr1_treat, anx_median_treat,anx_iqr2_treat,n_treat,"t"))) %>%
  unnest_wider(k) %>%
  rowwise() %>% 
  mutate(k = list(unmed(anx_iqr1_cont, anx_median_cont,anx_iqr2_cont,n_cont,"c"))) %>%
  unnest_wider(k) %>% 
  mutate(
         anx_mean_cont = ifelse(!is.na(est_mean_cont),est_mean_cont, anx_mean_cont),
      anx_sd_cont = ifelse(!is.na(est_sd_cont),est_sd_cont,anx_sd_cont),
      anx_mean_treat = ifelse(!is.na(est_mean_treat),est_mean_treat,anx_mean_treat),
      anx_sd_treat = ifelse(!is.na(est_sd_treat),est_sd_treat,anx_sd_treat)
    ) %>%
   select(!est_mean_treat:est_sd_cont)

# owl <-  select(dfm_cda,unique_id, anx_iqr1_cont, anx_median_cont,anx_iqr2_cont,n_cont,anx_mean_treat)


dfm_cdap <- dfm_cda %>% # NB no medians for controls here
  rowwise() %>% 
    mutate(k = list(unmed(pain_iqr1_treat, pain_median_treat,pain_iqr2_treat,n_treat,"t"))) %>%
    unnest_wider(k) %>%
  mutate(
            temp = "pain_medians",
            pain_mean_treat = ifelse(!is.na(est_mean_treat),est_mean_treat,pain_mean_treat),
            pain_sd_treat = ifelse(!is.na(est_sd_treat),est_sd_treat, pain_sd_treat)
      ) %>%
  select(!est_mean_treat:est_sd_treat)


# CHECKS

dfm3 %>%  count(is.na(cognitive_mean_treat)) 
dfm_cdap %>%  count(is.na(cognitive_mean_treat))

dfm3 %>%  count(is.na(dep_mean_treat)) 
dfm_cdap %>%  count(is.na(dep_mean_treat))

dfm3 %>%  count(is.na(pain_mean_treat))
dfm_cdap %>%  count(is.na(pain_mean_treat))
  
dfm3 %>%  count(is.na(anx_mean_treat))
dfm_cdap %>%  count(is.na(anx_mean_treat))  
  


  
  
  
  
  




  





  


  #--
  

  
  rowwise() %>% 
  mutate(k = list(unmed.m(pain_iqr1_treat,pain_median_treat,pain_iqr2_treat, n_treat,"p","t"))) %>%
  unnest_wider(k) %>% 
  
  
  
  rowwise() %>% 
  mutate(k = list(unmed.m(anx_iqr1_cont,anx_median_cont,anx_iqr2_cont, n_cont,"a","c"))) %>%
  unnest_wider(k) %>% 
  
  rowwise() %>% 
  mutate(k = list(unmed.m(anx_iqr1_treat,anx_median_treat,anx_iqr2_treat, n_treat,"a","t"))) %>%
  unnest_wider(k)
  
  
 

 # test - first with raw data and then with relevant row
  unmed.m(2,4,6,100)   
  
  dfm_cm <- dfm3 %>%
    filter(!is.na(cognitive_median_treat))

  
# Use functions to produce new variables. 
# NB this breaks when missing data, so just filtering non-missing and then recombine
  
  
  
  
  dfm <- dfm %>%
    mutate(es =  esc_mean_sd(grp1m = cognitive_mean_cont,   # mean of group 1
                             grp1sd = cognitive_sd_cont,  # standard error of group 1
                             grp1n = n_cont  ,    # sample in group 1
                             grp2m = cognitive_mean_treat ,    # mean of group 2
                             grp2sd = cognitive_sd_treat,  # standard error of group 2
                             grp2n = n_treat ,    # sample in group 2
                             es.type = "g")$es)
  
  
           ## Generate S2 summary data
           set.seed(1)
           n <- 100
           x <- stats::rlnorm(n, 2.5, 1)
           quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
           obs.mean <- mean(x)
           obs.sd <- stats::sd(x)
           
           ## Estimate the sample mean and standard deviation using the BC method
           res <- bc.mean.sd(q1.val = quants[1], med.val = quants[2],
                             q3.val = quants[3], n = n)
           print(res$est.mean)
           
  
  



 dfm_test_reduce <-   dfm2 %>%
   filter(!is.na(cognitive_median_treat)) %>%
   mutate(
     new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
     new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
     new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
     new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
   )    
 
 
 dfm_test_reduce2 <-   dfm_test %>%
   filter(!is.na(cognitive_median_treat)) %>%
   slice(1)  %>%
   mutate(
     new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
     new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
     new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
     new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
   )    
 
 

  
  
dfm_cm <- dfm %>%
  filter(!is.na(cognitive_median_treat)) %>%
        mutate(
                new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
                new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
                new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
                new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
      )     

dfm_depm <- dfm %>%
  filter(!is.na(cognitive_median_treat)) %>%
  mutate(
    new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
    new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
    new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
    new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
  )     





dfm <- full_join(dfm2,dfm)

# create a final variable for means and sds - if an original value missing it inserts the new one from step above
dfm3 <- dfm %>% 
  mutate(
          fin_cognitive_mean_treat = ifelse(!is.na(cognitive_mean_treat),cognitive_mean_treat, new_t_mean),
          fin_cognitive_sd_treat = ifelse(!is.na(cognitive_sd_treat),cognitive_sd_treat, new_t_sd),
          fin_cognitive_mean_cont = ifelse(!is.na(cognitive_mean_cont),cognitive_mean_cont, new_c_mean),
          fin_cognitive_sd_cont = ifelse(!is.na(cognitive_sd_cont),cognitive_sd_cont, new_c_sd),
          median_used = ifelse(!is.na(cognitive_mean_treat), "No", "Yes")
                    )

# check - should be nas and zeros
checks <- list(
                (dfm3$fin_cognitive_mean_treat - dfm3$cognitive_mean_treat ),
               (dfm3$fin_cognitive_sd_treat - dfm3$cognitive_sd_treat),
               (dfm3$fin_cognitive_mean_cont - dfm3$cognitive_mean_cont),
               (dfm3$fin_cognitive_sd_cont - dfm3$cognitive_sd_cont),
               (dfm3$fin_cognitive_mean_treat - dfm3$new_t_mean ),
               (dfm3$fin_cognitive_sd_treat - dfm3$new_t_sd),
               (dfm3$fin_cognitive_mean_cont - dfm3$new_c_mean),
               (dfm3$fin_cognitive_sd_cont - dfm3$new_c_sd)
                 )
checks

# if ok, finalise dataset:
dfm <- dfm3

rm(dfm2,dfm3,checks)

# =======================================
# META ANALYSIS
# =======================================


# ====================
# creating Hedge's g effect sizes for each model SDM with SE

dfm <- dfm %>%
  mutate(es =  esc_mean_sd(grp1m = cognitive_mean_cont,   # mean of group 1
                           grp1sd = cognitive_sd_cont,  # standard error of group 1
                           grp1n = n_cont  ,    # sample in group 1
                           grp2m = cognitive_mean_treat ,    # mean of group 2
                           grp2sd = cognitive_sd_treat,  # standard error of group 2
                           grp2n = n_treat ,    # sample in group 2
                           es.type = "g")$es,
         se = esc_mean_sd(grp1m = cognitive_mean_cont,   # mean of group 1
                          grp1sd = cognitive_sd_cont,  # standard error of group 1
                          grp1n = n_cont  ,    # sample in group 1
                          grp2m = cognitive_mean_treat ,    # mean of group 2
                          grp2sd = cognitive_sd_treat,  # standard error of group 2
                          grp2n = n_treat ,    # sample in group 2
                          es.type = "g")$se
                                            )

# ====================
# Adjusting for range restriction

# find U, ratio of unrestricted SD / study SD





# ====================================================
# Preparing data for CHE model

# constant sampling correlation assumption
rho <- 0.6 # initial guess

# constant sampling correlation working model
V <- with(dfm, 
          impute_covariance_matrix(vi = var.z,
                                   cluster = author,
                                   r = rho))



# ====================================================
# Meta-analysis

# Use metcont to pool results.
m_cont <- metacont(n.e = n_cont,
                   mean.e = fin_cognitive_mean_treat,
                   sd.e = fin_cognitive_sd_treat,
                   n.c = n_cont,
                   mean.c = fin_cognitive_mean_cont,
                   sd.c = fin_cognitive_sd_cont,
                   studlab = study_id,
                   data = dfm,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE, # if this doesn't work try comb.fixed. sometimes is having trouble with this
                   random = TRUE, #ditto comb.random
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Cognitive screens and chronic pain")
summary(m_cont)
print(m_cont)

colnames(dfm)

# Saving the Forest Plots
# This can be tricky
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest.html#saving-the-forest-plots
# you need to call the save function and then do the thing:

png(file = "forestplot.png", width = 2800, height = 2400, res = 300)

# forest plot
forest.meta(m_cont, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftcols = c("studlab", "cognitive_name"),
            rightcols = c("effect","ci"),
#            layout = "JAMA",
#            layout = "RevMan5",
            #leftlabs = c("studlab", "TE"),
            #rightlabs = c("studlab", "TE"),
            label.left = "pain ~ poorer cog",
            label.right = "pain ~ better cog"
            )
#and then deactivate the png-mode
dev.off()


# =======================================
# Funnel plot

# Firstly create a dataset with only those studies where the use of screen was central
 dfm_key <- dfm %>%
    filter(cognitive_focus == "key")

m.key <- metacont(n.e = n_cont,
                   mean.e = fin_cognitive_mean_treat,
                   sd.e = fin_cognitive_sd_treat,
                   n.c = n_cont,
                   mean.c = fin_cognitive_mean_cont,
                   sd.c = fin_cognitive_sd_cont,
                   studlab = study.id,
                   data = dfm_key,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE, # if this doesn't work try comb.fixed. sometimes is having trouble with this
                   random = TRUE, #ditto comb.random
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Cognitive screens part of study focus")




funnel.meta(m.key,
            xlim = c(-0.5, 2),
            studlab = TRUE)

# Add title
title("Funnel Plot (Chronic pain on cognitive screens - focused studies)")

# do a contour plot to point out which studies were more significant
col_contour = c("gray75", "gray85", "gray95")

funnel.meta(m.key, xlim = c(-0.5, 2),
            contour = c(0.9, 0.95, 0.99),
            col_contour = col_contour)
legend(x = 1.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col_contour)
title("Funnel Plot (Chronic pain on cognitive screens - focused studies)")




set.seed(123)
x1 <- rnorm(n = 20, mean = 28, sd = 3)
x2 <- rnorm(n = 20, mean = 35, sd = 0)

# Calculate values we need for the formulas
s1 <- sd(x1)
s2 <- sd(x2)
n1 <- 20
n2 <- 20

# Calculate the mean difference
MD <- mean(x1) - mean(x2)
MD

s_pooled <- sqrt(
  (((n1-1)*s1^2) + ((n2-1)*s2^2))/
    ((n1-1)+(n2-1))
)

# Calculate the standard error
se <- s_pooled*sqrt((1/n1)+(1/n2))
se
