
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



## Hand correction for 186XIA - 2 groups to merge with medians for duration of illness.
# median 10 (8-14.75), n=40 merge with  median 6 (5-8) n=48
# using https://smcgrath.shinyapps.io/estmeansd/
# calculated
mean_a <- 11.82
sd_a <- 6
ssize_a <- 40
mean_b <- 6.63
sd_b <- 2.47
ssize_b <- 48

mean_function(mean_a,mean_b,n1=ssize_a,n2=ssize_b)
sd_function(mean_a, mean_b, sd_a, sd_b, n1=ssize_a, n2=ssize_b)
