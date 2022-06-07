# SBEP_staff_script_q3

source("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\scripts\\vi_yi.R")

# ===============================
# Models

# simple random-effects model
simple <- rma(yi, vi, data=dfm_mod)
simple

forest(simple, slab = dfm_mod$author_final)

# ====================================================
# Preparing data for CHE model

# # constant sampling correlation assumption
# rho <- 0.6 # initial guess
# # constant sampling correlation working model
# V <- with(dfm_mod, 
#            impute_covariance_matrix(vi = vi,
#                                     cluster = study_id,
#                                     r = rho))
## Deprecated for fuller covariance matrix below



# or collect rho in here:
R <- matrix(1, nrow=5, ncol=5)
diag(R) <- 1
R[1,2]<- R[2,1] <- 0.87 # MMSE and MoCA - Nasreddine paper
R[1,3]<- R[3,1] <- 0.77 # MMSE & TYM - van de Zande paper
R[1,4]<- R[4,1] <- 0.877 # MMSE & ACE (III) - Guiu paper
R[1,5]<- R[5,1] <- 0.75 # MMSE & HVLT GUESS
R[2,3]<- R[3,2] <- 0.75 # MoCA & TYM GUESS
R[2,4]<- R[4,2] <- 0.679 # MoCA & ACE    Guiu paper
R[2,5]<- R[5,2] <- 0.75  # MoCA & HVLT GUESS
R[3,4]<- R[4,3] <- 0.75  # TYM & ACE GUESS
R[3,5]<- R[5,3] <- 0.75  # TYM & HVLT GUESS
R[4,5]<- R[5,4] <- 0.75  # ACE & HVLT GUESS
rownames(R) <- colnames(R) <- c("MMSE","MoCA","TYM","ACE","HVLT")


V <- vcalc(vi=viN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
           obs=cognitive_name, rho=R, data=dfm_mod)

# check that the 5 studies are covered - note that those with only screen differences map perfectly onto the 
cov2cor(V[dfm_mod$study_num == 89, dfm_mod$study_num == 89])
cov2cor(V[dfm_mod$study_num == 121, dfm_mod$study_num == 121]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 134, dfm_mod$study_num == 134]) # MMSE and TYM for part of it
cov2cor(V[dfm_mod$study_num == 182, dfm_mod$study_num == 182]) # MMSE and MOCA



# Raw CHE model:

che.model <- rma.mv(yi ~ 1,
                    V = V,
                    random = ~ 1 | study_num/unique_id,
                    data = dfm_mod,
                    sparse = TRUE)

conf_int(che.model, 
         vcov = "CR2")

# CHE model using grand N

che.model_N <- rma.mv(yiN ~ 1,
                    V = V,
                    random = ~ 1 | study_num/unique_id,
                    data = dfm_mod,
                    sparse = TRUE)

# The function computes predicted values, corresponding standard errors, confidence intervals, and prediction intervals 
# This produces the key outcomes.
study_outputs <- predict(che.model_N, digits=2)



# use robust inference methods based on this model
robust(che.model_N, cluster=study_id, clubSandwich=TRUE)

# check residuals
rstudent(che.model_N)

# ============================================================

# adding more yi and vi for other measures

multismd <- dfm_mod %>%
  # filter(pain_measured_cont=="yes") %>%
  mutate(yi_cog = yi,
         vi_cog = vi) %>%
  select(!c(yi,vi))

multismd  <- escalc(measure="SMD",
                    n1i = n_cont,
                    m1i = pain_mean_cont, 
                    sd1i = pain_sd_cont, 
                    n2i = n_treat ,
                    m2i = pain_mean_treat ,    # mean of group 2
                    sd2i = pain_sd_treat,  # standard error of group 2
                    data = multismd,
                    vtype="AV") # this incorporates sample-size weighted average of Hedges'g values.
multismd <- multismd %>%
  mutate(yi_pain = yi,
         vi_pain = vi) %>%
  select(!c(yi,vi))

multismd  <- escalc(measure="SMD",
                    n1i = n_cont,
                    m1i = age_mean_cont, 
                    sd1i = age_sd_cont, 
                    n2i = n_treat ,
                    m2i = age_mean_treat ,    # mean of group 2
                    sd2i = age_sd_treat,  # standard error of group 2
                    data = multismd,
                    vtype="AV") # this incorporates sample-size weighted average of Hedges'g values.
multismd <- multismd %>%
  mutate(yi_age = yi,
         vi_age = vi) %>%
  select(!c(yi,vi))

## ======


multismd %>% select(yi_pain, pain_mean_cont,pain_mean_treat)

che.model_pain <- rma.mv(yi_cog ~ 1 + yi_pain,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = multismd,
                     sparse = TRUE)



# can also add a potential covariate
che.model2 <- rma.mv(yi ~ 1 + cognitive_name,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = dfm_mod,
                     sparse = TRUE)

# alternative - pain group
che.model_age <- rma.mv(yi ~ 1 + age_mean_cont,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = dfm_mod,
                     sparse = TRUE)

dfm_mod_sift <- dfm_mod %>%
  filter(yi<3)
siftV <- vcalc(vi=viN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
               obs=cognitive_name, rho=R, data=dfm_mod_sift)


che.model_age_sift <- rma.mv(yi ~ 1 + age_mean_cont,
                        V = siftV,
                        random = ~ 1 | study_num/unique_id,
                        data = dfm_mod_sift,
                        sparse = TRUE)

## ====
# Models filtered by screen

# MMSE model
dfm_mmse <- dfm_mod %>%
  filter(cognitive_name =="MMSE") %>%
  mutate(author_final = str_trunc(author_final,19, "right"))

mmseV <- vcalc(vi=viN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
               obs=cognitive_name, rho=R, data=dfm_mmse)

che.model_mmse <- rma.mv(yiN ~ 1,
                         V = mmseV,
                         random = ~ 1 | study_num/unique_id,
                         data = dfm_mmse,
                         sparse = TRUE)
# Other models
dfm_screens <- dfm_mod %>%
  filter(cognitive_name !="MMSE")

screenV <- vcalc(vi=viN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
                 obs=cognitive_name, rho=R, data=dfm_screens)

che.model_screens <- rma.mv(yiN ~ 1,
                            V = screenV,
                            random = ~ 1 | study_num/unique_id,
                            data = dfm_screens,
                            sparse = TRUE)

