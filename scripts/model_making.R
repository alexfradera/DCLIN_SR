
# ===============================
# Models

# simple random-effects model
res <- rma(yi, vi, data=dfm_mod)
res

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
cov2cor(V[dfm_mod$study_num == 192, dfm_mod$study_num == 192]) # MMSE and MOCA


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


# can also add a potential covariate
che.model2 <- rma.mv(yi ~ 1 + cognitive_name,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = dfm_mod,
                     sparse = TRUE)

# alternative - pain group
che.model3 <- rma.mv(yi ~ 1 + sample_treat_cat,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = dfm_mod,
                     sparse = TRUE)
