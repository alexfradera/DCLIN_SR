# SBEP_staff_script_q3

source("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\scripts\\vi_yi.R")

# ===============================
# Models
# ===============================
qual_tog <- quality_scores_vals %>%
  select(unique_id, toggle)
  dfm_mod <- left_join(dfm_mod,qual_tog, by="unique_id")


# ===============================
# FULL MODEL
# 1 simple random-effects model (no accounting for non-independence)
simple <- rma(yi, vi, data=dfm_mod)
simple

# with HSK
simple2 <- rma(yi, vi, data=dfm_mod, method = "HSk")
simple2

forest(simple, slab = dfm_mod$author_final)



# ===============================
# high quality comparisons only

dfm_hi <- dfm_mod %>%
  filter(toggle==1)


simple_hi <- rma(yi, vi, data=dfm_hi)
simple_hi

# ====================================================
# 2 Preparing data for CHE model


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

# create variance-covariance matrix (diagonal contains variances)
V <- vcalc(vi=viN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
           obs=cognitive_name, rho=R, data=dfm_mod)
Vhi <- vcalc(vi=viN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
             obs=cognitive_name, rho=R, data=dfm_hi)






Vs <- blsplit(V, dfm_mod$study_num) # this works but can only subset some of the lower value elements for some reason

# check that the 5 studies are covered - note that those with only screen differences map perfectly onto the 
cov2cor(V[dfm_mod$study_num == 89, dfm_mod$study_num == 89])
cov2cor(V[dfm_mod$study_num == 121, dfm_mod$study_num == 121]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 134, dfm_mod$study_num == 134]) # MMSE and TYM for part of it
cov2cor(V[dfm_mod$study_num == 182, dfm_mod$study_num == 182]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 993, dfm_mod$study_num == 993]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 994, dfm_mod$study_num == 994]) # MMSE 2 groups


# 3. start running Correlated and Hierarchical Effects models:
# 3a Raw CHE model:

che.model <- rma.mv(yi, V, 
                    data = dfm_mod, 
                    random = ~ 1 | study_num/unique_id, 
                    test ='t', 
                    method = "REML",
                    sparse = TRUE) #original



#che.modelX <- rma.mv(yi, V, mods = ~ cognitive_name - 1, random = ~ 1 | study_num/unique_id, struct = "UN", data = dfm_mod, sparse = TRUE)
#che.modelY <-rma.mv(yi, V, mods = ~ cognitive_name - 1, random = ~  study_num | unique_id, struct = "UN", data = dfm_mod, sparse = TRUE)

i2 <- var.comp(che.model)
i2
i2$plot


# W <- solve(V)
#  X <- model.matrix(che.modelY)
#  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
#  100 * che.modelY$tau2 / (che.modelY$tau2 + (che.modelY$k-che.modelY$p)/sum(diag(P)))

# This produces the key outcomes.
study_outputs <- predict(che.model, digits=2)
conf_int(che.model, vcov = "CR2")


# check residuals
resids_che <- as.tibble(rstudent(che.model)$resid)
ggplot(data = resids_che, mapping = aes(x=1, y=value)) + geom_violin() +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank()) + ylab("residuals - CHE")


# raw calculate heterogeneity
res.R <- che.modelX
res.F <- che.modelY

c(100 * (vcov(res.R)[1,1] - vcov(res.F)[1,1]) / vcov(res.R)[1,1],
  100 * (vcov(res.R)[2,2] - vcov(res.F)[2,2]) / vcov(res.R)[2,2])

W <- solve(V)
> X <- model.matrix(che.modelX)
> P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
> 100 * res$tau2 / (res$tau2 + (res$k-res$p)/sum(diag(P)))



# 4 use CHE model with robust inference methods


sav <- robust(che.model_N, cluster=study_id, clubSandwich=TRUE)
resids_sav <- as.tibble(residuals(sav))
ggplot(data = resids_sav, mapping = aes(x=1, y=value)) + geom_violin() +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank()) + ylab("residuals - Sandwich")







# ============================================================

# adding more yi and vi for other measures

multismd <- dfm_mod %>%
  # filter(pain_measured_cont=="yes") %>%
  mutate(yi_cog = yi,
         vi_cog = vi,
         yi_cogN = yiN,
         vi_cogN = viN) %>%
  select(!c(yi,vi,yiN,viN))

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

``` {r, age_rels}
# age diffs
#multismd  %>%
#  ggplot(mapping = aes(x=yi_age, y=yi_cog)) + geom_point() + stat_smooth(method=lm)

# age patients
multismd  %>%
  ggplot(mapping = aes(x=age_mean_treat, y=yi_cog)) + geom_point() + stat_smooth(method=lm)
```

``` {r, tempy}
# just using rough z-scoresy for all available pain group pain scores
#dfm_mod %>%
#  ggplot(mapping = aes(x=pain_mean_treat/pain_sd_treat, y=yi)) + geom_point() + stat_smooth(method=lm)
```

```{r, painstuff}
pain_treats <- dfm_mod %>%
  filter(pain_measured_treat=="yes") %>%
  summarise(n())
pain_conts <- dfm_mod %>%
  filter(pain_measured_cont=="yes") %>%
  summarise(n())
```

Although pain intensity summary data were published for pain group for  `r pain_treats[[1]]` comparisons, control data was only supplied in `r pain_conts[[1]]` instances. Within the subset for which data was available standardised mean differences for pain intensity were computed and the relationship between the two are plotted below. 

``` {r, multismd_checks, echo = F}

# pain diffs
multismd  %>%
  ggplot(mapping = aes(x=yi_pain, y=yi_cog)) + geom_point() + stat_smooth(method=lm)
```

Running a smaller model with pain standardised mean differences as a moderator of the effect found a significant relationship between greater pain in the pain group and the size of the average effect: \beta = `r round(che.model_pain$beta[2],3)`, se = `r round(che.model_pain$se[2],3)`, p = `r round(che.model_pain$pval[2],4)`.

Using patient age as as a moderator of the effect found a significant relationship where younger pain experiencing patients were likely to show a larger effect: \beta = `r round(che.model_age$beta[2],3)`, se = `r round(che.model_age$se[2],3)`, p = `r round(che.model_age$pval[2],3)`. [*NB this is influenced again by the fact the "big 3" studies were all for ages below 45. When removed, the effects remain but are attenuated....*]