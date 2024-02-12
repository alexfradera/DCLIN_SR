# SBEP_staff_script_q3

source(here('scripts', 'qa_aid2.R')) 


 
 
# ===============================
# Models
# ===============================

dfm_mod <- dfm_mod |> arrange(author_final)

# high quality comparisons only
qual_tog <- quality_scores_vals %>%
  select(unique_id, toggle)
dfm_mod <- left_join(dfm_mod,qual_tog, by="unique_id") 
dfm_mod <-dfm_mod |>
  mutate(risk = case_when(toggle ==1 ~ "Low risk",
                   toggle ==0 ~ "High risk") )
dfm_mod <- dfm_mod |> arrange(unique_id) # arrange so models and forest plots follow an order

dfm_hi <- dfm_mod %>%
  filter(toggle==1)
  
dfm_lo <- dfm_mod %>%
  filter(toggle==0)




### SCREENS  ###
# only mmse 
  dfm_mmse_full <- dfm_mod %>%
    filter(cognitive_name =="MMSE")

  table(dfm_mmse_full$toggle) # no of hi-qual available
  
  dfm_mmse <- filter(dfm_mmse_full, toggle==1)  
  
# only moca
  dfm_moca_full <- dfm_mod %>%
    filter(cognitive_name =="MoCA") 
  
  table(dfm_moca_full$toggle) # no of hi-qual available
  
  dfm_moca <- filter(dfm_moca_full, toggle==1)    

### Patient groups ###
  table(dfm_mod$sample_treat_cat)
  
  dfm_maincons <- dfm_mod %>%
    filter(sample_treat_cat %in% c("Arthritis","Fibromyalgia","Headache","MSK"))
  
  dfm_arthritis_full <-dfm_mod %>%
    filter(sample_treat_cat %in% c("Arthritis"))
  
  table(dfm_arthritis_full$toggle) # no of hi-qual available
  dfm_arthritis <- dfm_arthritis_full %>%
    filter(toggle==1)
  
  dfm_fm_full <-dfm_mod %>%
    filter(sample_treat_cat %in% c("Fibromyalgia"))
  table(dfm_fm_full$toggle)
  #dfm_fm <- dfm_fm_full %>%  # NB too few for analysis - step not taken
  #  filter(toggle==1)
  
  dfm_msk_full <-dfm_mod %>%
    filter(sample_treat_cat %in% c("MSK"))
  table(dfm_msk_full$toggle)
  #dfm_fm <- dfm_fm_full %>% # NB too few for analysis - step not taken
  #  filter(toggle==1)
  
  dfm_head_full <-dfm_mod %>%
    filter(sample_treat_cat %in% c("Headache"))
  table(dfm_head_full$toggle)
  #dfm_head <- dfm_head_full %>% # NB too few for analysis - step not taken
  #  filter(toggle==1)
  
## depression-free
  depcut<- filter(aim2,q6_mood_confound == "comparable")
  dfm_depfree_full <- semi_join(dfm_mod,depcut, by="unique_id")
  dfm_depfree<- filter(dfm_depfree_full, toggle==1)     

  
  
  dfm_mod <- dfm_mod |>
    mutate(low_age_treat = age_mean_treat - age_sd_treat,
           high_age_treat = age_mean_treat + age_sd_treat)
  dfm_mod |>
    select(low_age_treat) |>
    arrange(low_age_treat)
  dfm_mod |>
    select(high_age_treat, low_age_treat) |>
    arrange(high_age_treat)
  dfm_mod <- dfm_mod |>
    mutate(age_bucket = case_when(high_age_treat <55 ~ 1,
                                  low_age_treat >65 ~ 3,
                                  low_age_treat >55 ~ 2
    ))
  
  dfm_agers <- dfm_mod |>
    filter(!is.na(age_bucket))
  
  dfm_young <- dfm_agers|>
    filter((age_bucket==1))
  
  dfm_old  <- dfm_agers|>
    filter((age_bucket==2|age_bucket==3))
  
  dfm_well_old <- dfm_agers|>
    filter((age_bucket==3))
  
 
  
  
  
# tidy
  rm(aim1,df,df2,dfm, dfm_c,dfm_cd,dfm_cda,dfm_cdap,dfm_cdapa,dfm_e1,dfm_e2,dfm3)
  
# Collect rho in here:
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
quick_v <- function(dataset){
   vcalc(vi=vi, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
        obs=cognitive_name, rho=R, data=dataset) }

V <- quick_v(dfm_mod)
Vhi <- quick_v(dfm_hi)
Vlo <- quick_v(dfm_lo)
Vmms <- quick_v(dfm_mmse)
Vmms_f <- quick_v(dfm_mmse_full)
Varth <- quick_v(dfm_arthritis)
Varth_f <- quick_v(dfm_arthritis_full)
Vfm  <- quick_v(dfm_fm_full)
Vmsk <- quick_v(dfm_msk_full)
Vhead <- quick_v(dfm_head_full)
# Vmoc <- quick_v(dfm_moca) unneeded, no clusters
Vmaincons <- quick_v(dfm_maincons)
# Vcomp <- quick_v(dfm_test)

Vs <- blsplit(V, dfm_mod$study_num) # this works but can only subset some of the lower value elements for some reason

# QA step: check  studies are covered - note that those with only screen differences map perfectly onto the 
cov2cor(V[dfm_mod$study_num == 89, dfm_mod$study_num == 89])
cov2cor(V[dfm_mod$study_num == 121, dfm_mod$study_num == 121]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 134, dfm_mod$study_num == 134]) # MMSE and TYM for part of it
cov2cor(V[dfm_mod$study_num == 182, dfm_mod$study_num == 182]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 993, dfm_mod$study_num == 993]) # MMSE and MOCA
cov2cor(V[dfm_mod$study_num == 994, dfm_mod$study_num == 994]) # MMSE 2 groups


# 3. start running Models, both
# 3a: simple random-effects model (no accounting for non-independence)
# 3b Correlated and Hierarchical Effects models (CHE) where clusters exist

# CHE function
rmvee <- function(dataset,vmat, modifier=""){
  if (is.na(modifier)){
                  rma.mv(yi, V= vmat, 
                     data = dataset, 
                     random = ~ 1 | study_num/unique_id, 
                     test ='t', 
                     method = "REML",
                     mod = modifier,
                       sparse = TRUE) 
                    }
              else {
                rma.mv(yi, V = vmat, 
                       data = dataset, 
                       random = ~ 1 | study_num/unique_id, 
                       test ='t', 
                       method = "REML",
                       sparse = TRUE)
                     }
}


# Main model - all data
## Simple RE model:
simple.model <- rma(yi, vi, data=dfm_mod)
confint(simple.model)
predict(simple.model)
## CHE model
che.model <- rmvee(dfm_mod,vmat=V)
che.ci <- confint(che.model)
model.i2 <- var.comp(che.model)
main_model_output <- predict(che.model)
## CHE + RVE/Sandwich
rve.model <- robust(che.model, cluster=study_id, clubSandwich=TRUE) # substantially same as che.model

# some manual sense-checking of the I2 factors
res <- rma.mv(yi, vi, random = ~ factor(unique_id) | study_num, data=dfm_mod)
res.ci <- confint(res)
W <- diag(1/dfm_mod$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$tau2 / (res$tau2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * res.ci[[1]]$random[1,2:3] / (res.ci[[1]]$random[1,2:3] + (res$k-res$p)/sum(diag(P)))

# High quality and low quality
## Simple RE model:
simple.hi <- rma(yi, vi, data=dfm_hi)
confint(simple.hi)
## CHE
che.hi <- rmvee(dfm_hi,vmat=Vhi)
hi.i2 <- var.comp(che.hi)

simple.lo <- rma(yi, vi, data=dfm_lo)
confint(simple.lo)

che.lo <- rmvee(dfm_lo,vmat=Vlo)
hi.i2 <- var.comp(che.lo)

# By cognitive screen type
# MMSE - hiqual only
simple.mmse <- rma(yi, vi, data=dfm_mmse)
che.mmse  <- rmvee(dfm_mmse,vmat=Vmms)
mmse.i2  <- var.comp(che.mmse)


#MMSE - all studies
simple.mmse_full <- rma(yi, vi, data=dfm_mmse_full)
che.mmse_full  <- rmvee(dfm_mmse_full,vmat=Vmms_f)
mmse_full.i2  <- var.comp(che.mmse_full)

# MoCA  No need for CHE - no clusters
simple.moca <- rma(yi, vi, data=dfm_moca_full)


# Using goups as a variable - ended up just breaking into subgroups
simple.groups<- rma(yi, vi, data=dfm_maincons, mods= ~ sample_treat_cat -1)
che.groups <-    rma.mv(yi, V= Vmaincons,
                      data = dfm_maincons,
                       random = ~ 1 | study_num/unique_id,
                       test ='t',
                       method = "REML",
                       mod = ~ sample_treat_cat -1,
                       sparse = TRUE)
groups.i2  <- var.comp(che.groups)

# Arthritis - hiqual only
simple.arth <- rma(yi,vi, data=dfm_arthritis)
che.arth <- rmvee(dfm_arthritis,vmat=Varth)

# Arthritis - all studies
simple.arth_full <- rma(yi,vi, data=dfm_arthritis_full)
che.arth_full <- rmvee(dfm_arthritis_full,vmat=Varth_f)

# Fibromyalgia
simple.fm <- rma(yi,vi, data=dfm_fm_full)
che.fm <- rmvee(dfm_fm_full,vmat=Vfm)

#MSK
simple.msk <- rma(yi,vi, data=dfm_msk_full)
che.msk <- rmvee(dfm_msk_full,vmat=Vmsk)

# Headache
simple.head <- rma(yi,vi, data=dfm_head_full)
che.head <- rmvee(dfm_head_full,vmat=Vhead)

# Depression (no clusters)
simple.dep <- rma(yi, vi, data=dfm_depfree)
simple.dep_full <- rma(yi, vi, data=dfm_depfree_full)


# age
simple.young <-rma(yi, vi, data=dfm_young, method = "REML")

young_num <- sum(dfm_young$n_treat)
sum(dfm_young$age_mean_treat * dfm_young$n_treat)/young_num       

simple.old <-rma(yi, vi, data=dfm_old, method = "REML")

old_num <- sum(dfm_old$n_treat)
sum(dfm_old$age_mean_treat * dfm_old$n_treat)/old_num  

simple.well.old <-rma(yi, vi, data=dfm_well_old, method = "REML")

older_num <- sum(dfm_well_old$n_treat)
sum(dfm_well_old$age_mean_treat * dfm_well_old$n_treat)/older_num

# ===========================
# Creating table with data
# note that we create rows including multiple (conditions*high_qual) that will be filtered out for report
# these will be included in appendix.

che_names <- c("Total dataset", "Low risk of bias", "High risk of bias", "MMSE","Arthritis","Fibromyalgia", "MSK", "Headache","MMSE_high_qual","Arthritis_high_qual")
che_modellist <- vector(mode = "list", length = 10)
  che_modellist[[1]] <- che.model
  che_modellist[[2]] <- che.hi
  che_modellist[[3]] <- che.lo
  che_modellist[[4]] <- che.mmse_full
  che_modellist[[5]] <- che.arth_full
  che_modellist[[6]] <- che.fm
  che_modellist[[7]] <- che.msk
  che_modellist[[8]] <- che.head
  che_modellist[[9]] <- che.mmse
  che_modellist[[10]] <- che.arth
  
  
che_tablelist <- vector(mode = "list", length = 10)

for (i in 1:10){
modelnow <-che_modellist[[i]]
names <- che_names
study_outputs <- predict(modelnow, digits=2)
raw_overall <- round(as_tibble(study_outputs), digits=3)
presentable <- raw_overall %>%
  transmute(
    type = names[i],
    method = "CHE",
    n = modelnow$k,
    justes =  pred,
    conf = paste0(pred, " [", ci.lb, " - ",  ci.ub,"]"),
    pval = ifelse (modelnow$pval < .001,"< .001", as.character(round(modelnow$pval,3))),
    i2 = round(var.comp(modelnow)$total, digits=2),
    i2_2 = var.comp(modelnow)$results[2,2],
    i2_3 = var.comp(modelnow)$results[3,2])
che_tablelist[[i]]<- presentable
rm(presentable,study_outputs,raw_overall)}   
#modeloutputs[1] <- presentable
#rm(study_outputs,raw_overall, presentable)
bind_rows(che_tablelist)


simp_names <-c(che_names[1:4], "MoCA", che_names[5:8],  "Matched depression", "MMSE_high_qual","Arthritis_high_qual","Depression_high_qual", "Young","Old","Well_old")  #NB rearranged to align MoCA with MMSE
simp_modellist <- vector(mode = "list", length = 16)
simp_tablelist <- vector(mode = "list", length = 16)
simp_modellist[[1]] <- simple.model
simp_modellist[[2]] <- simple.hi
simp_modellist[[3]]<- simple.lo
simp_modellist[[4]] <- simple.mmse_full
simp_modellist[[5]] <-  simple.moca
simp_modellist[[6]] <-  simple.arth_full
simp_modellist[[7]] <-  simple.fm
simp_modellist[[8]] <-  simple.msk
simp_modellist[[9]] <-  simple.head
simp_modellist[[10]] <-  simple.dep_full
simp_modellist[[11]] <-  simple.mmse
simp_modellist[[12]] <-  simple.arth
simp_modellist[[13]] <-  simple.dep
simp_modellist[[14]] <- simple.young
simp_modellist[[15]] <- simple.old
simp_modellist[[16]] <- simple.well.old


for (i in 1:16){
modelnow <-simp_modellist[[i]]
names <- simp_names
study_outputs <- predict(modelnow, digits=2)
raw_overall <- round(as_tibble(study_outputs), digits=3)
presentable <- raw_overall %>%
  transmute(
    type = names[i],
    method = "RE",
    justes =  pred,
    n = modelnow$k,
    conf = paste0(pred, " [", ci.lb, " - ",  ci.ub,"]"),
    pval = ifelse (modelnow$pval < .001,"< .001", as.character(round(modelnow$pval,3))),
    i2 = round(modelnow$I2),
    i2_2 = "",
    i2_3 = "")
simp_tablelist[[i]]<- presentable 
rm(presentable,study_outputs,raw_overall)}  
bind_rows(simp_tablelist)

total_table <- bind_rows(che_tablelist,simp_tablelist)

ap_total_table <- total_table %>% filter(grepl('_qual',type))
ap_total_table <- ap_total_table %>% arrange(desc(type), method)
ap_total_table <- as_tibble(ap_total_table) 


total_table <- total_table %>% filter(!grepl('_qual',type))

total_table <- total_table %>% 
          #  arrange(desc(type)) %>%
          #  mutate(neworder =c(1,2,9,18,16,17,7,8,3,4,5,6,14,15,12,13,10,11,19,20,21)) %>%
         mutate(neworder =c(1,3,5,7,10,12,16,14,2,4,6,8,9,11,13,17,15,18,19,20,21)) %>%
            arrange(neworder)

total_table <- as_tibble(total_table)



metacont(data=dfm_depfree,n_cont, cognitive_mean_cont, cognitive_sd_cont,n_treat, cognitive_mean_treat,cognitive_sd_treat, sm = "SMD")

