# SBEP_staff_script_q3

source("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\scripts\\qa_aid2.R")

# ===============================
# Models
# ===============================

#== model head and tail - for Forest plot depiction
# NB important this does not separate studies across plots - more for useability than anything.
# default would split Ojeda et al. Country arrangement preserves study by study but in new order.
overview <- read.xlsx("C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/data/study_overview_sheet.xlsx")
overview <- select(overview,-study_num)
dfm_studies <- left_join(dfm_mod,overview, by="study_id")
countrified <- dfm_studies %>%
  arrange(country)
rows = nrow(countrified)
hed <- round(rows/2,digits=0)
tel = rows = hed

dfm_hed <- head(countrified,hed)
dfm_tel <- tail(countrified,tel)




# high quality comparisons only
qual_tog <- quality_scores_vals %>%
  select(unique_id, toggle)
dfm_mod <- left_join(dfm_mod,qual_tog, by="unique_id") 

dfm_hi <- dfm_mod %>%
  filter(toggle==1)
  
# only uniques - no within-study heterogeneity. for exploration only
  dfm_test <- dfm_mod %>%
    filter(comp==1)

  
  
  
  
### SCREENS  ###
# only mmse 
  dfm_mmse <- dfm_mod %>%
    filter(cognitive_name =="MMSE")

  table(dfm_mmse$toggle) # no of hi-qual available
  
# only moca
  dfm_moca <- dfm_mod %>%
    filter(cognitive_name =="MoCA") 
  
  table(dfm_moca$toggle) # no of hi-qual available
  
  dfm_moca_q <- filter(dfm_moca, toggle==1)    
  
# compliment to only mmse - for depiction purposes?
  dfm_not_mmse <- dfm_mod %>%
    filter(cognitive_name !="MMSE")
   #  mutate(author_final2 = str_trunc(author_final,19, "right"))

### Patient groups ###
  table(dfm_mod$sample_treat_cat)
  
  dfm_maincons <- dfm_mod %>%
    filter(sample_treat_cat %in% c("Arthritis","Fibromyalgia","Headache","MSK"))
  

  
## depression-free
  depcut<- filter(aim2,q6_mood_confound == "comparable")
  dfm_depfree <- semi_join(dfm_mod,depcut, by="study_id")
  dfm_depfree_q <- filter(dfm_depfree, toggle==1)     
# ===============================
# FULL MODEL
# 1 simple random-effects model (no accounting for non-independence)
simple <- rma(yi, vi, data=dfm_mod)
simple

# with HSK
simple2 <- rma(yi, vi, data=dfm_mod, method = "HSk")
simple2

forest(simple, slab = dfm_mod$author_final)

# ====================================================
# 2 Preparing data for CHE model


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
Vhed <- quick_v(dfm_hed)
Vtel <- quick_v(dfm_tel)
Vhi <- quick_v(dfm_hi)
Vmms <- quick_v(dfm_mmse)
Vmoc <- quick_v(dfm_moca)
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


# 3. start running Correlated and Hierarchical Effects models:
# 3a Raw CHE model:
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



che.model <- rmvee(dfm_mod,vmat=V)
 che.hed <- rmvee(dfm_hed,vmat=Vhed)
#  che.tel <- rmvee(dfm_tel,vmat=Vtel)
# che.test <- rmvee(dfm_test,vmat=Vcomp)
#che.mmse  <- rmvee(dfm_mmse,vmat=Vmms)
#che.moca <- rmvee(dfm_moca,vmat=Vmoc)
che.groups <-    rma.mv(yi, V= Vmaincons,
                      data = dfm_maincons,
                       random = ~ 1 | study_num/unique_id,
                       test ='t',
                       method = "REML",
                       mod = ~ sample_treat_cat -1,
                       sparse = TRUE)



i2 <- var.comp(che.model)
i2$plot


# W <- solve(V)
#  X <- model.matrix(che.modelY)
#  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
#  100 * che.modelY$tau2 / (che.modelY$tau2 + (che.modelY$k-che.modelY$p)/sum(diag(P)))

# This produces the key outcomes.
study_outputs_overall <- predict(che.model, digits=2)
p<- conf_int(che.model, vcov = "CR2")





# 4 use CHE model with robust inference methods


rve.model <- robust(che.model, cluster=study_id, clubSandwich=TRUE)
resids_sav <- as.tibble(residuals(rve.model))
ggplot(data = resids_sav, mapping = aes(x=1, y=value)) + geom_violin() +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank()) + ylab("residuals - Sandwich")







# ============================================================





# can also add a potential covariate
che.model2 <- rma.mv(yi ~ 1 + cognitive_name,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = dfm_mod,
                     sparse = TRUE)


# ===========================

modeloutputs <- list
modelnames <- c("Total dataset", "Fixed effect", "MMSE", "MoCA")


modelnow <-che.model





study_outputs <- predict(modelnow, digits=2)
raw_overall <- round(as_tibble(study_outputs), digits=3)
presentable <- raw_overall %>%
  transmute(
    type = "Total dataset",
    n = modelnow$k,
    conf = paste0(pred, " (", ci.lb, "-",  ci.ub,")"),
    pval = ifelse (modelnow$pval < .001,"< .001", modelnow$pval),
    i2 = round(var.comp(modelnow)$total, digits=2),
    i2_2 = var.comp(modelnow)$results[2,2],
    i2_3 = var.comp(modelnow)$results[3,2])    
#modeloutputs[1] <- presentable
#rm(study_outputs,raw_overall, presentable)


modelnow <-che.groups

# study_outputs <- predict(modelnow,newmods = c("sample_treat_catArthritis","sample_treat_catFibromyalgia","sample_treat_catHeadache", "sample_treat_catMSK"), digits=2)
# raw_overall <- round(as_tibble(study_outputs), digits=3)
# presentable <- raw_overall %>%
#   transmute(
#     type = c("Ar","FM","Head","MSK"),
#     n = modelnow$k,
#     conf = paste0(pred, " (", ci.lb, "-",  ci.ub,")"),
#     pval = ifelse (modelnow$pval < .001,"< .001", modelnow$pval),
#     i2 = round(var.comp(modelnow)$total, digits=2),
#     i2_2 = var.comp(modelnow)$results[2,2],
#     i2_3 = var.comp(modelnow)$results[3,2])   
# 
# 
# modelnow <-rve.model
#     
# study_outputs_fixed <- predict(simple, digits=2)
# raw_fixed <- round(as_tibble(study_outputs_fixed), digits=3)
# present_fixed <- raw_fixed %>%
#   transmute(
#     type = "RVE",
#     n = simple$k,
#     conf = paste0(pred, " (", ci.lb, "-",  ci.ub,")"),
#     pval = ifelse (simple$pval < .001,"< .001", che.model$pval),
#     i2 = simple$I2,
#     i2_2 = "-",
#     i2_3 = "-")   
# combine <- rbind(present_overall, present_fixed)


#===
