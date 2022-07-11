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
  dfm_mmse_all <- dfm_mod %>%
    filter(cognitive_name =="MMSE")

  table(dfm_mmse_all$toggle) # no of hi-qual available
  
  
  dfm_mmse <- filter(dfm_mmse_all, toggle==1)  
  
# only moca
  dfm_moca_all <- dfm_mod %>%
    filter(cognitive_name =="MoCA") 
  
  table(dfm_moca_all$toggle) # no of hi-qual available
  
  dfm_moca <- filter(dfm_moca_all, toggle==1)    
  
# compliment to only mmse - for depiction purposes?
#  dfm_not_mmse <- dfm_mod %>%
#    filter(cognitive_name !="MMSE")
   #  mutate(author_final2 = str_trunc(author_final,19, "right"))

### Patient groups ###
  table(dfm_mod$sample_treat_cat)
  
  dfm_maincons <- dfm_mod %>%
    filter(sample_treat_cat %in% c("Arthritis","Fibromyalgia","Headache","MSK"))
  
  dfm_arthritis_full <-dfm_mod %>%
    filter(sample_treat_cat %in% c("Arthritis"))
  dfm_arthritis <- dfm_arthritis_full %>%
    filter(toggle==1)
  
  
  dfm_fm_full <-dfm_mod %>%
    filter(sample_treat_cat %in% c("Fibromyalgia"))
  dfm_fm <- dfm_fm_full %>%
    filter(toggle==1)
  
  
  
## depression-free
  depcut<- filter(aim2,q6_mood_confound == "comparable")
  dfm_depfree_all <- semi_join(dfm_mod,depcut, by="study_id")
  dfm_depfree<- filter(dfm_depfree_all, toggle==1)     

# tidy
  rm(aim1,df,df2,dfm, dfm_c,dfm_cd,dfm_cda,dfm_cdap,dfm_cdapa,dfm_e1,dfm_e2,dfm3)
  
  # ===============================
# FULL MODEL
# 1 simple random-effects model (no accounting for non-independence)
 
    


# with HSK
#simple_hsk <- rma(yi, vi, data=dfm_mod, method = "HSk")


# forest(simple, slab = dfm_mod$author_final)

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
Varth <- quick_v(dfm_arthritis)
Vfm  <- quick_v(dfm_fm_full)
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


simple.model <- rma(yi, vi, data=dfm_mod)
confint(simple.model)
predict(simple.model)
che.model <- rmvee(dfm_mod,vmat=V)
che.ci <- confint(che.model)
model.i2 <- var.comp(che.model)
main_model_output <- predict(simple.model)
rve.model <- robust(che.model, cluster=study_id, clubSandwich=TRUE) # substantially same as che.model




res <- rma.mv(yi, vi, random = ~ factor(unique_id) | study_num, data=dfm_mod)
res.ci <- confint(res)
W <- diag(1/dfm_mod$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$tau2 / (res$tau2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * res.ci[[1]]$random[1,2:3] / (res.ci[[1]]$random[1,2:3] + (res$k-res$p)/sum(diag(P)))


simple.hi <- rma(yi, vi, data=dfm_hi)
confint(simple.hi)
che.hi <- rmvee(dfm_hi,vmat=Vhi)
hi.i2 <- var.comp(che.hi)


# che.hed <- rmvee(dfm_hed,vmat=Vhed)
# che.tel <- rmvee(dfm_tel,vmat=Vtel)
# che.test <- rmvee(dfm_test,vmat=Vcomp)

simple.mmse <- rma(yi, vi, data=dfm_mmse)
che.mmse  <- rmvee(dfm_mmse,vmat=Vmms)
mmse.i2  <- var.comp(che.mmse)
forest(che.mmse, slab=author_final, annotate=TRUE, addfit=TRUE, addpred=FALSE, at= seq(-1,5, by =1), xlim=c(-5,8),
              showweights=FALSE, header=TRUE, order = study_id, cex=.8, top = 0)

simple.moca <- rma(yi, vi, data=dfm_moca)
# che.moca <- rmvee(dfm_moca,vmat=Vmoc) - no information, as no clusters here
#moca.i2  <- var.comp(che.moca)
#forest(che.moca, slab=author_final, annotate=TRUE, addfit=TRUE, addpred=FALSE, at= seq(-1,5, by =1), xlim=c(-5,8),
#       showweights=FALSE, header=TRUE, order = study_id, cex=.8, top = 0)


simple.groups<- rma(yi, vi, data=dfm_maincons, mods= ~ sample_treat_cat -1)
che.groups <-    rma.mv(yi, V= Vmaincons,
                      data = dfm_maincons,
                       random = ~ 1 | study_num/unique_id,
                       test ='t',
                       method = "REML",
                       mod = ~ sample_treat_cat -1,
                       sparse = TRUE)
groups.i2  <- var.comp(che.groups)

simple.arth <- rma(yi,vi, data=dfm_arthritis)
che.arth <- rmvee(dfm_arthritis,vmat=Varth)


simple.fm <- rma(yi,vi, data=dfm_fm_full)
che.fm <- rmvee(dfm_fm_full,vmat=Vfm)


simple.dep <- rma(yi, vi, data=dfm_depfree)













# ============================================================





# can also add a potential covariate
che.model2 <- rma.mv(yi ~ cognitive_name -1,
                     V = V,
                     random = ~ 1 | study_num/unique_id,
                     data = dfm_mod,
                     sparse = TRUE)


# ===========================


che_names <- c("Total dataset", "Low risk of bias", "MMSE","Arthritis","Fibromyalgia")
che_modellist <- vector(mode = "list", length = 5)
  che_modellist[[1]] <- che.model
  che_modellist[[2]] <- che.hi
  che_modellist[[3]] <- che.mmse
  che_modellist[[4]] <- che.arth
  che_modellist[[5]] <- che.fm
che_tablelist <- vector(mode = "list", length = 5)

for (i in 1:5){
modelnow <-che_modellist[[i]]
names <- che_names
study_outputs <- predict(modelnow, digits=2)
raw_overall <- round(as_tibble(study_outputs), digits=3)
presentable <- raw_overall %>%
  transmute(
    order = i,
    type = names[i],
    method = "Multi-level approach",
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


simp_names <-c(che_names, "MoCA", "Matched depression")
simp_modellist <- vector(mode = "list", length = 7)
simp_tablelist <- vector(mode = "list", length = 7)
simp_modellist[[1]] <- simple.model
simp_modellist[[2]] <- simple.hi
simp_modellist[[3]] <- simple.mmse
simp_modellist[[4]] <- simple.arth
simp_modellist[[5]] <- simple.fm
simp_modellist[[6]] <- simple.moca
simp_modellist[[7]] <- simple.dep


for (i in 1:7){
modelnow <-simp_modellist[[i]]
names <- simp_names
study_outputs <- predict(modelnow, digits=2)
raw_overall <- round(as_tibble(study_outputs), digits=3)
presentable <- raw_overall %>%
  transmute(
    order = i,
    type = names[i],
    method = "Random Effects",
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
total_table <- total_table %>% arrange(order,method)
total_table <- as.tibble(total_table)
