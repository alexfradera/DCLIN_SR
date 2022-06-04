


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

dfm_cdapa <- dfm_cdap %>%
  rowwise() %>% 
  mutate(k = list(unmed(age_iqr1_treat, age_median_treat,age_iqr2_treat,n_treat,"t"))) %>%
  unnest_wider(k) %>%
  rowwise() %>% 
  mutate(k = list(unmed(age_iqr1_cont, age_median_cont,age_iqr2_cont,n_cont,"c"))) %>%
  unnest_wider(k) %>% 
  mutate(
    age_mean_cont = ifelse(!is.na(est_mean_cont),est_mean_cont, age_mean_cont),
    age_sd_cont = ifelse(!is.na(est_sd_cont),est_sd_cont,age_sd_cont),
    age_mean_treat = ifelse(!is.na(est_mean_treat),est_mean_treat,age_mean_treat),
    age_sd_treat = ifelse(!is.na(est_sd_treat),est_sd_treat,age_sd_treat)
  ) %>%
  select(!est_mean_treat:est_sd_cont)


# CHECKS

dfm3 %>%  count(is.na(cognitive_mean_treat)) 
dfm_cdapa %>%  count(is.na(cognitive_mean_treat))

dfm3 %>%  count(is.na(dep_mean_treat)) 
dfm_cdapa %>%  count(is.na(dep_mean_treat))

dfm3 %>%  count(is.na(pain_mean_treat))
dfm_cdapa %>%  count(is.na(pain_mean_treat))

dfm3 %>%  count(is.na(anx_mean_treat))
dfm_cdapa %>%  count(is.na(anx_mean_treat))  

dfm3 %>%  count(is.na(age_mean_treat))
dfm_cdapa %>%  count(is.na(age_mean_treat))  

dfm_e <-dfm_cdapa
