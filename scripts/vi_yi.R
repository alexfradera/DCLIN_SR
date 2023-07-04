
source(here('scripts', 'medians_means.R')) 

# ============
# Creating yi and vi (effects and variances)


# ====================
# 1. "traditional route"
# Creating Hedge's g effect sizes for each model SDM with SE. 

# 1a create effect sizes (Hedge's g) and standard error - $es and $se
dfm_e1 <- dfm_e %>%
  rowwise() %>%
  dplyr::mutate(es =  esc_mean_sd(grp2m = cognitive_mean_treat,   
                           grp2sd = cognitive_sd_treat,  
                           grp2n = n_treat  ,    
                           grp1m = cognitive_mean_cont ,    
                           grp1sd = cognitive_sd_cont,  
                           grp1n = n_cont ,    
                           es.type = "g")$es,
         se = esc_mean_sd(grp2m = cognitive_mean_treat,  
                          grp2sd = cognitive_sd_treat, 
                          grp2n = n_treat  ,   
                          grp1m = cognitive_mean_cont , 
                          grp1sd = cognitive_sd_cont, 
                          grp1n = n_cont ,   
                          es.type = "g")$se
  )





# 1b create effect sizes (SMD), this time with sampling variance instead of se - $yi and $vi
# uses metafor escalc() function

dfm_e2 <-  escalc(measure="SMD",
                  n1i = n_cont,
                  m1i = cognitive_mean_cont, 
                  sd1i = cognitive_sd_cont, 
                  n2i = n_treat,
                  m2i =cognitive_mean_treat ,    # mean of group 2
                  sd2i = cognitive_sd_treat,  # standard error of group 2
                  data = dfm_e1,
                  vtype="LS") 

# =========================
# compare effect sizes
# select(dfm_e3,study_id, comp,n_cont,n_treat, 
#         cognitive_mean_cont, cognitive_mean_treat, 
#         cognitive_sd_cont, cognitive_sd_treat, 
#         sdpi, es,yi, se, vi,)   

# We can see that es and yi are virtually identical.
# vi varies from se (ok, as they are different measures)

dfm_mod <- dfm_e2

# ===========================
# Plotting out effect sizes

# relationship with age
dfm_mod %>%
  filter(yi < 2.5) %>%
  ggplot(mapping = aes(x=age_mean_cont,y=yi)) + geom_point() + stat_smooth(method=lm)

outliers <- dfm_mod %>%
  filter(yi > 2) %>%
  select(1:25,yi,cognitive_mean_cont,cognitive_sd_cont, cognitive_mean_treat,cognitive_sd_treat)

#-------

