# ============
# Creating yi and vi (effects and variances)



# ====================
# creating Hedge's g effect sizes for each model SDM with SE. 

# !!! AMEND VERSION OF DFM

# create effect sizes (Hedge's g) and standard error - $es and $se
dfm_e1 <- dfm_e %>%
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

# create effect sizes (SMD) and sampling variance - $yi and $vi
dfm_e2 <-  escalc(measure="SMD",
                     n1i = n_cont,
                     m1i = cognitive_mean_cont, 
                     sd1i = cognitive_sd_cont, 
                     n2i = n_treat ,
                     m2i =cognitive_mean_treat ,    # mean of group 2
                     sd2i = cognitive_sd_treat,  # standard error of group 2
                     data = dfm_e1,
                     vtype="AV") # this incorporates sample-size weighted average of Hedges'g values.


# create effect sizes (SMD)
dfm_e3 <- arrange(dfm_e2,study_num)

dfm_e3$Ni <- unlist(
  lapply(split(
    dfm_e3, dfm_e3$study_num),
    function(x) 
      rep(sum(x$n_treat) + x$n_cont[1], each=nrow(x))))
# check:
select(dfm_e3,study_id, study_num, comp,n_cont,n_treat,Ni)   

dfm_e3 <- dfm_e3 %>%
  mutate(
    sdpi = sqrt(((n_cont-1)*cognitive_sd_cont^2 + (n_treat-1)*cognitive_sd_treat^2) / (n_cont+n_treat-2)),
    yiN  = (cognitive_mean_cont-cognitive_mean_treat)/sdpi,
    viN = 1/n_cont+ 1/n_treat + yi^2/(2*Ni)
  )

select(dfm_e3,study_id, comp,n_cont,n_treat, Ni, cognitive_mean_cont, cognitive_mean_treat, cognitive_sd_cont, cognitive_sd_treat, sdpi, es,yi, yiN, se, vi,viN)   

# We can see that es, yi, and yiN are similar (but not identical)
# vi and viN are similar (but not identical) and vary from se (ok, as they are different measures)

dfm_mod <- dfm_e3
