
```{r pander_tweaks, include=FALSE}

# library(datapasta) # Ctrl + Shift + t OR Ctrl + Alt + Shift + v for vector
# library(groundhog)
# set.groundhog.folder('C:/Users/Alexander Fradera/OneDrive - University of Glasgow/non-dclin')
# set.groundhog.folder('C:/Users/Alexander Fradera/OneDrive - University of Glasgow/non-dclin/5_Code/groundhog_library')
# groundhog.library('stringi','2021-09-30') # this is to try and get around a dependency issue with tidyverse
# groundhog.library("tidyverse", "2021-09-30")
# groundhog.library("haven", "2021-09-30")
# groundhog.library("forcats", "2021-09-30")
# groundhog.library("lubridate", "2021-09-30")
# groundhog.library("RColorBrewer", "2021-09-30")
# groundhog.library("gcookbook", "2021-09-30")
# groundhog.library("purrr", "2021-09-30")
# groundhog.library("viridis", "2021-09-30")
# groundhog.library("captioner", "2021-09-30")
# groundhog.library("dplyr", "2021-09-30")
# groundhog.library("broom", "2021-09-30") # tidy statistical outputs
# groundhog.library("qqplotr", "2021-09-30")
# groundhog.library("stringr", "2021-09-30") # helping wrap title text
# groundhog.library("ggpubr", "2021-09-30") # making it easier to arrange graphs, eg for q3
# # groundhog.library("gt", "2021-09-30") unsure if needed - for certain table philosophy...
# groundhog.library("questionr", "2021-09-30")
# groundhog.library("janitor", "2021-09-30")
# groundhog.library("reshape2", "2021-09-30")
# groundhog.library("arsenal", "2021-09-30")
# groundhog.library("bookdown", "2021-09-30")
# groundhog.library("knitr", "2021-09-30")
# groundhog.library("flextable", "2021-09-30")
# groundhog.library("officer", "2021-09-30")
# groundhog.library("captioner", "2021-09-30")
# groundhog.library("pander", "2021-09-30")
# groundhog.library("papaja", "2021-09-30") #  Prepare APA journal articles with R Markdown
# groundhog.library("rstatix", "2021-09-30")
# groundhog.library("pwr", "2021-09-30")# for power analysis
# groundhog.library("ccoptimalmatch", "2021-09-30")
# groundhog.library("glue", "2021-09-30")
# groundhog.library("psych", "2021-09-30") # for PCA
# groundhog.library("plotrix", "2021-09-30") # for standard error
# groundhog.library("naniar", "2021-09-30") # for missing data
# 
# 
# panderOptions('table.alignment.default', function(df)
#     ifelse(sapply(df, is.numeric), 'right', 'left'))
# panderOptions('table.split.table', Inf)
# panderOptions('big.mark', ",")
# panderOptions('keep.trailing.zeros', TRUE)

```



``` {r set rounding}
basic_dec <- 0
data_dec <- 2
stat_dec <- 3
tabwidth = 0.8


```

``` {r cbf_palette}
# Creating a colour-blind friendly palette for increased useability

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

```

word_document:
  fig_caption: yes
reference_docx: C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic
Review/Writeup/DCLIN_SR_git/SR_Template.docx
bookdown::word_document2:
  fig_caption: yes
reference_docx: C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic
Review/Writeup/DCLIN_SR_git/SR_Template.docx
html_document:
  df_print: paged



---
  title: "Systematic Review"
author: "2509920F"
date: "`sa"
output: officedown::rdocx_document
reference_docx: 'C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/test.docx'
bibliography: 'C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/SR_biblio.bib'
csl: 'C:/Users/Alexander Fradera/OneDrive - University of Glasgow/non-dclin/5_Code/R library/csl/neuropsychology.csl'
---
  
  title: "Systematic Review"
author: "2509920F"
date: "`sa"
output: officedown::rdocx_document
reference_docx: 'C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/test.docx'
bibliography: 'C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/SR_biblio.bib'
csl: 'C:/Users/Alexander Fradera/OneDrive - University of Glasgow/non-dclin/5_Code/R library/csl/neuropsychology.csl'



vas_scores <- c(10,7,5,3,4,7,7,7,3,7)
summary(vas_scores)
sd(vas_scores)


# Good
iris %>%
  group_by(Species) %>%
  summarize_if(is.numeric, mean) %>%
  ungroup()%>%
  gather(measure, value, -Species)%>%
  arrange(value)


nr = nrow(ter_only)   # number of rows
newN <- rep(0,nr)        # array for combined n of this and previous group
new_cog_treat_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_cog_cont_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_pain_treat_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_age_treat_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_age_cont_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_dep_treat_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_dep_cont_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_cog_treat_Mean <- rep(0,nr)     # array for combined mean of this and previous group
new_cog_cont_Mean <- rep(0,nr)     # array for combined mean of this and previous group
# SDs
new_cog_treat_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_cog_cont_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_pain_treat_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_age_treat_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_age_cont_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_dep_treat_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_dep_cont_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_cog_treat_SD <- rep(0,nr)     # array for combined mean of this and previous group
new_cog_cont_SD <- rep(0,nr)     # array for combined mean of this and previous group
# Prime the first row by copying from data frame
newN_treat[1] = ter_only$n_treat[1]
newN_cont[1] = ter_only$n_cont


newMean[1]  = ter_only$mean[1]

f <- function(v1,m,v2,n) {
  res = bc.mean.sd(q1.val = v1,med.val = m,  q3.val  = v2, n  = n)
  list("est.mean" = res$est.mean, "est.sd" = res$est.sd)
}

df %>%
  rowwise() %>% 
  mutate(k = list(f(iqr1,median,iqr2,n))) %>%
  unnest_wider(k)



testy <- unmedian.mean(dfm_cm$cognitive_iqr1_treat[1], dfm_cm$cognitive_median_treat[1], dfm_cm$cognitive_iqr2_treat[1], dfm_cm$n_treat[1])

testy2 <- bc.mean.sd(q1.val = dfm_cm$cognitive_iqr1_treat[1],  
                     med.val = dfm_cm$cognitive_median_treat[1], 
                     q3.val  = dfm_cm$cognitive_iqr2_treat[1],    
                     n  = dfm_cm$n_treat[1])$est.mean

set.seed(1)
dfm_cm2 <- dfm_cm %>%
  slice(1:3)  %>%
  mutate(
    new_t_mean =  bc.mean.sd(q1.val = cognitive_iqr1_treat,   
                             med.val = cognitive_median_treat,  
                             q3.val  = cognitive_iqr2_treat,    
                             n  = n_treat)$est.mean
  )



easy <- select(dfm_cm,cognitive_iqr1_treat,cognitive_median_treat,cognitive_iqr2_treat,n_treat)


unmed.m<- function(v1,m,v2,n,type,group) {
  set.seed(1) # for reproducibility so the randomiser isn't affected
  res = bc.mean.sd(q1.val = v1,med.val = m,  q3.val  = v2, n  = n)
  if (type == "c" & group == "c" )
    list("est_cog_mean_cont" = res$est.mean, "est_cog_sd_cont" = res$est.sd)
  else if (type == "c" & group == "t" )
    list("est_cog_mean_treat" = res$est.mean, "est_cog_sd_treat" = res$est.sd)
  else if (type == "d" & group == "c" )
    list("est_dep_mean_cont" = res$est.mean, "est_dep_sd_cont" = res$est.sd)
  else if (type == "d" & group == "t" )
    list("est_dep_mean_treat" = res$est.mean, "est_dep_sd_treat" = res$est.sd)
  
  else if (type == "p" & group == "c" )
    list("est_pain_mean_cont" = res$est.mean, "est_pain_sd_cont" = res$est.sd)
  else if (type == "p" & group == "t" )
    list("est_pain_mean_treat" = res$est.mean, "est_pain_sd_treat" = res$est.sd)
  else if (type == "a" & group == "c" )
    list("est_anx_mean_cont" = res$est.mean, "est_anx_sd_cont" = res$est.sd)
  else if (type == "a" & group == "t" )
    list("est_anx_mean_treat" = res$est.mean, "est_anx_sd_treat" = res$est.sd)
  
  
  dfm_cm <- dfm3 %>%
    filter(!is.na(cognitive_median_treat)) %>%
    rowwise() %>% 
    mutate(k = list(unmed(cognitive_iqr1_treat, cognitive_median_treat,cognitive_iqr2_treat,n_treat,"t"))) %>%
    unnest_wider(k) %>% 
    rowwise() %>% 
    mutate(k = list(unmed(cognitive_iqr1_cont, cognitive_median_cont,cognitive_iqr2_cont,n_cont,"c"))) %>%
    unnest_wider(k) %>%
    mutate(
      temp = "cog_medians",
      cognitive_mean_cont = est_mean_cont,
      cognitive_sd_cont = est_sd_cont,
      cognitive_mean_treat = est_mean_treat,
      cognitive_sd_treat = est_sd_treat)  %>%
    select(!est_mean_treat:est_sd_cont)
  
  
  
  
  dfm_dm <- dfm3 %>%
    filter(!is.na(dep_iqr1_treat)) %>%
    rowwise() %>% 
    mutate(k = list(unmed(dep_iqr1_treat, dep_median_treat,dep_iqr2_treat,n_treat,"t"))) %>%
    unnest_wider(k) %>%
    rowwise() %>% 
    mutate(k = list(unmed(dep_iqr1_cont, dep_median_cont,dep_iqr2_cont,n_cont,"c"))) %>%
    unnest_wider(k) %>% 
    mutate(
      temp = "dep_medians",
      dep_mean_cont = est_mean_cont,
      dep_sd_cont = est_sd_cont,
      dep_mean_treat = est_mean_treat,
      dep_sd_treat = est_sd_treat) %>%
    select(!est_mean_treat:est_sd_cont)
  
  dfm_am <- dfm3 %>%
    filter(!is.na(anx_iqr1_cont)) %>%
    rowwise() %>% 
    mutate(k = list(unmed(anx_iqr1_treat, anx_median_treat,anx_iqr2_treat,n_treat,"t"))) %>%
    unnest_wider(k) %>%
    rowwise() %>% 
    mutate(k = list(unmed(anx_iqr1_cont, anx_median_cont,anx_iqr2_cont,n_cont,"c"))) %>%
    unnest_wider(k) %>% 
    mutate(
      temp = "anx_medians",
      anx_mean_cont = est_mean_cont,
      anx_sd_cont = est_sd_cont,
      anx_mean_treat = est_mean_treat,
      anx_sd_treat = est_sd_treat) # %>%
  # select(!est_mean_treat:est_sd_cont)
  
  dfm_pm <- dfm3 %>% # NB no medians for controls here
    filter(!is.na(pain_iqr1_treat)) %>%
    rowwise() %>% 
    mutate(k = list(unmed(pain_iqr1_treat, pain_median_treat,pain_iqr2_treat,n_treat,"t"))) %>%
    unnest_wider(k) %>%
    mutate(
      temp = "pain_medians",
      pain_mean_treat = est_mean_treat,
      pain_sd_treat = est_sd_treat) %>%
    select(!est_mean_treat:est_sd_treat)
  
  
  
  
  dat <- data.frame(study=c(1,1,2,3,4,4), 
                    trt=c(1,2,1,1,1,2),
                    m1i=c(7.87, 4.35, 9.32, 8.08, 7.44, 5.34),
                    m2i=c(-1.36, -1.36, 0.98, 1.17, 0.45, 0.45),
                    sdpi=c(4.2593,4.2593,2.8831,3.1764,2.9344,2.9344),
                    n1i=c(25,22,38,50,30,30), 
                    n2i=c(25,25,40,50,30,30))
  
  dat$Ni <- unlist(lapply(split(dat, dat$study), function(x) rep(sum(x$n1i) + x$n2i[1], each=nrow(x))))
  
  dat$yi <- with(dat, (m1i-m2i)/sdpi)
  dat$vi <- with(dat, 1/n1i + 1/n2i + yi^2/(2*Ni))
  dat
  
 dat <-  escalc(measure="SMD",
                       n1i = n_cont,
                       m1i =   m1i, 
                      sdpi = sdpi, 
                       n2i = n2i ,
                       m2i =m2i ,    # mean of group 2
                       data = dat,
                       vtype="AV") # this incorporates sample-size weighted average of Hedges'g values.
  
  
  
  
  calc.v <- function(x) {
    v <- matrix(1/x$n2i[1] + outer(x$yi, x$yi, "*")/(2*x$Ni[1]), nrow=nrow(x), ncol=nrow(x))
    diag(v) <- x$vi
    v
  }
  
  V <- bldiag(lapply(split(dat, dat$study), calc.v))
  V
  
  res <- rma.mv(yi, V, mods = ~ factor(trt) - 1, data=dat)
  print(res, digits=3)
  
  round(vcov(res), 5)

  
  #====  
  # Median offcuts
  
  
  
  
  #--
  
  
  
  rowwise() %>% 
    mutate(k = list(unmed.m(pain_iqr1_treat,pain_median_treat,pain_iqr2_treat, n_treat,"p","t"))) %>%
    unnest_wider(k) %>% 
    
    
    
    rowwise() %>% 
    mutate(k = list(unmed.m(anx_iqr1_cont,anx_median_cont,anx_iqr2_cont, n_cont,"a","c"))) %>%
    unnest_wider(k) %>% 
    
    rowwise() %>% 
    mutate(k = list(unmed.m(anx_iqr1_treat,anx_median_treat,anx_iqr2_treat, n_treat,"a","t"))) %>%
    unnest_wider(k)
  
  
  
  
  # test - first with raw data and then with relevant row
  unmed.m(2,4,6,100)   
  
  dfm_cm <- dfm3 %>%
    filter(!is.na(cognitive_median_treat))
  
  
  # Use functions to produce new variables. 
  # NB this breaks when missing data, so just filtering non-missing and then recombine
  
  
  
  
  dfm <- dfm %>%
    mutate(es =  esc_mean_sd(grp1m = cognitive_mean_cont,   # mean of group 1
                             grp1sd = cognitive_sd_cont,  # standard error of group 1
                             grp1n = n_cont  ,    # sample in group 1
                             grp2m = cognitive_mean_treat ,    # mean of group 2
                             grp2sd = cognitive_sd_treat,  # standard error of group 2
                             grp2n = n_treat ,    # sample in group 2
                             es.type = "g")$es)
  
  
  ## Generate S2 summary data
  set.seed(1)
  n <- 100
  x <- stats::rlnorm(n, 2.5, 1)
  quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
  obs.mean <- mean(x)
  obs.sd <- stats::sd(x)
  
  ## Estimate the sample mean and standard deviation using the BC method
  res <- bc.mean.sd(q1.val = quants[1], med.val = quants[2],
                    q3.val = quants[3], n = n)
  print(res$est.mean)
  
  
  
  
  
  
  dfm_test_reduce <-   dfm2 %>%
    filter(!is.na(cognitive_median_treat)) %>%
    mutate(
      new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
      new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
    )    
  
  
  dfm_test_reduce2 <-   dfm_test %>%
    filter(!is.na(cognitive_median_treat)) %>%
    slice(1)  %>%
    mutate(
      new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
      new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
    )    
  
  
  
  
  
  dfm_cm <- dfm %>%
    filter(!is.na(cognitive_median_treat)) %>%
    mutate(
      new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
      new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
    )     
  
  dfm_depm <- dfm %>%
    filter(!is.na(cognitive_median_treat)) %>%
    mutate(
      new_t_mean = unmedian.mean(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_t_sd = unmedian.sd(cognitive_iqr1_treat, cognitive_median_treat, cognitive_iqr2_treat, n_treat),
      new_c_mean = unmedian.mean(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
      new_c_sd = unmedian.sd(cognitive_iqr1_cont, cognitive_median_cont, cognitive_iqr2_cont, n_cont),
    )     
  
  
  
  
  
  dfm <- full_join(dfm2,dfm)
  
  # create a final variable for means and sds - if an original value missing it inserts the new one from step above
  dfm3 <- dfm %>% 
    mutate(
      fin_cognitive_mean_treat = ifelse(!is.na(cognitive_mean_treat),cognitive_mean_treat, new_t_mean),
      fin_cognitive_sd_treat = ifelse(!is.na(cognitive_sd_treat),cognitive_sd_treat, new_t_sd),
      fin_cognitive_mean_cont = ifelse(!is.na(cognitive_mean_cont),cognitive_mean_cont, new_c_mean),
      fin_cognitive_sd_cont = ifelse(!is.na(cognitive_sd_cont),cognitive_sd_cont, new_c_sd),
      median_used = ifelse(!is.na(cognitive_mean_treat), "No", "Yes")
    )
  
  # check - should be nas and zeros
  checks <- list(
    (dfm3$fin_cognitive_mean_treat - dfm3$cognitive_mean_treat ),
    (dfm3$fin_cognitive_sd_treat - dfm3$cognitive_sd_treat),
    (dfm3$fin_cognitive_mean_cont - dfm3$cognitive_mean_cont),
    (dfm3$fin_cognitive_sd_cont - dfm3$cognitive_sd_cont),
    (dfm3$fin_cognitive_mean_treat - dfm3$new_t_mean ),
    (dfm3$fin_cognitive_sd_treat - dfm3$new_t_sd),
    (dfm3$fin_cognitive_mean_cont - dfm3$new_c_mean),
    (dfm3$fin_cognitive_sd_cont - dfm3$new_c_sd)
  )
  checks
  
  # if ok, finalise dataset:
  dfm <- dfm3
  
  rm(dfm2,dfm3,checks)
  
  
  
  # factorise
  dfm_mod <- dfm_mod %>% 
    mutate(across(c(sample_treat_cat), factor))
  
  dfm_mod <- dfm_mod %>%
    mutate(sample_treat_cat = fct_recode(sample_treat_cat,
                                         "Fibromyalgia" = "FM",
                                         "Functional"  = "Somatisation",
                                         "Functional" = "Phantom"
    ))
  
  
  # =================
  # RMD
  dat2 <- tibble::tribble(
    ~biblio, ~Order, ~study.id, ~extraction.date,    ~reviewer.stage,                                                                                                                                                                      ~author,                                                                                                             ~title,   ~country,
    "AlMalki2020",     9L,  "ALM009",           44624L, "full-text-review",                                                                                 "Al-Malki,Daifallah;Kotb,Mamdouh Ali;Kamal,Ahmed M.;Abd El Fatah, Aliaa S.;Ahmed,Yassmin M.", "Cognitive performance in patients with chronic tension-type headache and its relation to neuroendocrine hormones",   "Egypt?",
    "Cardoso2021",    16L,  "APA016",           44624L,       "pre-search",                   "Apagueno,Brandon;Hoyos,Lorraine;Cardoso,Josue;Lysne,Paige;Riley,Joseph L.;Fillingim,Roger B.;Porges,Eric;Woods,Adam J.;Cohen,Ronald;Cruz-Almeida,Yenisel",                                                       "Pain and the Montreal Cognitive Assessment (MoCA) in Aging",      "USA",
    "BarceloMartinez2018",    26L,  "BAR026",           44624L, "full-text-review", "Barceló-Martinez,Ernesto;Gelves-Ospina,Melissa;Lechuga,Edgar Navarro;Allegri,Ricardo F.;Orozco-Acosta,Erick;Benítez-Agudelo,Juan C.;León-Jacobus,Alexandra;Román,Néstor F.",                 "Serum cortisol levels and neuropsychological impairments in patients diagnosed with fibromyalgia", "Colombia"
  )
  