
library(datapasta)
library(meta)
library(metafor)
library(dmetar)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(estmeansd)
library(robvis)
library("RColorBrewer")
library("clubSandwich")
library(esc)
library(naniar)
library(forestplot)


# ==================
# read and clean dataset

# dfm <- read.xlsx("Data_SR.xlsx")
dfm <- read.xlsx("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\Data_SR.xlsx")


# glimpse(dfm)

# factorise
dfm2<- dfm %>% 
  mutate(across(c(
    study_num,
    comp, 
    matching_education,
    matching_age,
    sample_treat_cat,
    cognitive_name,
    cognitive_variant,
    cognitive_focus,
    cognitive_exclusion_any,
    cognitive_exclusion_sametest,
    pain_measure,
    pain_measured_cont,
    pain_measured_treat,
    pain_medication_status,
    anxiety_measure,
    anx_diff,
    depression_measure,
    dep_diff
  ), factor))


levels(dfm2$cognitive_name)
# set levels - this is to match the structure of a correlation  matrix used later
dfm2 <- dfm2 %>%
  mutate(cognitive_name = fct_relevel(cognitive_name, "MMSE","MoCA", "TYM", "ACE", "HVLT"))

levels(dfm2$sample_treat_cat)



dfm2 <- dfm2 %>%
  mutate(sample_treat_cat = fct_recode(sample_treat_cat,
                                       "Fibromyalgia" = "FM",
                                        "Functional"  = "Somatisation",
                                       "Functional" = "Phantom"
                                       ))

# =================================
# Explore repeats
repeats <- dfm2 %>%
  group_by(study_id)%>%
  filter(n()>1) %>%
  select(study_id,unique_id, study_num, sample_cont,sample_treat,cognitive_name)







