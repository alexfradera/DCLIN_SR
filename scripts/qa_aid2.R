
source(here('scripts', 'vi_yi.R')) 

 df <- read.xlsx(here("data", "quality_review.xlsx"))

# df <- read.xlsx("C:\\Users\\ajf11r\\OneDrive - University of Glasgow\\Documents\\2. Research\\1. Ongoing research\\pre-qualification research projects\\DCLIN_SR_git\\quality_review.xlsx")

# simplify dataset and collapse the two Terassi rows into one, so it doesn't fail when merged with 
# analysis dataset (where this was done)
df2<- df %>% 
  mutate(across(!c(
    unique_id,
    qual_1_reason,
    qual_2_reason,
    qual_3_reason,
    qual_5_reason,
    qual_6_reason,
    qual_7_reason,
    qual_8_reason,
    q5_other_confound), factor),
    unique_id = if_else(unique_id == "TER167-A", "TER167-COMB", unique_id ))

# Operationalise decisions on Items 5 and 6 based upon reporting/controlling
# for key variables
aim1 <-   df2 %>%
  mutate(
    ed_age_report           = ifelse(q5_6_ed_confound == "not reported"|q5_6_ed_confound == "unclear"| q5_6_age_confound == "unclear", "no", "yes"),
    e_a_mood_report         = ifelse(ed_age_report == "yes"   & q5_mood_recorded == "yes", "yes", "no"),
    e_a_mood_meds_report    = ifelse(e_a_mood_report == "yes" &  q_5_med_recorded == "yes",  "yes", "no"),
    ed_age_controlled       = ifelse(q5_6_ed_confound == "no" & q5_6_age_confound == "no", "yes", "no"),
    e_a_meds_controlled     = ifelse(ed_age_controlled == "yes" &  q6_med_confound == "no",  "yes", "no"),
    e_a_meds_mood_controlled     = ifelse(e_a_meds_controlled   == "yes" &  q6_mood_confound == "comparable",  "yes", "no"),
    q5_score_final = e_a_mood_meds_report, # You should report all 4 variables
    q6_score_final = ed_age_controlled     # but only expected to control for two
  ) 

aim1 <- aim1 %>%
  filter(unique_id !="TER167-B")

# add in authornames from previous dataset
justauthors <- dfm_mod %>%
  select(author_final,study_id, unique_id, cognitive_name, comp)

aim2 <- left_join(aim1,justauthors, by=c("unique_id", "study_id"))
aim2 <-  mutate_all(aim2,factor)

# clean up dataset for easy use
quality_scores_f <- aim2 %>%
  transmute(
    study_id =study_id,
    comp = comp,
    Study = author_final,
    unique_id = unique_id,
    J1 = qual_1_score,
    J2 = qual_2_score,
    J3 = qual_3_score,
    J5 = q5_score_final,
    J6 = q6_score_final,
    J7 = qual_7_score,
    J8 = qual_8_score,
    screen = cognitive_name,
    Weight = 1
  ) 

# =======

# recode characters into 3/2/1 to work with quality plot (in qualinprogress.R)
quality_scores_vals <- quality_scores_f %>% 
  mutate_at(.vars = vars(J1:J8), 
            .funs = function(x) recode(x, 
                                       `yes` = 3,
                                       `unclear` = 2,
                                       `no` = 1)) 

# Introduce high/low quality variable, where toggle==0 means failing 
# one of the 3 key quality measures
quality_scores_vals <-  quality_scores_vals %>%
  mutate(
        quality_quant = rowSums(across(where(is.numeric)),na.rm = T),
        toggle = as.factor(ifelse(J1 == 1 | J6 ==1| J8 ==1, 0,1))
        )

options("openxlsx.borderColour" = "#4F80BD") #

write.xlsx(quality_scores_vals, here('data',"quality_scores_vals.xlsx"), asTable = FALSE, overwrite = TRUE)
#write.xlsx(quality_scores_vals, "quality_scores_vals.xlsx", asTable = FALSE, overwrite = TRUE)

# qualityset<- quality_scores_vals 

