source("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\scripts\\model_making.R")

# Quality review count and analysis aid

df <- read.xlsx("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\quality_review.xlsx")

# glimpse(df)


df2<- df %>% 
  mutate(across(!c(
    qual_1_reason,
    qual_2_reason,
    qual_3_reason,
    qual_5_reason,
    qual_6_reason,
    qual_7_reason,
    qual_8_reason,
    q5_other_confound), factor))


df3 <- df2 %>% distinct(study_id,  .keep_all= TRUE)



## Checking scores (Items 5-6 considered presently)
count(df3,qual_1_score)
count(df3,qual_2_score)
count(df3,qual_3_score)
count(df3,qual_7_score)
count(df3,qual_8_score)


# Exploring confounds
count(df3, q6_med_confound) 
count(df3, q6_med_confound) 
count(df3,q5_6_ed_confound)
count(df3,q5_6_age_confound)
count(df3,q5_mood_recorded)
count(df3,q_5_med_recorded)
count(df3,q6_med_confound )
count(df3,q6_mood_confound)

df4 <-   df3 %>%
  mutate(
          ed_age_report           = ifelse(q5_6_ed_confound == "not reported"|q5_6_ed_confound == "unclear"| q5_6_age_confound == "unclear", "no", "yes"),
          e_a_mood_report         = ifelse(ed_age_report == "yes"   & q5_mood_recorded == "yes", "yes", "no"),
          e_a_mood_meds_report    = ifelse(e_a_mood_report == "yes" &  q_5_med_recorded == "yes",  "yes", "no"),
          ed_age_controlled       = ifelse(q5_6_ed_confound == "no" & q5_6_age_confound == "no", "yes", "no"),
          e_a_meds_controlled     = ifelse(ed_age_controlled == "yes" &  q6_med_confound == "no",  "yes", "no"),
          e_a_meds_mood_controlled     = ifelse(e_a_meds_controlled   == "yes" &  q6_mood_confound == "comparable",  "yes", "no")
           ) 

count(df4,e_a_mood_meds_report)
count(df4,ed_age_controlled )
count(df4,e_a_meds_controlled )
count(df4,e_a_meds_mood_controlled )
count(df4,q6_mood_confound)

df4 <- df4 %>% 
  mutate(
        q5_score_final = e_a_mood_meds_report, # You should report all 4 variables
        q6_score_final = ed_age_controlled     # but only expected to control for two
        )


count(df4,q5_score_final)
count(df4,q6_score_final)


justauthors <- dfm_mod %>%
  select(author_final,study_id,cognitive_name)

df5 <- left_join(df4,justauthors, by="study_id")
df5 <-  mutate_all(df5,factor)

quality_scores <- df5 %>%
  transmute(
            Study = author_final,
            JBI1_inclusion_criteria = qual_1_score,
            JBI2_subjects_setting = qual_2_score,
            JBI3_exposure_measurement = qual_3_score,
            JBI5_confounds_identified = q5_score_final,
            JBI6_confounds_addressed = q6_score_final,
            JBI7_outcomes_measured = qual_7_score,
            JBI8_appropriate_statistics = qual_8_score,
            screen = cognitive_name,
            Overall = "",
            Weight = 1
            ) 





count(quality_scores,JBI1_inclusion_criteria)

quality_scores_vals <- quality_scores %>% 
  mutate_at(.vars = vars(JBI1_inclusion_criteria:JBI8_appropriate_statistics), 
            .funs = function(x) recode(x, 
                                       `yes` = 1, 
                                       `no` = 0)) 

quality_scores_vals <-  quality_scores_vals %>%
  mutate(quality_quant = rowSums(across(where(is.numeric)),na.rm = T))
         

         
options("openxlsx.borderColour" = "#4F80BD") #
write.xlsx(quality_scores_vals, "quality_scores_vals.xlsx", asTable = FALSE, overwrite = TRUE)


# =====



# ====
df7 <- filter(df5, cognitive_name=="MMSE")
qualityset <- df7
slots <- sum(!is.na(qualityset$study_id))

png(file = "qual_plot_mmse.png", width = 2800, height = 2400, res = 300)

forest(che.model_mmse, slab=author_final, annotate=FALSE, addfit=FALSE, addpred=FALSE, 
       showweights=FALSE, header=TRUE,  order = cognitive_name, alim=c(-1,3), xlim=c(-12,12), main="JBI Critical Appraisal scores - MMSE comparisons")

cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")
points( rep(4,slots), slots:1, pch=19, col=cols[qualityset$qual_1_score], cex=2)
text(4, slots:1, syms[qualityset$qual_1_score], cex=0.8)
points( rep(5,slots), slots:1, pch=19, col=cols[qualityset$qual_2_score], cex=2)
text(5, slots:1, syms[qualityset$qual_2_score], cex=0.8)
points( rep(6,slots), slots:1, pch=19, col=cols[qualityset$qual_3_score], cex=2)
text(6, slots:1, syms[qualityset$qual_3_score], cex=0.8)
points( rep(7,slots), slots:1, pch=19, col=cols[qualityset$q5_score_final], cex=2)
text(7, slots:1, syms[qualityset$q5_score_final], cex=0.8)
points( rep(8,slots), slots:1, pch=19, col=cols[qualityset$q6_score_final], cex=2)
text(8, slots:1, syms[qualityset$q6_score_final], cex=0.8)
points( rep(9,slots), slots:1, pch=19, col=cols[qualityset$qual_7_score], cex=2)
text(9, slots:1, syms[qualityset$qual_7_score], cex=0.8)
points( rep(10,slots), slots:1, pch=19, col=cols[qualityset$qual_8_score], cex=2)
text(10, slots:1, syms[qualityset$qual_8_score], cex=0.8)
text(4:10, slots+2, c("1", "2", "3", "5","6", "7", "8"))
dev.off()


# ===
df6 <- filter(df5, cognitive_name!="MMSE")
qualityset <- df6
slots <- sum(!is.na(qualityset$study_id))





png(file = "qual_plot_other_screens.png", width = 2800, height = 2400, res = 300)

cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")
forest(che.model_screens, slab=author_final, annotate=FALSE, addfit=FALSE, addpred=FALSE, 
       showweights=FALSE, header=TRUE,  order = cognitive_name, alim=c(-1,3), ilab = cbind.data.frame(cognitive_name), ilab.xpos = -3.6, xlim=c(-12,12), ilab.pos = 4, main="JBI Critical Appraisal scores - other screens")
points( rep(4,slots), slots:1, pch=19, col=cols[qualityset$qual_1_score], cex=2) 
text(4, slots:1, syms[qualityset$qual_1_score], cex=0.8) 
points( rep(5,slots), slots:1, pch=19, col=cols[qualityset$qual_2_score], cex=2) 
text(5, slots:1, syms[qualityset$qual_2_score], cex=0.8) 
points( rep(6,slots), slots:1, pch=19, col=cols[qualityset$qual_3_score], cex=2) 
text(6, slots:1, syms[qualityset$qual_3_score], cex=0.8) 
points( rep(7,slots), slots:1, pch=19, col=cols[qualityset$q5_score_final], cex=2) 
text(7, slots:1, syms[qualityset$q5_score_final], cex=0.8) 
points( rep(8,slots), slots:1, pch=19, col=cols[qualityset$q6_score_final], cex=2) 
text(8, slots:1, syms[qualityset$q6_score_final], cex=0.8) 
points( rep(9,slots), slots:1, pch=19, col=cols[qualityset$qual_7_score], cex=2) 
text(9, slots:1, syms[qualityset$qual_7_score], cex=0.8) 
points( rep(10,slots), slots:1, pch=19, col=cols[qualityset$qual_8_score], cex=2) 
text(10, slots:1, syms[qualityset$qual_8_score], cex=0.8) 
text(4:10, slots+2, c("1", "2", "3", "5","6", "7", "8")) 
dev.off()