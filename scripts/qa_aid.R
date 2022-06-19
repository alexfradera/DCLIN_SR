source("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\scripts\\model_making.R")

# Quality review count and analysis aid

df <- read.xlsx("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\quality_review.xlsx")

# glimpse(df)


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
  select(author_final,study_id, unique_id, cognitive_name)

# df5 <- left_join(df4,justauthors, by="study_id")
df5 <- left_join(df4,justauthors, by=c("unique_id", "study_id"))
df5 <-  mutate_all(df5,factor)



quality_descripts <- df5

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
unique_codes <- dfm_mod %>% select(study_id, unique_id)
recuperated <- full_join(unique_codes,df5)

recup_mmse <- filter(recuperated, cognitive_name=="MMSE")
qualityset <- recup_mmse
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

# =================
# MEDICATION

# how many instances of recording or controlling meds
mrecs <- count(df5, q_5_med_recorded =="yes")
mconf <- count(df5, q6_med_confound =="no")

# how many also were generally well-controlled:
goodmeds <- df5 %>%
  filter(q6_med_confound =="no")
goodmeds_plus <- goodmeds %>%
  filter(q5_6_age_confound =="no" & q5_6_ed_confound =="no" )

sharemeds <- df5 %>% 
  mutate( medscon = case_when(
                            q6_med_confound == "no" ~0,
                            q6_med_confound =="uncontrolled difference" ~ 1,
                            q6_med_confound == "unknown" ~ 999  )
         ) %>%
  replace_with_na(replace=list(medscon=999)) %>%
  select(unique_id,medscon)






# data for meds added into the main model
dfm_all_confs <- left_join(multismd,sharemeds, by="unique_id")

## plugging in with other moderators: too many redundancies, fails...
#che.model_moderators <- rma.mv(yi_cogN, mods = cbind(medscon, age_mean_cont, yi_pain),
#                        V = V,
#                        random = ~ 1 | study_num/unique_id,
#                        data = dfm_all_confs,
#                        sparse = TRUE)

# ======
# subsetted model with controlled medication only

dfm_medscon <- semi_join(multismd,goodmeds)

V_mc <- vcalc(vi=vi_cogN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
               obs=cognitive_name, rho=R, data=dfm_medscon)

che.model_medscon <- rma.mv(yi_cogN ~ 1,
                         V = V_mc,
                         random = ~ 1 | study_num/unique_id,
                         data = dfm_medscon,
                         sparse = TRUE)

#

dfm_allcon <- semi_join(multismd,goodmeds_plus)

V_mt <- vcalc(vi=vi_cogN, cluster=study_num, grp1=sample_cont, grp2=sample_treat, w1=n_cont, w2=n_treat,
              obs=cognitive_name, rho=R, data=dfm_allcon)

che.model_allcon <- rma.mv(yi_cogN,
                            V = V_mt,
                            random = ~ 1 | study_num/unique_id,
                            data = dfm_allcon,
                            sparse = TRUE)

che.model_allcon_mods <- rma.mv(yi_cogN, mods = age_mean_cont,
                                V = V_mt,
                                random = ~ 1 | study_num/unique_id,
                                data = dfm_allcon,
                                sparse = TRUE)


