# qa_aid2
df <- read.xlsx("C:\\Users\\Alexander Fradera\\OneDrive - University of Glasgow\\DClin\\Deliverables\\Systematic Review\\Writeup\\DCLIN_SR_git\\quality_review.xlsx")

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
  select(author_final,study_id, unique_id, cognitive_name)

aim2 <- left_join(aim1,justauthors, by=c("unique_id", "study_id"))
aim2 <-  mutate_all(aim2,factor)


quality_scores_f <- aim2 %>%
  transmute(
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






quality_scores_vals <- quality_scores_f %>% 
  mutate_at(.vars = vars(J1:J8), 
            .funs = function(x) recode(x, 
                                       `yes` = 3,
                                       `unclear` = 2,
                                       `no` = 1)) 

quality_scores_vals <-  quality_scores_vals %>%
  mutate(
        quality_quant = rowSums(across(where(is.numeric)),na.rm = T),
        toggle = ifelse(J1 == 1 | J6 ==1| J8 ==1, 0,1)
        )





options("openxlsx.borderColour" = "#4F80BD") #
write.xlsx(quality_scores_vals, "quality_scores_vals.xlsx", asTable = FALSE, overwrite = TRUE)



#====

# filter only mmse-based quality scores
mmse_filter <- filter(dfm_mod, cognitive_name=="MMSE")
quality_mmse <- semi_join(quality_scores_vals,mmse_filter, by="unique_id")



qualityset <- quality_mmse
slots <- sum(!is.na(qualityset$unique_id))
modelnow <- che.model_mmse

png(file = "qual_plot_mmse.png", width = 2800, height = 2400, res = 300)

forest(modelnow, slab=author_final, annotate=FALSE, addfit=FALSE, addpred=FALSE, 
       showweights=FALSE, header=TRUE,  order = cognitive_name, alim=c(-1,3), xlim=c(-12,12), main="JBI Critical Appraisal scores - MMSE comparisons")

cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")

points( rep(4,slots), slots:1, pch=19, col=cols[qualityset$J1], cex=2)
text(4, slots:1, syms[qualityset$J1], cex=0.8)
points( rep(5,slots), slots:1, pch=19, col=cols[qualityset$J2], cex=2)
text(5, slots:1, syms[qualityset$J2], cex=0.8)
points( rep(6,slots), slots:1, pch=19, col=cols[qualityset$J3], cex=2)
text(6, slots:1, syms[qualityset$J3], cex=0.8)
points( rep(7,slots), slots:1, pch=19, col=cols[qualityset$J5], cex=2)
text(7, slots:1, syms[qualityset$J5], cex=0.8)
points( rep(8,slots), slots:1, pch=19, col=cols[qualityset$J6], cex=2)
text(8, slots:1, syms[qualityset$J6], cex=0.8)
points( rep(9,slots), slots:1, pch=19, col=cols[qualityset$J7], cex=2)
text(9, slots:1, syms[qualityset$J7], cex=0.8)
points( rep(10,slots), slots:1, pch=19, col=cols[qualityset$J8], cex=2)
text(10, slots:1, syms[qualityset$J8], cex=0.8)
text(4:10, slots+2, c("1", "2", "3", "5","6", "7", "8"))
dev.off()


# filter only other quality scores
oscreen_filter <- filter(dfm_mod, cognitive_name!="MMSE")
quality_other <- semi_join(quality_scores_vals,oscreen_filter, by="unique_id")

qualityset <- quality_other
slots <- sum(!is.na(qualityset$unique_id))
modelnow <- che.model_screens

png(file = "qual_plot_other_screens.png", width = 2800, height = 2400, res = 300)

forest(modelnow, slab=author_final, annotate=FALSE, addfit=FALSE, addpred=FALSE, 
       showweights=FALSE, header=TRUE,  order = cognitive_name, alim=c(-1,3), xlim=c(-12,12), main="JBI Critical Appraisal scores - MMSE comparisons")

cols <- c("red", "yellow","green")
syms <- c( "-", "?","+")
points( rep(4,slots), slots:1, pch=19, col=cols[qualityset$J1], cex=2)
text(4, slots:1, syms[qualityset$J1], cex=0.8)
points( rep(5,slots), slots:1, pch=19, col=cols[qualityset$J2], cex=2)
text(5, slots:1, syms[qualityset$J2], cex=0.8)
points( rep(6,slots), slots:1, pch=19, col=cols[qualityset$J3], cex=2)
text(6, slots:1, syms[qualityset$J3], cex=0.8)
points( rep(7,slots), slots:1, pch=19, col=cols[qualityset$J5], cex=2)
text(7, slots:1, syms[qualityset$J5], cex=0.8)
points( rep(8,slots), slots:1, pch=19, col=cols[qualityset$J6], cex=2)
text(8, slots:1, syms[qualityset$J6], cex=0.8)
points( rep(9,slots), slots:1, pch=19, col=cols[qualityset$J7], cex=2)
text(9, slots:1, syms[qualityset$J7], cex=0.8)
points( rep(10,slots), slots:1, pch=19, col=cols[qualityset$J8], cex=2)
text(10, slots:1, syms[qualityset$J8], cex=0.8)
text(4:10, slots+2, c("1", "2", "3", "5","6", "7", "8"))
dev.off()
