# Presentation table

overview <- read.xlsx("C:/Users/Alexander Fradera/OneDrive - University of Glasgow/DClin/Deliverables/Systematic Review/Writeup/DCLIN_SR_git/data/study_overview_sheet.xlsx")
overview <- overview %>%
  mutate(biblio_comp = paste0("@",biblio))

pres_overview <- select(overview, study_id, biblio_comp)

pres_dat <- dfm_e %>%
  select(study_id, sample_treat, n_treat, age_mean_treat, age_sd_treat,  n_cont, matching_education, cognitive_name )

test <- left_join(pres_dat,pres_overview, by="study_id")
test <- test %>%
  arrange(biblio_comp) %>%
  relocate(biblio_comp) %>%
  select(!study_id)

test <- test %>%
  mutate(age_mean_treat = round(age_mean_treat, 2),
         age_sd_treat = round(age_sd_treat, 2)
         )


test %>%
  flextable() %>%
  colformat_md() %>%
  autofit()
