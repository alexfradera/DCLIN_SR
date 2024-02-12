# Additional checks
dfm_agers |>
  group_by(age_bucket)|>
  summarize(
    mean_effect = mean(es),
    sd_effect = sd(es),
    n = n()
  )


simple.age <- rma(yi, vi, data=dfm_agers, mods = ~ age_bucket, method = "REML")

test<-rma(yi, vi, data=dfm_agers, method = "REML")


dfm_allages <- dfm_mod |>
  filter(!is.na(age_mean_treat))

total_num <- sum(dfm_allages$n_treat)
sum(dfm_allages$age_mean_treat * dfm_allages$n_treat)/total_num


dfm_pained <- dfm_mod |>
  filter(pain_measured_treat == "yes")
    
dfm_pained <- dfm_pained |> 
  arrange(pain_measure) |> 
  select(study_id, unique_id, sample_treat,cognitive_name,sample_treat, pain_measure,pain_mean_treat,pain_sd_treat,pain_mean_cont,pain_sd_cont,pain_duration_mean, pain_duration_sd, pain_intensity_qualitative)
  
  
#=======================
# Pain levels

pain_only <- dfm_mod |>
  select(author_final, cognitive_name,sample_treat, pain_measure:pain_sd_treat) |>
  #filter(pain_measured_treat == "yes")
  filter(!is.na(pain_mean_treat))

pain_trimmed <- as.data.frame(pain_only) |>
    arrange(pain_measure) |>
  mutate(
    author_final = case_when(
      author_final == "Liao et al. (2018)" ~ paste(author_final, "(", cognitive_name, ")"),
      author_final == "Chen et al. (2016)" ~ paste(author_final, "(", cognitive_name, ")" ),
      author_final == "Peterson et al. (2018)" ~ paste(author_final, "(", sample_treat, ")" ),
      author_final == "Fayed et al. (2012)" ~ paste(author_final, "(", sample_treat, ")" ),
      author_final == "Ojeda et al. (2016)" ~ paste(author_final, "(", sample_treat, "/", cognitive_name,")" ),
      TRUE ~ author_final
    )) |>
  select(!c(pain_measured_treat, pain_measured_cont, pain_intensity_qualitative,cognitive_name,sample_treat))



library(gt)

pain_table <-
pain_trimmed |>
gt() |>
  fmt_number(
    #columns = num,
    decimals = 2,
    use_seps = FALSE
  ) |>
    cols_merge(
        columns = c(pain_mean_treat, pain_sd_treat),
    #hide_columns = columns[-1],
    rows = everything(),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(pain_mean_cont, pain_sd_cont),
    #hide_columns = columns[-1],
    rows = everything(),
    pattern = "{1} ({2})"
    ) |>
  cols_label(
    author_final = "Study",
    pain_measure = "Measure",
    pain_mean_cont = "Control Mean (SD)",
    pain_mean_treat = "Patient Mean (SD)"
  )

pain_table

