# Pain summaries
pain_treats <- dfm_mod %>%
filter(pain_measured_treat=="yes") %>%
  summarise(n())
pain_conts <- dfm_mod %>%
  filter(pain_measured_cont=="yes") %>%
  summarise(n())

median(dfm_mod$pain_duration_mean, na.rm = TRUE)




sum(!is.na(dfm_mod$pain_measured_cont))

painsum <- dfm_mod %>%
  group_by(pain_measure) %>%
  summarise(
            mean = mean(pain_mean_treat, na.rm = TRUE),
            count = n()
                        )

sum(!is.na(painsum$count))





# relationship with pain
dfm_mod %>%
  filter(pain_measure=="VAS (0-10)") %>%
  ggplot(mapping = aes(x=pain_mean_treat,y=yi)) + geom_point() + stat_smooth(method=lm)

# relationship with pain
dfm_mod %>%
  ggplot(mapping = aes(x=pain_mean_treat,y=yi, colour =pain_measure)) + geom_point() + stat_smooth(method=lm)

dfm_mod %>%
  mutate(
    sta_pain = ifelse(pain_measure == "PVAS (0-100)", (pain_mean_treat*.1)/(pain_sd_treat*.1), pain_mean_treat/pain_sd_treat),
    raw_pain = ifelse(pain_measure == "PVAS (0-100)", (pain_mean_treat*.1), pain_mean_treat)) %>%
  group_by(pain_measure) %>%
  filter(n()>2) %>%
  ggplot(mapping = aes(x=raw_pain,y=yi, colour =pain_measure)) + geom_point() + stat_smooth(method=lm, se= FALSE)


dfm_mod %>%
  select(age_mean_treat,yi) %>%
  arrange(-yi)
