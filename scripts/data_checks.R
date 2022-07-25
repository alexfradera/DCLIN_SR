# checking data#
library(naniar)

dfm2 %>%
  ggplot(mapping = aes(x=cognitive_sd_treat)) + geom_histogram(binwidth = .2, fill = "wheat", color = "black")  


dfm2 %>%
  group_by(cognitive_name) %>%
  summarize(mean = mean(cognitive_sd_treat))

dfm2 %>%
  ggplot(mapping = aes(x=cognitive_sd_treat, fill = cognitive_name)) + geom_histogram(position = "identity", binwidth = .2, alpha = 0.4)  

dfm2 %>%
  ggplot(mapping = aes(x=cognitive_sd_cont, fill = cognitive_name)) + geom_histogram(position = "identity", binwidth = .2, alpha = 0.4)  
