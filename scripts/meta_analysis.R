
library(datapasta)
library(meta)
library(dmetar)
library(tidyverse)
library(openxlsx)
library(estmeansd)

#====================
# temp data

df <- tibble::tribble(
  ~study.id, ~n.cont, ~age.mean.cont, ~age.sd.cont, ~male.n.cont, ~female.n.cont, ~n.treat, ~age.mean.treat, ~age.sd.treat, ~male.n.treat, ~female.n.treat, ~cognitive.name, ~cognitive.focus,                                                                       ~cognitive.process, ~cognitive.exclusions, ~cognitive.mean.cont, ~cognitive.sd.cont, ~cognitive.mean.treat, ~cognitive.sd.treat, ~cognitive.median.cont, ~cognitive.iqr1.cont, ~cognitive.iqr2.cont, ~cognitive.median.treat, ~cognitive.iqr1.treat, ~cognitive.iqr2.treat,
  "WEI184",    160L,           73.5,          4.8,          94L,            66L,       163L,            73.6,           5.2,           83L,             80L,          "MMSE",     "incidental",                                                                                       NA,                   "Y",                 28.7,                1.3,                  28.3,                 1.3,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "ALM009",    105L,          35.51,         7.35,          42L,            63L,     100L,           35.31,          6.95,           40L,             60L,          "MOCA",            "key",                                                                         "none described",                    NA,                27.12,               1.33,                 21.92,                4.13,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "APA016",     23L,           71.8,          6.9,          12L,            11L,      39L,            71.1,           6.1,            9L,             30L,          "MOCA",            "key",                                                                         "none described",                   "Y",                 27.9,                1.5,                  27.2,                 2.7,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BAR026",     30L,           48.7,         11.1,           0L,            30L,      30L,              52,           8.9,            0L,             30L,          "MMSE",            "key",                            "Does not describe, but plausible that Spanish versions used",                    NA,                29.73,               0.69,                 29.29,                1.42,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BOL034",    118L,           53.2,         10.4,          11L,            15L,     118L,            50.6,           9.5,           11L,             15L,          "MOCA",            "key",                                                                        "German language",                    NA,                   27,                  4,                  25.5,                 2.7,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BOR37",     18L,           49.5,  7.793285194,           0L,            18L,      18L,     50.38888889,   9.870567591,            0L,             18L,          "MOCA",            "key", "NB: scores are reported incorrectly in paper, however accessed raw scores from author.",                    NA,          27.61111111,        2.913233273,           25.38888889,          2.78945259,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "BUC047",      8L,           69.9,          3.9,           3L,             5L,       8L,            74.5,           4.2,            4L,              4L,          "MMSE",     "incidental",                                                                                       NA,                   "Y",                 27.6,                1.2,                 28.25,                0.71,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "CAN053",     40L,          63.73,         9.55,          10L,            30L,      40L,           65.63,          8.59,           10L,             30L,          "MMSE",            "key",                     "Completed by one person - a psychiatrist (GP). In Italian versions",                    NA,                   NA,                 NA,                    NA,                  NA,                  25.25,                 23.5,                 26.2,                   23.35,                  21.1,                  25.2,
  "CHE062",     18L,          39.11,         9.99,           4L,            14L,      16L,           42.44,          8.65,            4L,             12L,          "MOCA",     "incidental",                                                                                       NA,                    NA,                26.89,               2.47,                 22.94,                5.37,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "COE069",     45L,          40.76,        10.05,           NA,             NA,      45L,           41.07,          9.68,            NA,              NA,          "MMSE",            "key",                                                                         "No information",                    NA,                28.86,               1.44,                 27.86,                2.56,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "COR070",     27L,           56.7,         3.34,           NA,             NA,      31L,            56.9,         14.62,           19L,             12L,          "HVLT",               NA,                                                                                       NA,                   "Y",                27.41,               3.32,                 25.94,                3.15,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA,
  "DEM077",     23L,           43.3,          9.1,           1L,            22L,      23L,            47.6,            12,            1L,             22L,          "MMSE",     "incidental",                                                                                       NA,                    NA,                 24.5,                3.2,                  22.7,                 4.5,                     NA,                   NA,                   NA,                      NA,                    NA,                    NA
)
#====================


df <- read.xlsx("DATASETNAME.xlsx")

glimpse(df)

# factorise
df$cognitive.name = as.factor(df$cognitive.name)
df$cognitive.focus = as.factor(df$cognitive.focus)
df$cognitive.exclusions = as.factor(df$cognitive.exclusions)
levels(df$cognitive.name)

# turn median scores into useable scores


                        
# firstly a short function to redescribe proportions as percentages

      
      unmedian.mean<- function(min.v,med.v,max.v,the.n) {
        set.seed(1)
        return(bc.mean.sd(min.val = min.v, med.val = med.v, max.val = max.v, n = the.n)$est.mean)
      }
      
      unmedian.sd<- function(min.v,med.v,max.v,the.n) {
        set.seed(1)
        bc.mean.sd(min.val = min.v, med.val = med.v, max.val = max.v, n = the.n)$est.sd
      }
      
      


df9 <- df %>%
  filter(!is.na(cognitive.median.treat)) %>%
        mutate(
                new.t.mean = unmedian.mean(cognitive.iqr1.treat, cognitive.median.treat, cognitive.iqr2.treat, n.treat),
                new.t.sd = unmedian.sd(cognitive.iqr1.treat, cognitive.median.treat, cognitive.iqr2.treat, n.treat),
                new.c.mean = unmedian.mean(cognitive.iqr1.cont, cognitive.median.cont, cognitive.iqr2.cont, n.cont),
                new.c.sd = unmedian.sd(cognitive.iqr1.cont, cognitive.median.cont, cognitive.iqr2.cont, n.cont),
      )      

df10 <- full_join(df9,df)


df11 <- df10 %>% 
  mutate(
          fin.cognitive.mean.treat = ifelse(!is.na(cognitive.mean.treat),cognitive.mean.treat, new.t.mean),
          fin.cognitive.sd.treat = ifelse(!is.na(cognitive.sd.treat),cognitive.sd.treat, new.t.sd),
          fin.cognitive.mean.cont = ifelse(!is.na(cognitive.mean.cont),cognitive.mean.cont, new.c.mean),
          fin.cognitive.sd.cont = ifelse(!is.na(cognitive.sd.cont),cognitive.sd.cont, new.c.sd),
                    )

# check - should be nas and zeros
checks <- list(
                (df11$fin.cognitive.mean.treat - df11$cognitive.mean.treat ),
               (df11$fin.cognitive.sd.treat - df11$cognitive.sd.treat),
               (df11$fin.cognitive.mean.cont - df11$cognitive.mean.cont),
               (df11$fin.cognitive.sd.cont - df11$cognitive.sd.cont),
               (df11$fin.cognitive.mean.treat - df11$new.t.mean ),
               (df11$fin.cognitive.sd.treat - df11$new.t.sd),
               (df11$fin.cognitive.mean.cont - df11$new.c.mean),
               (df11$fin.cognitive.sd.cont - df11$new.c.sd)
                 )
checks


# ====================================================

# Use metcont to pool results.
m.cont <- metacont(n.e = n.cont,
                   mean.e = fin.cognitive.mean.treat,
                   sd.e = fin.cognitive.sd.treat,
                   n.c = n.cont,
                   mean.c = fin.cognitive.mean.cont,
                   sd.c = fin.cognitive.sd.cont,
                   studlab = study.id,
                   data = df,
                   sm = "SMD",
                   method.smd = "Hedges",
                   comb.fixed = FALSE,
                   comb.random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Suicide Prevention")
summary(m.cont)
print(m.cont)

colnames(df)
