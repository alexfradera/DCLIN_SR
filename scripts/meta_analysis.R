

# =======================================
# META ANALYSIS
# =======================================




# ====================
# Adjusting for range restriction

# find U, ratio of unrestricted SD / study SD

dfm_test3$study_id



# ====================================================
# Preparing data for CHE model

# constant sampling correlation assumption
rho <- 0.6 # initial guess

# constant sampling correlation working model
V <- with(dfm, 
          impute_covariance_matrix(vi = var.z,
                                   cluster = author,
                                   r = rho))



# ====================================================
# Meta-analysis

# Use metcont to pool results.
m_cont <- metacont(n.e = n_cont,
                   mean.e = fin_cognitive_mean_treat,
                   sd.e = fin_cognitive_sd_treat,
                   n.c = n_cont,
                   mean.c = fin_cognitive_mean_cont,
                   sd.c = fin_cognitive_sd_cont,
                   studlab = study_id,
                   data = dfm,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE, # if this doesn't work try comb.fixed. sometimes is having trouble with this
                   random = TRUE, #ditto comb.random
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Cognitive screens and chronic pain")
summary(m_cont)
print(m_cont)

colnames(dfm)

# Saving the Forest Plots
# This can be tricky
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest.html#saving-the-forest-plots
# you need to call the save function and then do the thing:

png(file = "forestplot.png", width = 2800, height = 2400, res = 300)

# forest plot
forest.meta(m_cont, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftcols = c("studlab", "cognitive_name"),
            rightcols = c("effect","ci"),
#            layout = "JAMA",
#            layout = "RevMan5",
            #leftlabs = c("studlab", "TE"),
            #rightlabs = c("studlab", "TE"),
            label.left = "pain ~ poorer cog",
            label.right = "pain ~ better cog"
            )
#and then deactivate the png-mode
dev.off()


# =======================================
# Funnel plot

# Firstly create a dataset with only those studies where the use of screen was central
 dfm_key <- dfm %>%
    filter(cognitive_focus == "key")

m.key <- metacont(n.e = n_cont,
                   mean.e = fin_cognitive_mean_treat,
                   sd.e = fin_cognitive_sd_treat,
                   n.c = n_cont,
                   mean.c = fin_cognitive_mean_cont,
                   sd.c = fin_cognitive_sd_cont,
                   studlab = study.id,
                   data = dfm_key,
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE, # if this doesn't work try comb.fixed. sometimes is having trouble with this
                   random = TRUE, #ditto comb.random
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "Cognitive screens part of study focus")




funnel.meta(m.key,
            xlim = c(-0.5, 2),
            studlab = TRUE)

# Add title
title("Funnel Plot (Chronic pain on cognitive screens - focused studies)")

# do a contour plot to point out which studies were more significant
col_contour = c("gray75", "gray85", "gray95")

funnel.meta(m.key, xlim = c(-0.5, 2),
            contour = c(0.9, 0.95, 0.99),
            col_contour = col_contour)
legend(x = 1.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col_contour)
title("Funnel Plot (Chronic pain on cognitive screens - focused studies)")




set.seed(123)
x1 <- rnorm(n = 20, mean = 28, sd = 3)
x2 <- rnorm(n = 20, mean = 35, sd = 0)

# Calculate values we need for the formulas
s1 <- sd(x1)
s2 <- sd(x2)
n1 <- 20
n2 <- 20

# Calculate the mean difference
MD <- mean(x1) - mean(x2)
MD

s_pooled <- sqrt(
  (((n1-1)*s1^2) + ((n2-1)*s2^2))/
    ((n1-1)+(n2-1))
)

# Calculate the standard error
se <- s_pooled*sqrt((1/n1)+(1/n2))
se
