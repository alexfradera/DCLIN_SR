
# NOW DEPRECATED _ SEE QUALINPROGRESS



# =======================================
# Forests and funnels
# =======================================
par(mfcol=c(2,1), heights = c(5,8))

forest(che.model_screens, slab=author_final, annotate=FALSE, addfit=FALSE, addpred=FALSE, 
       showweights=FALSE, header=TRUE,  order = cognitive_name, alim=c(-1,3), ilab = cbind.data.frame(cognitive_name), ilab.xpos = -3.6, xlim=c(-12,12), ilab.pos = 4, main="JBI Critical Appraisal scores - other screens")


df6 <- filter(df5, cognitive_name!="MMSE")


qualityset <- df6
slots <- sum(!is.na(qualityset$study_id))


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



# other one







# ====================
# Adjusting for range restriction

# find U, ratio of unrestricted SD / study SD





# ====================================================
# Meta-analysis
dfm_split <- dfm_mod
dfm_split$study_num <- as.double(dfm_split$study_num)  #NB note this ruins the numbers and turns them to 1-53 inclusive
dfm_split_1 <- filter(dfm_split,study_num < 15)

# Use metcont to pool results.
m_cont <- metacont(n.e = n_cont,
                   mean.e = cognitive_mean_treat,
                   sd.e = cognitive_sd_treat,
                   n.c = n_cont,
                   mean.c = cognitive_mean_cont,
                   sd.c = cognitive_sd_cont,
                   studlab = study_id,
                   data = dfm_split_1,
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

# png(file = "forestplot.png", width = 2800, height = 2400, res = 300)

# forest plot


# 
# 
# 
# 
# 
# forest.meta(m_cont, 
#             sortvar = TE,
#             prediction = FALSE, 
#             print.tau2 = FALSE,
#             random = FALSE,
#             leftcols = c("studlab", "cognitive_name"),
#             rightcols = c("effect","ci"),
# #            layout = "JAMA",
# #            layout = "RevMan5",
#             #leftlabs = c("studlab", "TE"),
#             #rightlabs = c("studlab", "TE"),
#             label.left = "pain ~ poorer cog",
#             label.right = "pain ~ better cog"
#             )
# #and then deactivate the png-mode
# dev.off()


# =======================================
# Funnel plot

# Firstly create a dataset with only those studies where the use of screen was central
 dfm_key<- dfm_mod %>%
    filter(cognitive_focus == "key")

res_key <- rma(yi, vi, data=dfm_key)
res_tri <- trimfill(res_key)
funnel(res_tri)

# what about without outlier?
dfm_key_out <- filter(dfm_mod, yi <3)

res_key_out <- rma(yi, vi, data=dfm_key_out)
res_tri_out <- trimfill(res_key_out)
funnel(res_tri_out, legend= TRUE, main = "Funnel Plot (focused studies)") 

# 
# m_key <- metacont(n.e = n_cont,
#                    mean.e = cognitive_mean_treat,
#                    sd.e = cognitive_sd_treat,
#                    n.c = n_cont,
#                    mean.c = cognitive_mean_cont,
#                    sd.c = cognitive_sd_cont,
#                    studlab = study_id,
#                    data = dfm_key_mmse,
#                    sm = "SMD",
#                    method.smd = "Hedges",
#                    fixed = FALSE, # if this doesn't work try comb.fixed. sometimes is having trouble with this
#                    random = TRUE, #ditto comb.random
#                    method.tau = "REML",
#                    hakn = TRUE,
#                    title = "Cognitive screens part of study focus")







# Add title


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
