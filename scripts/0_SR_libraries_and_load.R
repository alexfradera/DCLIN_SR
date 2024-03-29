
library(datapasta)
library(meta)
library(metafor)
#library(dmetar)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(estmeansd)
library(robvis)
library("RColorBrewer")
library("clubSandwich")
library(esc)
library(naniar)
library(forestplot)
library(patchwork)
library(ggplotify)
library("flextable")
library("ftExtra")
library(stringr)
library(pryr) # for shortcuts using partial
library(captioner)
library(here)
# library(dmetar)
#---------------------------------
# FUNCTIONS
#---------------------------------

## Secondly, computing functions that will be re-used repeatedly later.

#-------------------------
# Caption making function.
figure_nums <- captioner(prefix = "Fig.")
table_nums <- captioner(prefix = "Table") #Not currently used
sup_figure_nums <- captioner(prefix = "Supplementary Fig.")
sup_table_nums <- captioner(prefix = "Supplementary Table")

citef <- partial(figure_nums, display = "cite")
citet <- partial(table_nums, display = "cite")
citesf <- partial(sup_figure_nums, display = "cite")
citest <- partial(sup_table_nums, display = "cite")

# ==================
# read and clean dataset

# dfm <- read.xlsx("~/2. Research/1. Ongoing research/pre-qualification research projects/DCLIN_SR_git/Data_SR.xlsx")

dfm <-read.xlsx(here('data', 'Data_SR.xlsx'))

# glimpse(dfm)

# factorise
dfm2<- dfm %>% 
  mutate(across(c(
    study_num,
    comp, 
    matching_education,
    matching_age,
    sample_treat_cat,
    cognitive_name,
    cognitive_variant,
    cognitive_focus,
    cognitive_exclusion_any,
    cognitive_exclusion_sametest,
    pain_measure,
    pain_measured_cont,
    pain_measured_treat,
    pain_medication_status,
    anxiety_measure,
    anx_diff,
    depression_measure,
    dep_diff
  ), factor))


levels(dfm2$cognitive_name)
# set levels - this is to match the structure of the correlation  matrix used later
dfm2 <- dfm2 %>%
  mutate(cognitive_name = fct_relevel(cognitive_name, "MMSE","MoCA", "TYM", "ACE", "HVLT"))

levels(dfm2$sample_treat_cat)



dfm2 <- dfm2 %>%
  mutate(sample_treat_cat = fct_recode(sample_treat_cat,
                                       "Fibromyalgia" = "FM",
                                        "Functional"  = "Somatisation",
                                       "Other" = "Phantom"
                                       ))

# =================================
# Explore repeats
repeats <- dfm2 %>%
  group_by(study_id)%>%
  filter(n()>1) %>%
  select(study_id,unique_id, author_final, comp, sample_cont,sample_treat,cognitive_name)



#' Calculate I-squared values and variance distribution for multilevel meta-analysis models
#' This is a function used later in the process - specifically in model_making.R
#'
#' This function calculates values of \eqn{I^2} and the variance distribution for multilevel meta-analysis
#' models fitted with \code{\link[metafor]{rma.mv}}.
#'
#'
#' @usage mlm.variance.distribution(x)
#'
#' @param x An object of class \code{rma.mv}. Must be a multilevel model with two random effects (three-level meta-analysis model).
#'
#' @details This function estimates the distribution of variance in a three-level meta-analysis
#' model (fitted with the \code{\link[metafor]{rma.mv}} function). The share of variance attributable to
#' sampling error, within and between-cluster heterogeneity is calculated,
#' and an estimate of \eqn{I^2} (total and for Level 2 and Level 3) is provided. The function uses the formula by
#' Cheung (2014) to estimate the variance proportions attributable to each model component and to derive the \eqn{I^2} estimates.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/mlma.html}{Chapter 12}.
#'
#'Cheung, M. W. L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural equation modeling approach. \emph{Psychological Methods, 19}(2), 211.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @aliases var.comp
#'
#' @import ggplot2
#' @importFrom stats model.matrix
#'
#' @return Returns a data frame containing the results. A plot summarizing the variance distribution and \eqn{I^2} values can be generated using \code{plot}.
#'
#' @export mlm.variance.distribution
#' @export var.comp
#'
#' @examples
#' # Use dat.konstantopoulos2011 from the "metafor" package
#' library(metafor)
#'
#' # Build Multilevel Model (Three Levels)
#' m = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)
#'
#' # Calculate Variance Distribution
#' mlm.variance.distribution(m)
#'
#' # Use alias 'var.comp' and 'Chernobyl' data set
#' data("Chernobyl")
#' m2 = rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
#' res = var.comp(m2)
#'
#' # Print results
#' res
#'
#' # Generate plot
#' plot(res)



mlm.variance.distribution = var.comp = function(x){
  
  m = x
  
  # Check class
  if (!(class(m)[1] %in% c("rma.mv", "rma"))){
    stop("x must be of class 'rma.mv'.")
  }
  
  # Check for three level model
  if (m$sigma2s != 2){
    stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
  }
  
  # Check for right specification (nested model)
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1){
    stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
  }
  
  # Get variance diagonal and calculate total variance
  n = m$k.eff
  vector.inv.var = 1/(diag(m$V))
  sum.inv.var = sum(vector.inv.var)
  sum.sq.inv.var = (sum.inv.var)^2
  vector.inv.var.sq = 1/(diag(m$V)^2)
  sum.inv.var.sq = sum(vector.inv.var.sq)
  num = (n-1)*sum.inv.var
  den = sum.sq.inv.var - sum.inv.var.sq
  est.samp.var = num/den
  
  # Calculate variance proportions
  level1=((est.samp.var)/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level2=((m$sigma2[2])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level3=((m$sigma2[1])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  
  # Prepare df for return
  Level=c("Level 1", "Level 2", "Level 3")
  Variance=c(level1, level2, level3)
  df.res=data.frame(Variance)
  colnames(df.res) = c("% of total variance")
  rownames(df.res) = Level
  I2 = c("---", round(Variance[2:3], 2))
  df.res = as.data.frame(cbind(df.res, I2))
  
  totalI2 = Variance[2] + Variance[3]
  
  
  # Generate plot
  df1 = data.frame("Level" = c("Sampling Error", "Total Heterogeneity"),
                   "Variance" = c(df.res[1,1], df.res[2,1]+df.res[3,1]),
                   "Type" = rep(1,2))
  
  df2 = data.frame("Level" = rownames(df.res),
                   "Variance" = df.res[,1],
                   "Type" = rep(2,3))
  
  df = as.data.frame(rbind(df1, df2))
  
  
  g = ggplot(df, aes(fill=Level, y=Variance, x=as.factor(Type))) +
    coord_cartesian(ylim = c(0,1), clip = "off") +
    geom_bar(stat="identity", position="fill", width = 1, color="black") +
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color="black"),
          axis.line.y = element_blank(),
          axis.title.y=element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(lineend = "round"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          axis.ticks.length=unit(.25, "cm"),
          plot.margin = unit(c(1,3,1,1), "lines")) +
    scale_fill_manual(values = c("darkseagreen3", "deepskyblue3", "darkseagreen2",
                                 "deepskyblue1", "deepskyblue2")) +
    
    # Add Annotation
    
    # Total Variance
    annotate("text", x = 1.5, y = 1.05,
             label = paste("Total Variance:",
                           round(m$sigma2[1]+m$sigma2[2]+est.samp.var, 3))) +
    
    # Sampling Error
    annotate("text", x = 1, y = (df[1,2]/2+df[2,2])/100,
             label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 3) +
    
    # Total I2
    annotate("text", x = 1, y = ((df[2,2])/100)/2-0.02,
             label = bquote("Total"~italic(I)^2*":"~.(round(df[2,2],2))*"%"), size = 3) +
    annotate("text", x = 1, y = ((df[2,2])/100)/2+0.05,
             label = paste("Variance not attributable \n to sampling error: \n", round(m$sigma2[1]+m$sigma2[2],3)), size = 3) +
    
    # Level 1
    annotate("text", x = 2, y = (df[1,2]/2+df[2,2])/100, label = paste("Level 1: \n",
                                                                       round(df$Variance[3],2), "%", sep=""), size = 3) +
    
    # Level 2
    annotate("text", x = 2, y = (df[5,2]+(df[4,2]/2))/100,
             label = bquote(italic(I)[Level2]^2*":"~.(round(df[4,2],2))*"%"), size = 3) +
    
    # Level 3
    annotate("text", x = 2, y = (df[5,2]/2)/100,
             label = bquote(italic(I)[Level3]^2*":"~.(round(df[5,2],2))*"%"), size = 3)
  
  returnlist = list(results = df.res,
                    totalI2 = totalI2,
                    plot = g)
  class(returnlist) = c("mlm.variance.distribution", "list")
  
  invisible(returnlist)
  
  returnlist
  
}





