
# APPENDIX



# HERE FOLLOWS ALL THE PREPARATORY CODE/SYNTAX FOR THE SBEP ANALYSIS


#---------------------------------
# LIBRARIES
#---------------------------------

## Firstly, getting the packages that allow the work to be done ----
library(tidyverse)
library(haven)
library(forcats)
library(lubridate)
library(RColorBrewer)
library(gcookbook)
library(purrr)
library(viridis)
library(captioner)
library(dplyr)
library(broom) # tidy statistical outputs
library(qqplotr)
library(stringr) # helping wrap title text
library(ggpubr) # making it easier to arrange graphs, eg for q3
# library(gt) unsure if needed - for certain table philosophy...
# library(glue) ditto
library(questionr)
library(janitor)
library(reshape2)
library(arsenal)
library(bookdown)
library(knitr)
library(pander)
library(papaja)



# set rounding 
basic_dec <- 0
data_dec <- 2
stat_dec <- 3
tabwidth = 0.8


# Creating a colour-blind friendly palette for increased useability
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")





############################
#### PATIENT SCRIPTS #######
############################



############################
#### STAFF SCRIPTS #########
############################


