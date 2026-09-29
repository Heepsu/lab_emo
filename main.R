library(here)
# includes dplyr and ggplot2
library(tidyverse)
library(lme4)
library(ggrepel)
library(data.table)
library(tidyr)
library(stringr)
library(emmeans)
library(ggcorrplot) 

# load custom functions to global environment
source(here("R", "utils.R"))

# run analysis
source(here("scripts", "01_import.R"))
source(here("scripts", "02_demographics.R"))
source(here("scripts", "03a_emotion_descriptive.R"))
source(here("scripts", "03b_choice_descriptive.R"))
source(here("scripts", "05_correlation_analysis.R"))
source(here("scripts", "06_lmm.R"))

message("Analysis pipeline executed successfully!")