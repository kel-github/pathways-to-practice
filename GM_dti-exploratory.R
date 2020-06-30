# written by K. Garner and Georgia Marsh, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

rm(list=ls())

# set working directory to current location
install.packages("rstudioapi")  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
install.packages("tidyverse") 
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load data
# --------------------------------------------------------------------------------

fpath <- 'C:/Git/pathways-to-practice/dti-data/' 

tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"), 
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Superior_occipital_gyrus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Superior_occipital_gyrus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Superior_occipital_gyrus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Superior_occipital_gyrus_Left"),
               c("Caudate_nucleus_Left", "Superior_occipital_gyrus_Left"),
               c("Caudate_nucleus_Left", "Superior_occipital_gyrus_Right"),
               c("Caudate_nucleus_Right", "Superior_occipital_gyrus_Right"),
               c("Caudate_nucleus_Right", "Superior_occipital_gyrus_Left"),
               c("Thalamus_Left", "Superior_occipital_gyrus_Left"),
               c("Thalamus_Left", "Superior_occipital_gyrus_Right"),
               c("Thalamus_Right", "Superior_occipital_gyrus_Right"),
               c("Thalamus_Right", "Superior_occipital_gyrus_Left"))  
sub.data <- GetDTIData(fpath, tracts)

# 1. tidy dataframe up
# --------------------------------------------------------------------------------
# chapter 15 of https://r4ds.had.co.nz/factors.html will help you here as will the commands View() and head()

sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$sub <- as.factor(sub.data$sub)

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-trial", "post-trial")

sub.data$tract_names <- c("lDLPFC_lCN", "lDLPFC_rCN", "rDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "lDLPFC_rLNP", "rDLPFC_rLNP", "rDLPFC_lLNP",
                          "lDLPFC_lSOG", "lDLPFC_rSOG", "rDLPFC_rSOG", "rDLPFC_lSOG",
                          "lCN_lSOG", "lCN_rSOG", "rCN_rSOG", "rCN_lSOG",
                          "lTHA_lSOG", "lTHA_rSOG", "rTHA_rSOG", "rTHA_lSOG")


# 2. basic data check
# --------------------------------------------------------------------------------
# this will help you as a start https://rstudio-education.github.io/tidyverse-cookbook/transform-tables.html

summary(sub.data)
# Put the above data into one line of code

summary <- sub.data %>% group_by(group, session) %>%
  summarise(N=length(unique(sub)))

# group
# session
# N
# 1	practice	pre-trial	45
# 2	practice	post-trial	39
# 3	control	pre-trial	45
# 4	control	post-trial	46

sub.data <- sub.data %>% distinct(sub, tract_names, .keep_all = TRUE)

# get s1 data
s1.data <- sub.data %>% filter(session == 0)
s1.data$sub <- as.factor(s1.data$sub)
# because we lost some session 1 DTI data in the great back up miss of 2014, I am going to work out who does not have session 1 data, and I'll add their session 2 data to this dataframe
missed.subs <- unique(sub.data$sub)[!(unique(sub.data$sub) %in% unique(s1.data$sub))]
s1.data <- rbind(s1.data, sub.data[sub.data$sub %in% missed.subs, ])

summary <- sub.data %>% group_by(group) %>%
  summarise(N=length(unique(sub)))

# group
# N
# 1	practice	49
# 2	control	47

sub.data %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))
# This line of code aimed to count the number of NAs across
# multiple columns - apparently there are no missing values
# sub group session tract_start tract_end FA tract_names
# 1   0     0       0           0         0  0           0

which(sub.data$FA == 0.0000, arr.ind=TRUE)
# This line found FA values with a 0 value, I tried to add a group by function
# but I couldn't quite figure out how to do it


