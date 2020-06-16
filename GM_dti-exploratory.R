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

sub.data$tract_names <- c("lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP",
                          "lDLPFC_lSOG", "rDLPFC_rSOG", "lDLPFC_rSOG", "rDLPFC_lSOG",
                          "lCN_lSOG", "rCN_rSOG", "lCN_rSOG", "rCN_lSOG",
                          "lTHA_lSOG", "rTHA_rSOG", "lTHA_rSOG", "rTHA_lSOG")


# 2. basic data check
# --------------------------------------------------------------------------------
# this will help you as a start https://rstudio-education.github.io/tidyverse-cookbook/transform-tables.html

sub.data %>%
  arrange(desc(FA))
# Highest FA recorded for rTHA_rSOG tract
sub.data %>%
  group_by(group, session) %>%
  summarise(avg = mean(FA))
# Created a tibble that represents the mean FA values separated by 
# group and session, revealed that the practice FA means are marginally
# higher than the control with a slight increase in FA post-trial across all tracts
summary(sub.data$FA)
# Quick summary of the FA variable, shows overall mean, minimum and maximum, etc.

agg = aggregate(sub.data,
                by = list(sub.data$group, sub.data$session),
                FUN = mean)
# I thought this line of code would be helpful to figure out mean FAs by group and session
# but it didn't seem to work
  
sub.data %>%
  count(session)
# 1880 data points pre-trial, 1760 post-trial

sub.data %>%
  count(group)
# 1820 points in practice, 1820 points in control

summary(sub.data)
# Put the above data into one line of code

sub.data %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))
# This line of code aimed to count the number of NAs across
# multiple columns - apparently there are no missing values

which(sub.data$FA == 0.0000) %>%
  group_by(sub.data$session)
# This line found FA values with a 0 value, I tried to add a group by function
# but I couldn't quite figure out how to do it

attach(sub.data)
mytable <- table(group,session) # A will be rows, B will be columns 
mytable # print table 
margin.table(mytable, 1)
group
margin.table(mytable, 2) # B frequencies (summed over A)
session
# Just a couple frequency tables and whatnot, Lizzy helped me out with these