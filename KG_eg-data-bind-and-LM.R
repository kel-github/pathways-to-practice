# Written by K. Garner, 2020
# Example code to load two CSV files and bind the datasets, save a csv file, and

rm(list=ls())

# Load packages, source function files and define path variables
library(tidyverse)
library(cowplot)
library(interactions)
source("R_rainclouds.R")
source("KG_data-wrangling.R")
tract_data = 'dti-data/KG_2factSol_subdata.csv'
fpath <- paste(getwd(), 'cleaned-data', sep='/')# path for attaining data
CV.dat <- read.csv(paste(fpath,"CV-all-subs.csv", sep="/"))
## --------------------------------------------------------------------------


# load behavioural data and relabel
CV.dat <- read.csv(paste(fpath, "CV-all-subs.csv", sep="/"))
CV.dat$group <- NA
CV.dat$group[CV.dat$sub < 200] = "practice"
CV.dat$group[CV.dat$sub > 199] = "control"
CV.dat$group <- factor(CV.dat$group, levels=c("practice", "control"))
CV.dat$sub <- as.factor(CV.dat$sub)
CV.dat <- CV.dat %>% filter(sess == "Pre")


## --------------------------------------------------------------------------
# add in the tract data and tidy up
tract.dat <- read.csv(tract_data) %>% select(-X) 
tract.dat$sub <- as.factor(tract.dat$sub)
reg.dat <- inner_join(CV.dat, tract.dat, by=c('sub')) %>%
  unique() %>% na.omit
reg.dat$sub <- as.factor(reg.dat$sub)

## --------------------------------------------------------------------------
# save the new dataframe to a cvs file
write.csv(reg.dat, paste(fpath, "KG_example.csv", sep="/"), row.names=FALSE)

## --------------------------------------------------------------------------
# run a linear regression on the data from the single task condition
mod <- lm(CV ~ cort_to_Put*cort_to_CN, data=reg.dat[reg.dat$mult_cond == "single", ])
# print the output
summary(mod)
# Also see Chapter 15 of Navarro's LSR - https://learningstatisticswithr.com/lsr-0.6.pdf