## Written by K. Garner, 2020
## takes the practice behavioural data, plots it by condition (and group),
## fits power and exponential functions to the data per participant, 
## and then takes the average RMSE across participants, to find the best function
## for the data

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  # uncomment and run if you don't already have this package installed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse")  # uncomment and run if you don't have tidyverse 
# installed already
# install.packages("zoo")
library(tidyverse)
library(zoo)
library(lme4)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load raw data and tidy up by labelling factors
# --------------------------------------------------------------------------------
fpath  <-  paste(getwd(), 'raw-behav-data', sep='/')# path to data
raw.multi.data <- GetPracticeDataMulti(fpath)
raw.multi.data$sub <- as.factor(raw.multi.data$sub)
raw.multi.data$cond_block <- as.factor(raw.multi.data$cond_block)
raw.multi.data$cond <- as.factor(raw.multi.data$cond)

raw.vis.search.data <- GetPracticeDataVisSearch(fpath)
raw.vis.search.data$sub <- as.factor(raw.vis.search.data$sub)
raw.vis.search.data$cond_block <- as.factor(raw.vis.search.data$cond_block)
raw.vis.search.data$cond <- as.factor(raw.vis.search.data$cond)

# for each task, calculate accuracy for each subject, block and condition using tidyverse functions:
# --------------------------------------------------------------------------------
mult.acc <- raw.multi.data %>% group_by(sub, cond_block, cond) %>%
            summarise(Acc=mean(acc)) %>%
            ggplot(aes(x=cond, y=Acc)) +
            geom_boxplot(notch=TRUE) +
            facet_wrap(~cond_block)

vis.acc <- raw.vis.search.data %>% group_by(sub, cond_block, cond) %>%
           summarise(Acc=mean(acc)) %>%
           ggplot(aes(x=cond, y=Acc)) +
           geom_boxplot(notch=TRUE) +
           facet_wrap(~cond_block)

# are we setting a minimum accuracy criteria?
# --------------------------------------------------------------------------------
# visual inspection of the above plots, plus want them to be above chance, so setting
# criterion of .75


# now clean the RT data (all correct responses and all data > 200 ms or < + 2.5 stdevs from the
# mean for that participant in that condition, and that block), 
# for the multitask data, recode the data so that trials are labelled
# as either single task, or the first task performed on multitask trials
# --------------------------------------------------------------------------------
clean.multi.data <- GetPracticeMultiClean(raw.multi.data)
clean.multi.data <- RecodePracticeMultiData(clean.multi.data)
clean.vis.search.data <- GetPracticeVisSearchClean(raw.vis.search.data)


# now filter out participants who scored below the minimum accuracy criteria, using
# the tidyverse filter function (see https://r4ds.had.co.nz/transform.html section 5.2)
# label, recode and where necessary, reorder factors of the dataframe
# --------------------------------------------------------------------------------
multi.excl <- mult.acc$data %>% filter(Acc<.75) %>%
              summarise(excl=unique(sub)) %>%
              select(sub) %>%
              unique() 
              
clean.multi.data <- clean.multi.data %>% filter(!sub %in% multi.excl$sub )

vis.search.excl <- vis.acc$data %>% filter(Acc<.75) %>%
                   summarise(excl=unique(sub)) %>%
                   select(sub) %>%
                   unique()

clean.vis.search.data <- clean.vis.search.data %>% filter(!sub %in% vis.search.excl$sub)


# for each task, poarticipant, block and condition, perform visual data checks of 
# data using boxplots and qqplots. 
# Are there any outliers? Any large deviations from a
# normal distribution?
# ----------------------------------------------------------------------------------
mult.qq <- clean.multi.data %>% 
           ggplot(aes(sample=RT)) +
           stat_qq() + stat_qq_line() +
           facet_wrap(~cond_block*mult_cond)
# eek - all is very not normal!
vis.qq <- clean.vis.search.data %>% 
          ggplot(aes(sample=RT)) +
          stat_qq() + stat_qq_line() +
          facet_wrap(~cond_block*cond)
# same again! Will try fitting power functions by subject and see what happens with the 
# parameters that we get out

# -----------------------------------------------------------------------------------

# now we are going to compute a moving average across trials, for each participant
# and condition, this is what we will plot and fit our functions to
# https://en.wikipedia.org/wiki/Moving_average
# we will average over each set of 12 trials (you can play with this value
# and see what effect it has when you plot it below)
# ----------------------------------------------------------------------------------
width = 24
roll.mu.multi <- rbind(     clean.multi.data %>% filter(mult_cond == "single") %>%
                              group_by( sub, cond ) %>%
                              mutate( move_mu = rollmean(RT, mean, k = width, fill = NA ) ),
                            clean.multi.data %>% filter(mult_cond != "single") %>%
                              group_by( sub, mult_cond ) %>%
                              mutate( move_mu = rollmean(RT, mean, k=width, fill = NA)))


roll.mu.vis.search <- clean.vis.search.data %>% group_by( sub, cond ) %>%
  mutate( move_mu = rollmean( RT, mean, k=width, fill = NA))

# -----------------------------------------------------------------------------------
# plot rolling averages
roll.mu.multi %>% ggplot(aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
                  geom_line() + facet_wrap(~sub) 
roll.mu.vis.search %>% ggplot(aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
                  geom_line() + facet_wrap(~sub)


# -----------------------------------------------------------------------------------
# Fitting exponential/power functions and comparing between the two
roll.mu.multi <-  roll.mu.multi %>% na.omit()
exp.model.MT <- lmer(log(move_mu) ~ cond_trial + (1+cond|sub), data=roll.mu.multi)
pwr.model.MT <- lmer(log(move_mu) ~ log(cond_trial) + (1+cond|sub), data=roll.mu.multi)
anova(exp.model.MT,pwr.model.MT)

roll.mu.vis.search <-  roll.mu.vis.search %>% na.omit()
exp.model.VS <- lmer(log(move_mu) ~ cond_trial + (1+cond|sub), data=roll.mu.vis.search )
pwr.model.VS <- lmer(log(move_mu) ~ log(cond_trial) + (1+cond|sub), data=roll.mu.vis.search )
anova(exp.model.VS,pwr.model.VS)

# -----------------------------------------------------------------------------------
# plot residuals
plot(pwr.model.MT)
plot(pwr.model.VS)
# http://docs.statwing.com/interpreting-residual-plots-to-improve-your-regression/
# residuals are basically ok

# -----------------------------------------------------------------------------------
# power model is better under both circumstances
# http://msenux2.redwoods.edu/MathDept/R/TransformingData.php

# I compute the conversion from log to exponential as
# y = exp(intercept) * trial_num^coeff * exp(cond_coeff)
# proof
# if log(y) = intercept + beta*log(x) + c
# then y should = e^int * x^beta * e^c
intercept = .4
beta = .5
x = 2
c = .4
ly = intercept + beta*log(x) + c
exp(ly)
# 3.14739
exp(intercept)*x^beta*exp(c)
# 3.14739
# :)

# -----------------------------------------------------------------------------------
# for each model, extract the exponent parameters 
# because its a power function, the first derivative is just the same as the function
prac.coefs <- rbind(get.coef.exps(pwr.model.MT), get.coef.exps(pwr.model.VS))
write_csv(prac.coefs, paste(fpath, 'practice-pwr-coeffs.csv', sep='/'))    
