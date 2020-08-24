## Written by K. Garner, 2020
## takes the practice behavioural data, plots it by condition (and group),
## fits power and exponential functions to the data per participant, 
## and then compares model fits, to find the best function
## for the data, then extracts coefficients for each participant

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
library(gridExtra)
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

# ----------------------------------------------------------------------------------
# for the multidata, collapse the single task trials over the task factor
clean.multi.data <- rbind(clean.multi.data %>% filter(mult_cond=="single") %>%
                          group_by(sub, cond_trial, cond_block) %>%
                          transmute(RT=mean(RT),
                          mult_cond="single"),
                    clean.multi.data %>% filter(mult_cond!="single"))
   

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
# we will average over each set of 24 trials (you can play with this value
# and see what effect it has when you plot it below)
# ----------------------------------------------------------------------------------
width = 24
roll.mu.multi <- clean.multi.data %>% arrange(sub, mult_cond, cond_trial) %>% group_by( sub, mult_cond ) %>%
                                      mutate( move_mu = rollmean(RT, mean, k=width, fill = NA))
roll.mu.multi$mult_cond <- factor(roll.mu.multi$mult_cond, levels=c("single", "multi-first", "multi-second"))

roll.mu.vis.search <- clean.vis.search.data %>% group_by( sub, cond ) %>%
  mutate( move_mu = rollmean( RT, mean, k=width, fill = NA))

# -----------------------------------------------------------------------------------
# plot rolling averages
roll.mu.multi%>% ggplot(aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
                  geom_line() + facet_wrap(~sub) 
# interesting how for the above, participants wither get better at first, more rapidly than
# second multitask, but for other participants, the learning curves are almost on top of
# each other (appears to separate serial from grouping strategies)
roll.mu.vis.search %>% ggplot(aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
                  geom_line() + facet_wrap(~sub)

# -----------------------------------------------------------------------------------
# MODELS AT THE SINGLE SUBJECT LEVEL
# Fitting exponential/power functions and comparing between the two

# wrt the construction of the pwr model:
# we are seeking to predict the effect of practice trials on response time. For each participant we fit both
# power and exponential models that allow the slope to change per condition.

# The power model we want to fit looks like this:
# y = constant * trial^(cond-coefficient)
# to linearise (i.e. so lm can fit the data), we can take the log of both sides, which in its most simple form, will look like the following (also applying
# the rule that multiplication = summation in log space), and the property that log(x^a) = a.log(x)
# log(y) = log(constant) + cond-coefficient.(log(trial))
# to convert this back into linear space, we do the following:
# 1) exponentiate both sides of the equation:
# e^log(y) = e^(log(constant) + cond-coefficient.(log(trial)))
# 2) exponential and logarithm are inverses so we can immediately simplift the left hand side of the equation
# y = e^(log(constant) + cond-coefficient.(log(trial)))
# 3) we can use property of the exponents to split up the coefficient on e
# y = e^(log(constant)) . e^(cond-coefficient.log(trial)) 
# 4) use a property of the logaritms to move the fixed and subject effects:
# y = e^(log(constant)) . e^log(trial)^cond-coefficient
# 5) simplify the first two terms, and then use the fact that the log and the exp are inverses
# y = N . trial^cond-coefficient 

# wrt to the construction of the exponential model:
# full model
# y = constant * cond-coefficient^trial
# note: now trial is the exponent
# and we can prepare for the linear fit as:
# log(y) = constant + (the fixed effect of trial) 
# thus the output parameters do not require any conversion back to linear space, as they are predicting a converted dv (y)

# source: http://msenux2.redwoods.edu/MathDept/R/TransformingData.php
# -----------------------------------------------------------------------------------
all.fits.multi.dat <- lapply(unique(roll.mu.multi$sub), fit.mod.sub.level.multidat, data=roll.mu.multi)
all.fits.VS.dat <- lapply(unique(roll.mu.vis.search$sub), fit.mod.sub.level.VS, data=roll.mu.vis.search)
all.fits.multi.dat <- do.call(rbind, all.fits.multi.dat)
all.fits.VS.dat <- do.call(rbind, all.fits.VS.dat)

# plot some fits at random for both groups
# -----------------------------------------------------------------------------------
plot.multi.fit(roll.mu.multi, all.fits.multi.dat, "145")
plot.VS.fit(roll.mu.vis.search, all.fits.VS.dat, "215")

# for each function and task, compute lowest RMSE
# -----------------------------------------------------------------------------------
multi.comp <- all.fits.multi.dat %>% group_by(model) %>% summarise(s=sum(RMSE))
VS.comp <- all.fits.VS.dat %>% group_by(model) %>% summarise(s=sum(RMSE))
# in both cases the power function has a lower RMSE 

# making a dataframe of the parameters, while also calculating the first derivative values
# -----------------------------------------------------------------------------------
pwr.params.multi <- all.fits.multi.dat %>% filter(model == "pwr") %>%
                          mutate(a = int, # for y = a x^b
                                 b = slp,
                                 deriv_a = int*slp,
                                 deriv_b = slp-1)

pwr.params.VS <- all.fits.VS.dat %>% filter(model == "pwr") %>%
                          mutate(a = int, # for y = a x^b
                                 b = slp,
                                 deriv_a = int*slp,
                                 deriv_b = slp-1)

# write to a csv file for later analysis
# -----------------------------------------------------------------------------------
write_csv(pwr.params.multi, paste(fpath, 'practiceGrp-pwr-coeffs.csv', sep='/'))    
write_csv(pwr.params.VS, paste(fpath, 'controlGrp-pwr-coeffs.csv', sep='/'))  