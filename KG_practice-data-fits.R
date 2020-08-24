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
# Fitting exponential/power functions and comparing between the two

# wrt the construction of the pwr model:
# we are seeking to predict the effect of practice trials on response time. We construct a model that allows the 
# function that describes the effect of going from the single to the multitask conditions as a random effect that
# varies from participant to participant. We assume a fixed effect for the single task trial (with an intercept that 
# varies across subjects), and we ask what the co-efficient is that allows us to predict, for each subject, their RT
# in the multitask conditions (first multitask trial)/second multitask trial.

# The power model we want to fit looks like this:
# y = constant * subject constant * trial^(the fixed effect + the effect of being that subject in that multitask condition)
# which we can simplify to:
# y = constant * subject constant * trial^fixed effect * trial^subject condition effect
# to linearise (i.e. so lm can fit the data), we can take the log of both sides, which in its most simple form, will look like the following (also applying
# the rule that multiplication = summation in log space), and the property that log(x^a) = a.log(x)
# log(y) = log(constant) + log(subject constant) + fixed effect.log(trial) + sub-cond-effect.(log(trial))
# to convert this back into linear space, we do the following:
# 1) exponentiate both sides of the equation:
# e^log(y) = e^(log(constant) + log(subject constant) + fixed effect.log(trial) + sub-cond-effect.(log(trial)))
# 2) exponential and logarithm are inverses so we can immediately simplift the left hand side of the equation
# y = e^(log(constant) + log(subject constant) + fixed effect.log(trial) + sub-cond-effect.(log(trial)))
# 3) we can use property of the exponents to split up the coefficient on e
# y = e^(log(constant)) . e^(log(subject constant)) . e^(fixed effect.log(trial)) . e^(sub-cond-effect.(log(trial))))
# 4) use a property of the logaritms to move the fixed and subject effects:
# y = e^(log(constant)) . e^(log(subject constant)) . e^log(trial)^fixedeffect . e^log(trial)^sub_cond_effect
# 5) simplify the first two terms, and then use the fact that the log and the exp are inverses
# y = N . N . trial^fixedeffect . trial^sub.cond.effect

# wrt to the construction of the exponential model:
# full model
# y = constant * subject constant * (the fixed effect + the effect of being that subject in that multitask condition)^trial
# note: now trial is the exponent
# which we can also write as:
# y = constant * subject constant * (the fixed effect)^trial * (the effect of being that subject in that multitask condition)^trial
# and we can prepare for the linear fit as:
# log(y) = constant * subject constant * (the fixed effect of trial) * (the effect of being that subject in that multitask condition by trial)
# thus the output parameters do not require any conversion back to linear space, as they are predicting a converted dv (y)

# source: http://msenux2.redwoods.edu/MathDept/R/TransformingData.php
roll.mu.multi <-  roll.mu.multi %>% na.omit()
roll.mu.multi$cond_trial_rsc = roll.mu.multi$cond_trial/100 # rescaling cond_trial -> https://rpubs.com/jimsavage/scale_issues
exp.model.MT <- lmer(log(move_mu) ~ cond_trial_rsc + (1+cond_trial_rsc:mult_cond|sub), data=roll.mu.multi, REML=FALSE)
pwr.model.MT <- lmer(log(move_mu) ~ log(cond_trial_rsc) + (1:log(cond_trial_rsc):mult_cond|sub), data=roll.mu.multi, REML=FALSE)
anova(exp.model.MT,pwr.model.MT)
summary(exp.model.MT)


## plotting data from a couple of randomly selected subjects, along with the model fits
sim.result.and.plot <- function(sub.num, model, data){
  # given the subject number, a lmer mixed model object, and the dataframe on which 
  # the model was applied, predict a new set of results and plot against the participant's
  # own data
  nu.data = data %>% select(sub, cond_trial_rsc, mult_cond, move_mu) %>% filter(sub == sub.num)
  nu.data$fitted = predict(model, newdata=nu.data)
  nu.data$fitted = exp(nu.data$fitted)
  nu.data %>% ggplot(aes(x=cond_trial_rsc, y=fitted, group=cond, color=cond)) +
                     geom_point() + geom_line(aes(x=cond_trial_rsc, y=fitted, group=cond, color=cond))
  # step 1 sanity check = do the predictions by hand, given the coefficients in the model 
  nu.data$hand.fitted = NA
  x = unique(nu.data$cond_trial_rsc)
  nu.data$hand.fitted[nu.data$mult_cond == "single" & nu.] = exp( -0.1324032) * exp(-0.31501126) * exp(-0.0259306049*log(x))
}


roll.mu.vis.search <-  roll.mu.vis.search %>% na.omit()
roll.mu.vis.search$cond_trial_rsc = roll.mu.vis.search$cond_trial/100 
exp.model.VS <- lmer(log(move_mu) ~ cond_trial_rsc + (1+cond_trial_rsc:cond|sub), data=roll.mu.vis.search, REML=FALSE)
pwr.model.VS <- lmer(log(move_mu) ~ log(cond_trial_rsc) + (1+log(cond_trial_rsc):cond|sub), data=roll.mu.vis.search, REML=FALSE)
anova(exp.model.VS,pwr.model.VS)

# -----------------------------------------------------------------------------------
# plot residuals
plot(exp.model.MT)
plot(pwr.model.VS)
# http://docs.statwing.com/interpreting-residual-plots-to-improve-your-regression/
# residuals are basically ok

# -----------------------------------------------------------------------------------
# power model is better under both circumstances
# http://msenux2.redwoods.edu/MathDept/R/TransformingData.php


# -----------------------------------------------------------------------------------
# comments on the first derivative


# -----------------------------------------------------------------------------------
# for each model, extract the exponent parameters 
# because its a power function, the first derivative is just the same as the function
prac.coefs <- rbind(get.coef.exps(pwr.model.MT), get.coef.exps(pwr.model.VS))
write_csv(prac.coefs, paste(fpath, 'practice-pwr-coeffs.csv', sep='/'))    
