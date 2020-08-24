# written by K. Garner and Georgia Marsh, 2020
# this code reads in the practice behavioural data, tidies it, and performs an exploratory
# and descriptive data analysis.

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
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load raw data and tidy up by labelling factors
# --------------------------------------------------------------------------------
fpath  <-  paste(getwd(), 'raw-behav-data', sep='/')# path to data
raw.multi.data <- GetPracticeDataMulti(fpath)
raw.vis.search.data <- GetPracticeDataVisSearch(fpath)

raw.multi.data$cond_block <- as.factor(raw.multi.data$cond_block)
raw.multi.data$cond_trial <- as.factor(raw.multi.data$cond_trial)
raw.multi.data$cond <- as.factor(raw.multi.data$cond) # KG: would be good to label these meaningfully - 1= shape single, 2 = sound single, 3 = multitask
raw.multi.data$sub <- as.factor(raw.multi.data$sub)

raw.vis.search.data$cond_trial <- as.factor(raw.vis.search.data$cond_trial)
raw.vis.search.data$cond_block <- as.factor(raw.vis.search.data$cond_block)
raw.vis.search.data$cond <- as.factor(raw.vis.search.data$cond)
raw.vis.search.data$sub <- as.factor(raw.vis.search.data$sub)

# for each task, calculate accuracy for each subject, block and condition using tidyverse functions:
# group_by https://dplyr.tidyverse.org/reference/group_by.html
# summarise https://dplyr.tidyverse.org/reference/summarise.html
# --------------------------------------------------------------------------------

multi.accuracy <- raw.multi.data %>% 
  group_by(sub, cond, cond_block) %>%
  summarise(accu = mean(acc))

vis.accuracy <- raw.vis.search.data %>% 
  group_by(sub, cond, cond_block) %>%
  summarise(accu = mean(acc))

multi.group.accuracy <- multi.accuracy %>%
  group_by(cond, cond_block) %>%
  summarise(group.accu = mean(accu))

vis.group.accuracy <- vis.accuracy %>%
  group_by(cond, cond_block) %>%
  summarise(group.accu = mean(accu))

# are we setting a minimum accuracy criteria?
# --------------------------------------------------------------------------------

# Minimum accuracy criteria of 70% accuracy as per Garner, Lynch & Dux (2016)

##### KG: here is the point where you need to define which subjects you will
# exclude based on the minimum accuracy criteria
# There is an example of this kind of process in GM_s1data_code.R



# -------------------------------------------------------------------------------
# now clean the RT data (all correct responses and all data > 200 ms or < + 2.5 stdevs from the
# mean for that participant in that condition, and that block), 
# for the multitask data, recode the data so that trials are labelled
# as either single task, or the first task performed on multitask trials
# --------------------------------------------------------------------------------
clean.multi.data <- GetPracticeMultiClean(raw.multi.data)
clean.multi.data <- RecodePracticeMultiData(clean.multi.data)
clean.vis.search.data <- GetPracticeVisSearchClean(raw.vis.search.data)

# KG: for the multidata, collapse the single task trials over the task factor
clean.multi.data <- rbind(clean.multi.data %>% filter(mult_cond=="single") %>%
                            group_by(sub, cond_trial, cond_block) %>%
                            transmute(RT=mean(RT),
                                      mult_cond="single"),
                          clean.multi.data %>% filter(mult_cond!="single"))

# now filter out participants who scored below the minimum accuracy criteria, using
# the tidyverse filter function (see https://r4ds.had.co.nz/transform.html section 5.2)
# label, recode and where necessary, reorder factors of the dataframe
# --------------------------------------------------------------------------------
# KG: you will need to use the dataframe of subject numbers that you would have 
# defined above  

# KG: the below code won't work because 'clean.multi.data' etc does not contain any
# error trials, i.e. all the incorrect responses have been removed so that we can 
# get condition averages for the correct trials only

#filter.multi <- clean.multi.data %>% filter(acc > 0.7)
#filter.vis <- clean.vis.search.data %>% filter(acc > 0.7)
# There are no data points to remove as the previous code did this for us

# by subject, block, condition, use group_by and summarise to compute the mean
# and std deviation
# ----------------------------------------------------------------------------------

summary.multi <- clean.multi.data %>%
  group_by(sub, cond, cond_block) %>%
  summarise(mean(RT), sd(RT))
summary.vis <- clean.vis.search.data %>%
  group_by(sub, cond, cond_block) %>%
  summarise(mean(RT), sd(RT))


# for each task, poarticipant, block and condition, perform visual data checks of 
# data using boxplots and qqplots. 
# Are there any outliers? Any large deviations from a
# normal distribution?
# ----------------------------------------------------------------------------------

# KG: apologies for including participant above. What you want to do is plot boxplots for 
# block x condition, for the multitask, and the visual search task
# for example:
clean.multi.data %>% ggplot(aes(y=RT, group=cond)) +
                     geom_boxplot(notch=TRUE) +
                     facet_wrap(~cond_block)

# Boxplots:
# Sub X Multitask RT
# ggplot(clean.multi.data, aes(RT, sub)) + 
#   geom_boxplot(notch = TRUE)
# # Task X Multitask RT
# ggplot(clean.multi.data, aes(RT, task)) + 
#   geom_boxplot(notch = TRUE)
# # Block X Multitask RT
# ggplot(clean.multi.data, aes(RT, cond_block)) + 
#   geom_boxplot(notch = TRUE)
# # Cond X Multitask RT
# ggplot(clean.multi.data, aes(RT, mult_cond)) + 
#   geom_boxplot(notch = TRUE)
# # Sub X Visual RT
# ggplot(clean.vis.search.data, aes(RT, sub)) + 
#   geom_boxplot(notch = TRUE)
# # Block X Visual RT
# ggplot(clean.vis.search.data, aes(RT, cond_block)) + 
#   geom_boxplot(notch = TRUE)
# # Cond X VIsual RT
# ggplot(clean.vis.search.data, aes(RT, cond)) + 
#   geom_boxplot(notch = TRUE)
# 
# # QQ Plots:
# # Task X Multitask QQ
# ggplot(clean.multi.data, aes(sample = RT, colour = factor(task))) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~ task, nrow = NULL)
# # Cond X Multitask QQ
# ggplot(clean.multi.data, aes(sample = RT, colour = factor(mult_cond))) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~ mult_cond, nrow = NULL)
# # Block X Visual QQ
# ggplot(clean.vis.search.data, aes(sample = RT, colour = factor(cond_block))) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~ cond_block, nrow = NULL)
# # Cond X Visual QQ
# ggplot(clean.vis.search.data, aes(sample = RT, colour = factor(cond))) +
#   stat_qq() +
#   stat_qq_line() +
#   facet_wrap(~ cond, nrow = NULL)

# exclude outliers
# ----------------------------------------------------------------------------------

# KG: examining the plots and data, I think we are losing too much data with this
# criteria. As we excluded outliers at the level of each participant above, lets leave
# this step.

# outliers.multi <- clean.multi.data %>% group_by(cond) %>%
#   filter(((RT - mean(RT))/sd(RT)) > 3)
# outliers.multi
# 
# outliers.vis <- clean.vis.search.data %>% group_by(cond) %>%
#   filter(((RT - mean(RT))/sd(RT)) > 3)
# outliers.vis
# 
# clean.multi.data <- clean.multi.data %>% filter(RT < (mean(RT) + (3*sd(RT))))
# # Filters out any RT outliers + 3*sd
# clean.multi.data <- clean.multi.data %>% filter(RT > (mean(RT) - (3*sd(RT))))
# # Filters out any RT outliers - 3*sd
# clean.vis.search.data <- clean.vis.search.data %>% filter(RT < (mean(RT) + (3*sd(RT))))
# # Filters out any RT outliers + 3*sd
# clean.vis.search.data <- clean.vis.search.data %>% filter(RT > (mean(RT) - (3*sd(RT))))
# # Filters out any RT outliers - 3*sd

# now we are going to compute a moving average across trials, for each participant
# and condition, this is what we will plot and fit our functions to
# https://en.wikipedia.org/wiki/Moving_average
# we will average over each set of 12 trials (you can play with this value
# and see what effect it has when you plot it below)
# ----------------------------------------------------------------------------------
width = 24
roll.mu.multi <-  clean.multi.data %>% group_by( sub, mult_cond ) %>%
                                       mutate( move_mu = rollmean(RT, mean, k = width, fill = NA ) )
                 

roll.mu.vis.search <- clean.vis.search.data %>% group_by( sub, cond ) %>%
                                                mutate( move_mu = rollmean( RT, mean, k=width, fill = NA))


# now select 10 subjects from each task at random, and plot, for each participant, a line graph of 
# their moving average RT, with trials on the x-axis, moving mu on the y-axis. 
# You want a separate line for each condition and a shaded area around the line to indicate standard error
# e.g. http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# e.g. https://ggplot2.tidyverse.org/reference/geom_ribbon.html
# ----------------------------------------------------------------------------------

# KG: how about something like:
subs <-  sample(clean.multi.data$sub, 10)
plot.dat <- roll.mu.multi %>% filter(sub %in% subs)
# Multitask Graph:
ggplot(data=plot.dat, aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  facet_wrap(~ sub)



# save the final measures from each task into their own csv files into the clean data folder
# ----------------------------------------------------------------------------------