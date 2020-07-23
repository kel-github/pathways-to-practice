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

# for each task, calculate accuracy for each subject, block and condition using tidyverse functions:
# group_by https://dplyr.tidyverse.org/reference/group_by.html
# summarise https://dplyr.tidyverse.org/reference/summarise.html
# --------------------------------------------------------------------------------




# are we setting a minimum accuracy criteria?
# --------------------------------------------------------------------------------



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





# by subject, block, condition, use group_by and summarise to compute the mean
# and std deviation
# ----------------------------------------------------------------------------------





# for each task, poarticipant, block and condition, perform visual data checks of 
# data using boxplots and qqplots. 
# Are there any outliers? Any large deviations from a
# normal distribution?
# ----------------------------------------------------------------------------------




# exclude outliers
# ----------------------------------------------------------------------------------





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


# now select 10 subjects from each task at random, and plot, for each participant, a line graph of 
# their moving average RT, with trials on the x-axis, moving mu on the y-axis. 
# You want a separate line for each condition and a shaded area around the line to indicate standard error
# e.g. http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
# e.g. https://ggplot2.tidyverse.org/reference/geom_ribbon.html
# ----------------------------------------------------------------------------------






# save the final measures from each task into their own csv files into the clean data folder
# ----------------------------------------------------------------------------------