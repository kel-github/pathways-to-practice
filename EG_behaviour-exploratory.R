# written by K. Garner and Elizabeth Greary, 2020
# this code reads in the behavioural data, tidies it, and performs an exploratory
# and descriptive data analysis.

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  # uncomment and run if you don't already have this package installed
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse")  # uncomment and run if you don't have tidyverse 
# installed already
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load raw data and tidy up by labelling factors
# --------------------------------------------------------------------------------
fpath  <-  # path to data
raw.data  <- GetPrePostDataRaw(fpath)


# calculate accuracy using tidyverse functions:
# group_by https://dplyr.tidyverse.org/reference/group_by.html
# summarise https://dplyr.tidyverse.org/reference/summarise.html
# --------------------------------------------------------------------------------






# are we setting a minimum accuracy criteria?
# --------------------------------------------------------------------------------


# now load the cleaned RT data (all correct responses and all data > +/- 2.5 stdevs from the
# mean for that participant in that condition), recode the data so that trials are labelled
# as either single task, or the first task performed on multitask trials
# --------------------------------------------------------------------------------
clean.data <- GetPrePostClean(fpath)
clean.data <- RecodeMultiData(clean.data)

# now filter out participants who scored below the minimum accuracy criteria, using
# the tidyverse filter function (see https://r4ds.had.co.nz/transform.html section 5.2)
# label, recode and where necessary, reorder factors of the dataframe
# --------------------------------------------------------------------------------





# by subject, sess, condition, use group_by and summarise to compute the mean, variance, 
# signal to noise ratio (mean/std) and coefficient of variation (std/mean)
# https://en.wikipedia.org/wiki/Signal-to-noise_ratio (see the alternative definition)
# https://www.investopedia.com/terms/c/coefficientofvariation.asp
# ----------------------------------------------------------------------------------





# perform visual data checks of the three variables, using only the session 1 data,
# using boxplots and qqplots. Are there any outliers? Any large deviations from a
# normal distribution?
# ----------------------------------------------------------------------------------



# exclude outliers
# ----------------------------------------------------------------------------------



# from our examination of the data, we chose [x] as our DV of interest. Now compute the 
# ratio of each multitask condition to the single task condition (multi-x / single) using
# the group_by and summarise functions
# ----------------------------------------------------------------------------------




# reliability analysis - correlate our resulting measures between session 1 & 2, 
# for the control group only
# ----------------------------------------------------------------------------------





# create a raincloud plot of our final measure (from the session 1 data, by condition)
# ----------------------------------------------------------------------------------





# save the final measures from the session 1 data as a csv file
# ----------------------------------------------------------------------------------