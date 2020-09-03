# written by K. Garner and Georgia Marsh, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  # you don't need to run this line again once you have installed it the first time
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse") - again, not needed now
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load data
# --------------------------------------------------------------------------------

fpath <- 'C:/Git/pathways-to-practice/dti-data/' 
fpath <- 'dti-data/' # to make work on Kel's setup, comment out when using yourself
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
levels(sub.data$session) <- c("pre-trial", "post-trial") # there is a thread about this on the slack - a trial is a single
# instance of an observation in an experiment. To say something is 'pre-trial' suggests that it is taken from the time just
# preceeding a trial - (i.e. the intertrial interval). Can you give the levels of this factor a more appropriate name?

sub.data$tract_names <- c("lDLPFC_lCN", "lDLPFC_rCN", "rDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "lDLPFC_rLNP", "rDLPFC_rLNP", "rDLPFC_lLNP",
                          "lDLPFC_lSOG", "lDLPFC_rSOG", "rDLPFC_rSOG", "rDLPFC_lSOG",
                          "lCN_lSOG", "lCN_rSOG", "rCN_rSOG", "rCN_lSOG",
                          "lTHA_lSOG", "lTHA_rSOG", "rTHA_rSOG", "rTHA_lSOG")
sub.data$tract_names <- as.factor(sub.data$tract_names)

# 2. basic data check
# --------------------------------------------------------------------------------
# this will help you as a start https://rstudio-education.github.io/tidyverse-cookbook/transform-tables.html

summary(sub.data)
# Put the above data into one line of code -- KG: what do you mean by this?

summary <- sub.data %>% group_by(group, session) %>%
  summarise(N=length(unique(sub)))

# group
# session
# N
# 1	practice	pre-trial	45
# 2	practice	post-trial	39
# 3	control	pre-trial	45
# 4	control	post-trial	46

length(unique(sub.data)) == nrow(sub.data) # KG: length gives you the columns of sub.data, not the rows. As unique(sub.data) gives
# a dataframe that contains all the unique rows of sub.data, a safer, less bug prone way to do this would be:
nrow(unique(sub.data)) == nrow(sub.data)
#[1] FALSE

#The FALSE result tells me that there are some duplicates

sum(duplicated(sub.data)) # KG: nice, I didn't know about this function :)
#[1] 140
# KG: you could also put the total rows of sub.data here, so that you have all the information you need 
# to check that the line of code below works properly

# sub.data <- sub.data %>% distinct(sub, tract_names, .keep_all = TRUE) # KG: this should have been covered in the code Lizzy shared with
# you, we learned its better to use unique rather than distinct
sub.data <- sub.data %>% unique()

# get s1 data
s1.data <- sub.data %>% filter(session == 0) # KG: you named the levels of session above as 'pre-trial' and 'post-trial' so this line won't work
s1.data <- sub.data %>% filter(session == 'pre-trial')

s1.data$sub <- as.factor(s1.data$sub) # KG: you defined this as a factor already when you made the sub.data frame, no need to do it again

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
  select(everything()) %>% # KG: why are you selecting everything? seems like a redundant line of code to me, also why are you looking at sub.data when you made the dataframe s1.data?
  summarise_all(funs(sum(is.na(.))))
# This line of code aimed to count the number of NAs across
# multiple columns - apparently there are no missing values
# sub group session tract_start tract_end FA tract_names
# 1   0     0       0           0         0  0           0

which(sub.data$FA == 0.0000, arr.ind=TRUE)
# This line found FA values with a 0 value, I tried to add a group by function
# but I couldn't quite figure out how to do it

# Basic Descriptive Statistics:

summary(s1.data)
# FA before removing 0 values
# Min. : 0.0000
# 1st Qu. : 0.3589
# Median : 0.4178
# Mean : 0.3463
# 3rd Qu. : 0.4572
# Max. : 0.6133

summary(s1.data)
# FA after removing 0 values
# Min. : 0.2963
# 1st Qu. : 0.3918
# Median : 0.4340
# Mean : 0.4335
# 3rd Qu. : 0.4653
# Max. : 0.6133

# Univariate Outliers
# Boxplot:
ggplot(s1.data, aes(FA, tract_names)) + 
  geom_boxplot(notch = TRUE)

# Boxplot showed that the majority of the tracts are relatively normally distributed 
# (rDLPFC_rCN, rDLPFC_lLNP, rDLPFC_lCN, rCN_rSOG, lTHA_rSOG, lTHA_lSOG, lDLPFC_rLNP, lDLPFC_lLNP, lDLPFC_lCN)
# Some tracts visibly skewed (rDLPFC_rSOG, rDLPFC_rSOG, rDLPFC_rLNP, lDLPFC_rCN, lDLPFC_lSOG, lCN_lSOG)
# Some tracts don't really make much sense (rTHA_lSOG, rDLPFC_lSOG, rCN_lSOG, lDLPFC_rSOG, lCN_rSOG)

# Ran boxplot again after removing outleirs and 0 values, majority of data still looks normally distributed,
# some skew to the right (higher values)

s1.data [(s1.data$FA < mean(s1.data$FA) - 3*sd(s1.data$FA)) | (s1.data$FA > mean(s1.data$FA) + 3*sd(s1.data$FA)),]

s1.data <- s1.data %>% filter(FA > 0)
# Retains any FA values above 0

s1.data <- s1.data %>% filter(FA < (mean(FA) + (3*sd(FA))))
# Filters out any FA outliers + 3*sd

with(s1.data, sum(FA > mean(FA)+(3*sd(FA))))
# Shows the number of outliers remaining in dataset (0)

mean(s1.data$FA) - 3*sd(s1.data$FA)
# 0.280798 (sanity check)
mean(s1.data$FA) + 3*sd(s1.data$FA)
# 0.5850062 (sanity check)

# QQ plot:
ggplot(s1.data, aes(sample = FA, colour = factor(tract_names))) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ tract_names, nrow = NULL)
# Most tracts look good, data points follow the lines closely. 
# Questionable tracts: lCN_rSOG, rCN_lSOG (data points appear quite sparse)

# Correlations:
# KG: so here you are allocating your sub.data frame to a new variable called 'corrected_frame'
corrected_frame <- sub.data

# count how many participants you have per tract and per session
sub.count <- corrected_frame %>% group_by(session, tract_names) %>%
  summarise(N=length(unique(sub)))
#This creates a new vector with just the session and tract names and the N of each
#they're not the same so we need to check that

#This finds what the numbers of participants are in each session, so that
# we can use them to match up the participants between pre- and post- sessions
s1.subs <- unique(sub.data$sub[sub.data$session=='pre-trial'])
s2.subs <- unique(sub.data$sub[sub.data$session=='post-trial'])


# now we have a list of subjects who are in each session, we now work out 
# which subs are in both sessions, they are the ones we will keep
substokeep <- s1.subs %in% s2.subs # gives us a true/false vector letting us know if each sub that is in sess 1 is also in sess 2
subtokeep1 <- s1.subs [substokeep] # we use that true/false vector to retain the subs for which that condition is true
# the output is a vector of the sub numbers that appear in both sessions

# we only want to keep the subs that appear in both so this line creates a new vector
#that includes those that fall into both sessions and keeps those and removes those who
#only have one session data - if you console it it'll show you which ones are not in both

# KG note: given that you've already made a dataframe called corrected frame, 
# we may as well keep updating that (makes it easier to track and reduces chance of error)
# corrected_frame <- sub.data %>% filter(sub %in% subtokeep1) 
corrected_frame <- corrected_frame %>% filter(sub %in% subtokeep1)

#This then filters the subs that have s1 and s2 data into the new version

sub.count <- corrected_frame %>% group_by(session, tract_names) %>%
  summarise(N=length(unique(sub)))

#This is now applying the new cleaned data to the sub.count variable

# KG note: this below line is not needed - let me know if you can work out why 
# it doesn't provide the test we want
# with(corrected_frame, cor.test(FA[session=='pre-training'],
#                         FA[session=='post-training']))

#  KG: Note which function do you mean here?
# This functions as a check to make sure that the s1 and s2 are the same - which they are
# also - this function has round brackets where it should have square ones
# getcorrelation <- function(data, tract_name){
#   frame_correlate <- data(data$tract_names==tract_name,)
#   out <- with(frame_correlate, (cor.test(x=FA[session=='pre-training'], 
#                                   y=FA[session=='post-training'])))
#   out
# }
getcorrelation <- function(data, tract_name){
  frame_correlate <- data[data$tract_names==tract_name,]
  out <- with(frame_correlate, (cor.test(x=FA[session=='pre-trial'], 
                                         y=FA[session=='post-trial'])))
  out
}

# So this now is creating a correlation function called 'frame_correlate' for the individual tract
#separately, this is then used to correlate pre and post FA and this is put into a function 'out'
#where when you type it into the console it'll give you the correlation (getcorrelation also
#requires there to be the data and the tract name to work ie. wont work without both)

uniquetracts <- unique(corrected_frame$tract_names)

#this now lists the tracts that we want to correlate, effectively pulling them out so we
#can easily apply the next function

# KG note: this line was a test line we wrote on the day, you 
# don't need it because you are repeating the same thing below
#lapply(uniquetracts, getcorrelation, data=corrected_frame)

cors <- lapply(uniquetracts, getcorrelation, data=corrected_frame)

#This is then using the 'lapply' function to test the correlations of the pre and post
#for each of the individual tracts. This is also saved into the function 'cors' so that 
#you can just type cors into the console and it'll print it out without having to run the 
#full thing every time



# Raincloud Plot:


install.packages("cowplot")

library(cowplot)

ggplot(s1.data, aes(x=tract_names, y=FA, fill = tract_names, colour = tract_names)) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                     TRUE) +
  geom_point(position=position_jitter(width=.15), size=.25) +
  geom_boxplot(aes(x = tract_names, y = FA), outlier.shape = NA,
               alpha = 0.3, width = .1, colour = "BLACK") +
  scale_y_continuous(limits=c(0.2,0.6)) + coord_flip() +
  ylab('FA') + xlab('connection') + theme_cowplot() + 
  guides(fill = FALSE, colour = FALSE) +
  theme(axis.title.x = element_text(face = "italic")) +
  ggtitle("Raincloud w/ Boxplots")
  ggsave("Raincloud.png", width = 20, height = 30)

# Nonlinearity and Heteroscedasticity:
  
  # use tidyverse functionality to convert from long to wide, drop people who have an NA value on some measure
s1.dat.wide <- s1.data %>% select(-c(group, session, tract_start, tract_end)) %>%
    pivot_wider(id_cols=sub, names_from=tract_names, values_from=FA) %>%
    drop_na()
  # apply the pairs() function to my new dataframe - see https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/pairs for function documentation 
s1.data %>% select(-sub) %>% pairs()
