# This code was written to ensure the N in pre and post conditions remained the same
# also for the purpose of calculating correlations between sessions for each tract

#This is to be run before the exploratory or efa code.

# written by K. Garner and E. Geary, August 2020

rm(list=ls())
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load data
# --------------------------------------------------------------------------------
fpath <- '~/Documents/THESIS/pathways-to-practice/dti-data/'
tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Left"), # amend this to contain which tracts you seek
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Right"))

sub.data <- GetDTIData(fpath, tracts)
# relabel factors etc
sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-training", "post-training")

sub.data$tract_names <- c("lDLPFC_lACC", "rDLPFC_rACC", "lDLPFC_rACC", "rDLPFC_lACC",
                          "lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP",
                          "lACC_lLNP", "rACC_rLNP", "rACC_lLNP", "lACC_rLNP",
                          "lACC_lCN", "rACC_rCN", "rACC_lCN", "lACC_rCN")
                         

# KG note: you need to define tract names as a factor, if not this will lead you to errors later on
sub.data$tract_names <- as.factor(sub.data$tract_names)

sub.data$sub <- as.factor(sub.data$sub)

sub.data <- sub.data %>% unique()

#Correlation
#______________________________________________________________________________________________________

# KG: so here you are allocating your sub.data frame to a new variable called 'corrected_frame'
corrected_frame <- sub.data

# count how many participants you have per tract and per session
sub.count <- corrected_frame %>% group_by(session, tract_names) %>%
             summarise(N=length(unique(sub)))
#This creates a new vector with just the session and tract names and the N of each
#they're not the same so we need to check that

#This finds what the numbers of participants are in each session, so that
# we can use them to match up the participants between pre- and post- sessions
s1.subs <- unique(sub.data$sub[sub.data$session=='pre-training'])
s2.subs <- unique(sub.data$sub[sub.data$session=='post-training'])


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
  out <- with(frame_correlate, (cor.test(x=FA[session=='pre-training'], 
                                         y=FA[session=='post-training'])))
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

cors
