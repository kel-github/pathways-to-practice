# Correlations code, ran after the initial s1data_code has been run but before converting the s1.data
# into wide-format:
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
corrected_frame <- corrected_frame %>% unique()
# Removed duplicates
cors <- lapply(uniquetracts, getcorrelation, data=corrected_frame)

#This is then using the 'lapply' function to test the correlations of the pre and post
#for each of the individual tracts. This is also saved into the function 'cors' so that 
#you can just type cors into the console and it'll print it out without having to run the 
#full thing every time

# Effect size calculation (Hedge's g, translatable to Cohen's d):
install.packages("esc")
library(esc)
esc_rpb(r = 0.1951094, grp1n = 1580, grp2n = 1580, es.type = "g")
# Effect Size:   0.3978
# Standard Error:   0.0359
# Variance:   0.0013
# Lower CI:   0.3274
# Upper CI:   0.4682
# Weight: 774.6715

# Power analyses to determine sample size required:
install.packages("pwr")
library(pwr)
pwr.t.test(n = NULL, d=0.3978, sig.level=.05, power = .8, type = 'one.sample')
# For a large effect (.8), sample size = 52 required.
# One-sample t test power calculation
# n = 51.55319
# d = 0.3978
# sig.level = 0.05
# power = 0.8
# alternative = two.sided

# The below code identifies the percentage of 0 values per tract after the s1.data.wide data has been generated:
nrow(s1.data.wide[s1.data.wide$lDLPFC_lCN == 0,])/nrow(s1.data.wide)
# 0
nrow(s1.data.wide[s1.data.wide$lDLPFC_rCN == 0,])/nrow(s1.data.wide)
# 0.0313 (3%, 3/96)
nrow(s1.data.wide[s1.data.wide$rDLPFC_lCN == 0,])/nrow(s1.data.wide)
# 0.0104 (1%, 1/96) 
nrow(s1.data.wide[s1.data.wide$rDLPFC_rCN == 0,])/nrow(s1.data.wide)
# 0
nrow(s1.data.wide[s1.data.wide$lDLPFC_lLNP == 0,])/nrow(s1.data.wide)
# 0 
nrow(s1.data.wide[s1.data.wide$lDLPFC_rLNP == 0,])/nrow(s1.data.wide)
# 0 
nrow(s1.data.wide[s1.data.wide$rDLPFC_lLNP == 0,])/nrow(s1.data.wide)
# 0.0104 (1%, 1/96)
nrow(s1.data.wide[s1.data.wide$rDLPFC_rCN == 0,])/nrow(s1.data.wide)
# 0
nrow(s1.data.wide[s1.data.wide$lDLPFC_lSPG == 0,])/nrow(s1.data.wide)
# 0.0104 (1%, 1/96)
nrow(s1.data.wide[s1.data.wide$lDLPFC_rSPG == 0,])/nrow(s1.data.wide)
# 0.6458 (65%, 65/96)
nrow(s1.data.wide[s1.data.wide$rDLPFC_lSPG == 0,])/nrow(s1.data.wide)
# 0.8229 (82%, 82/96)
nrow(s1.data.wide[s1.data.wide$rDLPFC_rSPG == 0,])/nrow(s1.data.wide)
# 0
nrow(s1.data.wide[s1.data.wide$rCN_rSOG == 0,])/nrow(s1.data.wide)
# 0.0729 (7%, 7/96)
nrow(s1.data.wide[s1.data.wide$lTHA_rSOG == 0,])/nrow(s1.data.wide)
# 0.0938 (9%, 9/96)
nrow(s1.data.wide[s1.data.wide$lTHA_lSOG == 0,])/nrow(s1.data.wide)
# 0
nrow(s1.data.wide[s1.data.wide$rTHA_rSOG == 0,])/nrow(s1.data.wide)
# 0
nrow(s1.data.wide[s1.data.wide$rTHA_lSOG == 0,])/nrow(s1.data.wide)
# 0.2813 (28%, 27/96)
