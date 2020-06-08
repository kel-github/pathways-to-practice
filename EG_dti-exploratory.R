# written by K. Garner and Elizabeth Geary, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

rm(list=ls())
install.packages("rstudioapi")
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
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"))

sub.data <- GetDTIData(fpath, tracts)

sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-trial", "post-trial")

sub.data$tract_names <- c("lDLPFC_lACC", "rDLPFC_rACC", "lDLPFC_rACC", "rDLPFC_lACC",
                          "lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP")


attach(sub.data)
mytable <- table(group,session) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
group
#practice  control 
#1092     1092 
margin.table(mytable, 2) # B frequencies (summed over A)
session
#pre-trial post-trial 
#1128       1056 

# I have chosen these two tables (partly because it was in your list of suggested tables) becuase it 
# shows me the drop out rate from pre to post trial is 72 participants. I could've used a tibble to 
# generate the table but when I was googling to find how to create one this seemed to be the most straight
# forward option, there was no specific methodoligical basis for choosing this path.

summary(sub.data$FA)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.3681  0.4000  0.4058  0.4484  0.5452 

#This will help loosley inform the next table as it shows me the mean FA of all of the tracts so I have
# an idea of how the following specific tracts deviate from the average. Also, this seemed like a good 
# thing to have - it felt like the natural precursor to the next table.

aggregate(sub.data[6], list(sub.data$tract_name),mean)
#Group.1        FA
#1  lDLPFC_lACC 0.3726710
#2  lDLPFC_lCN 0.3621235
#3  lDLPFC_lLNP 0.3689947
#4  lDLPFC_rACC 0.4300748
#5   lDLPFC_rCN 0.4575443
#6  lDLPFC_rLNP 0.4557069
#7  rDLPFC_lACC 0.4179832
#8   rDLPFC_lCN 0.4663301
#9  rDLPFC_lLNP 0.4429937
#10 rDLPFC_rACC 0.3386966
#11  rDLPFC_rCN 0.3752132
#12 rDLPFC_rLNP 0.3814579

# This table I made so that I could assess the mean FA for each of the tracts, this can be conpared 
# to the overall mean calculated above.  I did want to separate these out into FA of the tracts at 
#the levels of session and group so I can do further comparisons, but I had a look and a play around 
#for 20 mins and couldn't find anything that told me how to separate again down into session and group
#- so perhaps for another time... This was also a case of ease - this was the code that I found that seemed
# to do what I wanted it to with the least amount of hassle. It seems pretty easy to understand.

aggregate(sub.data[6], list(sub.data$session), mean)
#Group.1        FA
#1  pre-trial 0.4057302
#2 post-trial 0.4059073

#This table here shows me that there was a difference between pre and post trial and that training
#did have an effect. But, it's not a huge difference, so I'm not sure how much of an impact the training
# had - but I also don't know how much change to expect - maybe thats good I'm not sure :) also - 
# This factors in control so I imagine it's not hugely indicative.
#Also, I would like to compare reaction times but I'm not sure if I have them??

aggregate(sub.data[6], list(sub.data$group), mean)
#Group.1        FA
#1 practice 0.4071166
#2  control 0.4045150

# This table is a bit better than the previous one in that it tells me that overall practice 
# was better than control so there was an effect of training to seems

aggregate(sub.data[6], list(sub.data$group, session),mean)
#Group.1    Group.2        FA
#1 practice  pre-trial 0.4070669
#2  control  pre-trial 0.4042746
#3 practice post-trial 0.4071747
#4  control post-trial 0.4047501

#This one combines the last two to show how the group and session variables interact - perhaps doesn't
#add a whole lot of insight but a nice thing to look at.


