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
fpath  <-  '~/Documents/THESIS/pathways-to-practice/raw-behav-data/' # path to data
  raw.data  <- GetPrePostDataRaw(fpath)

  raw.data$cond <- as.factor(raw.data$cond)
  levels(raw.data$cond) <- c("single_shape","single_sound" , "multitask")

  raw.data$sess <- as.factor(raw.data$sess)
  levels(raw.data$sess) <- c("pre-training", "post-training")
  
  raw.data$sub <- as.factor(raw.data$sub)
  raw.data$task <- as.factor(raw.data$task)
  raw.data$trial_num <- as.factor(raw.data$trial_num)
  raw.data$run <- as.factor(raw.data$run)
  
# calculate accuracy using tidyverse functions:
# group_by https://dplyr.tidyverse.org/reference/group_by.html
# summarise https://dplyr.tidyverse.org/reference/summarise.html
# --------------------------------------------------------------------------------
  
  s1.acc.dat <- raw.data %>% group_by(sub, cond) %>%
    filter(sess == "pre-training") %>%
    summarise (mean=mean(acc))
  
 gr.acu <- s1.acc.dat %>% group_by(cond) %>%
  summarise (mean=mean(mean))

# Minimum Accuracy Criteria
# --------------------------------------------------------------------------------
#We have set a minimum accuracy criteria of at least 70% in accordance 
#with Garner, Lynch, & Dux, 2016
 

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
clean.data$cond <- as.factor(clean.data$cond)
levels(clean.data$cond) <- c("single_shape","single_sound" , "multitask")

clean.data$sess <- as.factor(clean.data$sess)
levels(clean.data$sess) <- c("pre-training", "post-training")

clean.data$sub <- as.factor(clean.data$sub)
clean.data$task <- as.factor(clean.data$task)
clean.data$trial_num <- as.factor(clean.data$trial_num)
clean.data$run <- as.factor(clean.data$run)


# by subject, sess, condition, use group_by and summarise to compute the mean, variance, 
# signal to noise ratio (mean/std) and coefficient of variation (std/mean)
# https://en.wikipedia.org/wiki/Signal-to-noise_ratio (see the alternative definition)
# https://www.investopedia.com/terms/c/coefficientofvariation.asp
# ----------------------------------------------------------------------------------
#mean, variance, signal to noise, coefficient of variation

Mean.vari <- clean.data %>% group_by(sub, cond, sess) %>%
  summarise (mean=mean(RT), (sd=sd(RT)))

SD <- clean.data %>% group_by(sub, cond, sess) %>%
  summarise (sd=sd(RT))

cov <- clean.dat %>% group_by(sub, sess, mult_cond) %>%
  summarise(q25 = quantile(RT, 0.25),
            q50 = quantile(RT, 0.50),
            q75 = quantile(RT, 0.75)) %>%
  group_by(sub, sess, mult_cond) %>%
  transmute(CV=((q75-q25)/q50)*100) %>%
  filter(sess == 'Pre') 

snr <- clean.data

# perform visual data checks of the three variables, using only the session 1 data,
# using boxplots and qqplots. Are there any outliers? Any large deviations from a
# normal distribution?
# ----------------------------------------------------------------------------------
ggplot(clean.data, aes(RT,cond)) + geom_boxplot()
[behavbox]("documents/THESIS/pathways-to-practice/behavbox.png")

qqp <- clean.data %>% 
  ggplot(aes(sample=RT)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~cond)
qqp
[behavqq] ("documents/THESIS/pathways-to-practice/behavqq.png")

# exclude outliers
# ----------------------------------------------------------------------------------
clean.data<- clean.data %>% filter(RT > (mean(RT) - (3*sd(RT))))
clean.data <- clean.data %>% filter(RT < (mean(RT) + (3*sd(RT))))

# from our examination of the data, we chose [x] as our DV of interest. Now compute the 
# ratio of each multitask condition to the single task condition (multi-x / single) using
# the group_by and summarise functions
# ----------------------------------------------------------------------------------

ratio.sound<- clean.data %>% group_by(sub) %>%
  filter ()
  summarise (multi-second - singl)



# create a raincloud plot of our final measure (from the session 1 data, by condition)
# ----------------------------------------------------------------------------------
library(cowplot)
p <- ggplot(XXXXXX, aes(x=cond, y=XXXXXX, fill = cond, colour = cond)) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                     TRUE) +
  geom_point(position=position_jitter(width=.15), size=.25) +
  geom_boxplot(aes(x = cond, y = XXXXXXX), outlier.shape = NA,
               alpha = 0.3, width = .1, colour = "BLACK") +
  scale_y_continuous(limits=c(0.2,0.65)) + coord_flip() +
  ylab('XXXXXX') + xlab('cond') + theme_cowplot() + 
  guides(fill = FALSE, colour = FALSE) +
  theme(axis.title.x = element_text(face = "italic"))



# save the final measures from the session 1 data as a csv file
# ----------------------------------------------------------------------------------

