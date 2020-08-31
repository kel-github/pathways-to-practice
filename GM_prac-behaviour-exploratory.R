# written by K. Garner and Georgia Marsh, 2020
# this code reads in the practice behavioural data, tidies it, and performs an exploratory
# and descriptive data analysis.

rm(list=ls())

# set working directory to current location
# install.packages("rstudioapi")  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
# install.packages("tidyverse")   
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

# Multitask data factors:
raw.multi.data$cond_block <- as.factor(raw.multi.data$cond_block)
raw.multi.data$cond <- as.factor(raw.multi.data$cond)
# levels(raw.multi.data$cond) <- c("shape single", "sound single", "multitask")
raw.multi.data$sub <- as.factor(raw.multi.data$sub)

# Visual search data factors:
raw.vis.search.data$cond_block <- as.factor(raw.vis.search.data$cond_block)
raw.vis.search.data$cond <- as.factor(raw.vis.search.data$cond)
raw.vis.search.data$sub <- as.factor(raw.vis.search.data$sub)


# for each task, calculate accuracy for each subject, block and condition using tidyverse functions:
# group_by https://dplyr.tidyverse.org/reference/group_by.html
# summarise https://dplyr.tidyverse.org/reference/summarise.html
# --------------------------------------------------------------------------------

# Multitask data accuracy at participant level:
multi.accuracy <- raw.multi.data %>% 
  group_by(sub, cond, cond_block) %>%
  summarise(accu = mean(acc))

# Visual search data accuracy at participant level:
vis.accuracy <- raw.vis.search.data %>% 
  group_by(sub, cond, cond_block) %>%
  summarise(accu = mean(acc))

# Multitask data accuracy at group level:
multi.group.accuracy <- multi.accuracy %>%
  group_by(cond, cond_block) %>%
  summarise(group.accu = mean(accu))

# Visual search data accuracy at group level:
vis.group.accuracy <- vis.accuracy %>%
  group_by(cond, cond_block) %>%
  summarise(group.accu = mean(accu))


# are we setting a minimum accuracy criteria?
# --------------------------------------------------------------------------------

# Minimum accuracy criteria of 70% accuracy as per Garner, Lynch & Dux (2016)

# Visual search outliers:
min.vis.acc.criteria <- vis.accuracy %>% group_by(sub, cond, cond_block) %>%
  filter(accu < .7)
# 3 participants (223, 234, 246) are below the minimum accuracy criteria of 70% with 5 data points total - 
# sub   cond    cond_block    accu
# 223   8       1             0.5982143
# 223   12      1             0.5803571
# 223   16      1             0.5625000
# 234   16      1             0.6785714
# 246   16      5             0.6964286

# Multitask outliers:
min.multi.acc.criteria <- multi.accuracy %>% group_by(sub, cond, cond_block) %>%
  filter(accu < .7)
# 2 participants (106, 144) are below the minimum accuracy criteria of 70% with 15 data points total -
# sub   cond          cond_block    accu
# 106   sound single  1             0.08035714
# 106   sound single  2             0.20535714
# 106   sound single  3             0.14285714
# 106   sound single  4             0.14285714
# 106   sound single  5             0.11607143
# 106   sound single  6             0.15178571
# 106   multitask     1             0.60267857
# 106   multitask     2             0.60267857
# 106   multitask     3             0.54910714
# 106   multitask     4             0.55803571
# 106   multitask     5             0.55803571
# 106   multitask     6             0.55803571
# 144   shape single  3             0.50000000
# 144   sound single  3             0.56250000
# 144   multitask     3             0.53571429


# now clean the RT data (all correct responses and all data > 200 ms or < + 2.5 stdevs from the
# mean for that participant in that condition, and that block), 
# for the multitask data, recode the data so that trials are labelled
# as either single task, or the first task performed on multitask trials
# --------------------------------------------------------------------------------
clean.multi.data <- GetPracticeMultiClean(raw.multi.data)
clean.multi.data <- RecodePracticeMultiData(clean.multi.data)
clean.vis.search.data <- GetPracticeVisSearchClean(raw.vis.search.data)

# Collapse single trials over the task factor:
clean.multi.data <- rbind(clean.multi.data %>% filter(mult_cond=="single") %>%
                            group_by(sub, cond_trial, cond_block) %>%
                            transmute(RT=mean(RT),
                                      mult_cond="single"),
                          clean.multi.data %>% filter(mult_cond!="single"))


# now filter out participants who scored below the minimum accuracy criteria, using
# the tidyverse filter function (see https://r4ds.had.co.nz/transform.html section 5.2)
# label, recode and where necessary, reorder factors of the dataframe
# --------------------------------------------------------------------------------

# Filter out participants 106 and 144 who do not meet minimum criteria:
clean.multi.data <- filter(clean.multi.data, !(sub %in% unique(min.multi.acc.criteria$sub)))
# Filter out participants 223, 234 and 246 who do not meet minimum criteria:
clean.vis.search.data <- filter(clean.vis.search.data, !(sub %in% unique(min.vis.acc.criteria$sub)))


# by subject, block, condition, use group_by and summarise to compute the mean
# and std deviation
# ----------------------------------------------------------------------------------

# Multitask data summary:
summary.multi <- clean.multi.data %>%
  group_by(sub, cond, cond_block) %>%
  summarise(mean(RT), sd(RT))

# Visual search data summary:
summary.vis <- clean.vis.search.data %>%
  group_by(sub, cond, cond_block) %>%
  summarise(mean(RT), sd(RT))


# for each task, participant, block and condition, perform visual data checks of 
# data using boxplots and qqplots. 
# Are there any outliers? Any large deviations from a
# normal distribution?
# ----------------------------------------------------------------------------------

# Visual search data boxplots (block x condition):
clean.vis.search.data %>% ggplot(aes(y=RT, group=cond)) +
  geom_boxplot(notch=TRUE) +
  facet_wrap(~cond_block)

# Multitask data boxplots (block x condition):
clean.multi.data %>% ggplot(aes(y=RT, group=mult_cond)) +
  geom_boxplot(notch=TRUE) +
  facet_wrap(~cond_block)

# Multitask QQ plot:
ggplot(clean.multi.data, aes(sample = RT, colour = factor(mult_cond))) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ cond_block, nrow = NULL)

# Visual search QQ plot:
ggplot(clean.vis.search.data, aes(sample = RT, colour = factor(cond))) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ cond_block, nrow = NULL)


# exclude outliers
# ----------------------------------------------------------------------------------

# Losing too much data with this criteria, opting to delete this step from analysis:
# outliers.multi <- clean.multi.data %>% group_by(cond) %>%
#  filter(((RT - mean(RT))/sd(RT)) > 3)
# outliers.multi

# outliers.vis <- clean.vis.search.data %>% group_by(cond) %>%
#  filter(((RT - mean(RT))/sd(RT)) > 3)
# outliers.vis

# clean.multi.data <- clean.multi.data %>% filter(RT < (mean(RT) + (3*sd(RT))))
# Filters out any RT outliers + 3*sd
# clean.multi.data <- clean.multi.data %>% filter(RT > (mean(RT) - (3*sd(RT))))
# Filters out any RT outliers - 3*sd
# clean.vis.search.data <- clean.vis.search.data %>% filter(RT < (mean(RT) + (3*sd(RT))))
# Filters out any RT outliers + 3*sd
# clean.vis.search.data <- clean.vis.search.data %>% filter(RT > (mean(RT) - (3*sd(RT))))
# Filters out any RT outliers - 3*sd

# now we are going to compute a moving average across trials, for each participant
# and condition, this is what we will plot and fit our functions to
# https://en.wikipedia.org/wiki/Moving_average
# we will average over each set of 12 trials (you can play with this value
# and see what effect it has when you plot it below)
# ----------------------------------------------------------------------------------
width = 12
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

# Multitask Graph:
# Subsetting 10 random participants from the data
plot.dat.multi <- roll.mu.multi %>% filter(sub %in% c(107, 135, 127, 128, 103, 108, 113, 117, 142, 137))

# Graphing moving averages for 10 participants with trials on x axis and moving average RT on y axis
ggplot(data=plot.dat.multi, aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  facet_wrap(~ sub, nrow = NULL)

# Graphing moving averages with error bars for 10 participants
ggplot(data=df3, aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
  geom_line() + geom_point()+
  geom_errorbar(aes(ymin=move_mu-sd, ymax=move_mu+sd), width=.1)+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  facet_wrap(~ sub, nrow = NULL)  

# Subsetting 5 random participants from the data
plot.dat.multi.5 <- roll.mu.multi %>% filter(sub %in% c(107, 135, 127, 128, 103))

# Graphing moving averages for 5 participants with trials on x axis and moving average RT on y axis
ggplot(data=plot.dat.multi.5, aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  facet_wrap(~ sub, nrow = NULL)


# Visual Search Graph:
# Subsetting 10 random participants from the data
plot.dat.vis <- roll.mu.vis.search %>% filter(sub %in% c(207, 235, 227, 228, 203, 208, 213, 217, 242, 237))

# Graphing moving averages for 10 participants with trials on x axis and moving average RT on y axis
ggplot(data=plot.dat.vis, aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  facet_wrap(~ sub, nrow = NULL)

# Standard deviations for moving averages per sub per condition per trial
df4 <- data_summary(plot.dat.vis, varname="move_mu", 
                    groupnames=c("sub", "cond", "cond_trial"))

# Graphing moving averages with error bars for 10 participants
ggplot(data=df4, aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
  geom_line() + geom_point()+
  geom_errorbar(aes(ymin=move_mu-sd, ymax=move_mu+sd), width=.1)+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  facet_wrap(~ sub, nrow = NULL)

# Subsetting 5 random participants from the data
plot.dat.vis.5 <- roll.mu.vis.search %>% filter(sub %in% c(207, 235, 227, 228, 203))

# Graphing moving averages for 5 participants with trials on x axis and moving average RT on y axis
ggplot(data=plot.dat.vis.5, aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()+
  facet_wrap(~ sub, nrow = NULL)

# Creating function that will help generate error bars on the final graphs --> no longer required
#data_summary <- function(plot.dat.multi, varname, groupnames){
#  require(plyr)
#  summary_func <- function(x, col){
#    c(mean = mean(x[[col]], na.rm=TRUE),
#     sd = sd(x[[col]], na.rm=TRUE))
#  }
# data_sum<-ddply(plot.dat.multi, groupnames, .fun=summary_func,
# varname)
# data_sum <- rename(data_sum, c("mean" = varname))
# return(data_sum)
#}

# Standard deviations for moving averages per sub per condition per trial
# df3 <- data_summary(plot.dat.multi, varname="move_mu", 
# groupnames=c("sub", "mult_cond", "cond_trial"))


# save the final measures from each task into their own csv files into the clean data folder
# ----------------------------------------------------------------------------------

write.csv(roll.mu.multi, file = "GM_multi-cleaned.csv", row.names = FALSE)
write.csv(roll.mu.vis.search, file = "GM_vis-cleaned.csv", row.names = FALSE)
