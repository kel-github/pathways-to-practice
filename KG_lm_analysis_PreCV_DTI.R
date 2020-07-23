# Written by K. Garner, 2020
# perform linear mixed effects models to determine associations between DTI & coefficient of variability
rm(list=ls())

# Load packages, source function files and define path variables
library(tidyverse)
library(cowplot)
source("R_rainclouds.R")
source("KG_data-wrangling.R")
tract_data = 'dti-data/KG_2factSol_subdata.csv'

## --------------------------------------------------------------------------
# load behavioural data and relabel
source("KG_behaviour-wrangles.R")
levels(dat.recoded$sess) <- c("Pre", "Post")
dat.save.loc = 'cleaned-data'
write_csv(dat.recoded, paste(dat.save.loc,'pre-data-for-CV.csv', sep='/'))
# plus variables for later
sd.crit = 3


## --------------------------------------------------------------------------
# Computations
# 1st, compute the mean and var by condition
head(dat.recoded)
CV.dat <- dat.recoded %>% group_by(sub, sess, mult_cond) %>%
                                   summarise(RTmu = mean(RT),
                                   RTsd = sd(RT)) %>%
                          group_by(sub, sess, mult_cond) %>%
                                   transmute(CV=RTsd/RTmu) %>%
                          filter(sess == 'Pre') 
# get how much the CV changed as a function of going from single task to multitask conditions
p.chnge <- CV.dat %>% group_by(sub) %>%
           transmute(MT1=CV[mult_cond == "multi-first"]/CV[mult_cond=="single"],
                     MT2=CV[mult_cond == "multi-second"]/CV[mult_cond=="single"]) %>%
           pivot_longer(c('MT1', 'MT2'), names_to = "condition", values_to="cvRatio")

# get the diff in mu RT per participant
RT.dat <- dat.recoded %>% group_by(sub, sess, mult_cond) %>%
                          summarise(RTmu = mean(RT),
                          RTsd = sd(RT)) %>%
                          group_by(sub, sess) %>%
                          transmute(MT1=RTmu[mult_cond == "multi-first"]/RTmu[mult_cond=="single"],
                                    MT2=RTmu[mult_cond == "multi-second"]/RTmu[mult_cond=="single"]) %>%
                                    pivot_longer(c('MT1', 'MT2'), names_to = "condition", values_to="muRatio") %>%
                          unique()


## --------------------------------------------------------------------------
# plot values to check distributions
CV.p <- ggplot(CV.dat, aes(x=mult_cond, y=CV, fill = mult_cond, colour = mult_cond)) +
                       geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                             TRUE) +
                       geom_point(position=position_jitter(width=.15), size=.25) +
                       geom_boxplot(aes(x = mult_cond, y = CV), outlier.shape = NA,
                             alpha = 0.3, width = .1, colour = "BLACK") +
                       scale_y_continuous(limits=c(0,1)) + coord_flip() +
                       ylab('condition') + xlab('CV') + theme_cowplot() + 
                       guides(fill = FALSE, colour = FALSE) +
                       theme(axis.title.x = element_text(face = "italic")) +
                       facet_wrap(~sess)
PC.p <- ggplot(p.chnge, aes(x=condition, y=cvRatio, fill = condition, colour = condition)) +
                       geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                            TRUE) +
                       geom_point(position=position_jitter(width=.15), size=.25) +
                       geom_boxplot(aes(x = condition, y = cvRatio), outlier.shape = NA,
                           alpha = 0.3, width = .1, colour = "BLACK") +
                           scale_y_continuous(limits=c(0,3)) + coord_flip() +
                       ylab('condition') + xlab('CV ratio') + theme_cowplot() + 
                       guides(fill = FALSE, colour = FALSE) +
                       theme(axis.title.x = element_text(face = "italic"))
RT.p <- ggplot(RT.dat,  aes(x=condition, y=muRatio, fill = condition, colour = condition)) +
                        geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                              TRUE) +
                        geom_point(position=position_jitter(width=.15), size=.25) +
                        geom_boxplot(aes(x = condition, y = muRatio), outlier.shape = NA,
                              alpha = 0.3, width = .1, colour = "BLACK") +
                        scale_y_continuous(limits=c(0,3)) + coord_flip() +
                        ylab('condition') + xlab('Mu ratio') + theme_cowplot() + 
                        guides(fill = FALSE, colour = FALSE) +
                        theme(axis.title.x = element_text(face = "italic"))

## --------------------------------------------------------------------------
## remove outliers (> | < mean +/- 3 sd)
CV.dat$group = NA
CV.dat$group[as.numeric(as.character(CV.dat$sub))<200] = 'practice'
CV.dat$group[as.numeric(as.character(CV.dat$sub))>=200] = 'control'
CV.dat$group <- as.factor(CV.dat$group)
CV.dat <- na.omit(CV.dat) %>% group_by(mult_cond) %>%
                              filter(CV < (mean(CV)+(sd.crit*sd(CV)))) %>%
                              filter(CV > mean(CV)-(sd.crit*sd(CV))) 

p.chnge$group = NA
p.chnge$group[as.numeric(as.character(p.chnge$sub))<200] = 'practice'
p.chnge$group[as.numeric(as.character(p.chnge$sub))>=200] = 'control'
p.chnge$group <- as.factor(p.chnge$group)
p.chnge <- na.omit(p.chnge)
p.chnge <- p.chnge %>% group_by(condition) %>%
                               filter(cvRatio < (mean(cvRatio)+(sd.crit*sd(cvRatio)))) %>%
                               filter(cvRatio > mean(cvRatio)-(sd.crit*sd(cvRatio))) 

RT.dat$group = NA
RT.dat$group[as.numeric(as.character(RT.dat$sub))<200] = 'practice'
RT.dat$group[as.numeric(as.character(RT.dat$sub))>=200] = 'control'
RT.dat$group <- as.factor(RT.dat$group)
RT.dat <- na.omit(RT.dat)
RT.dat <- RT.dat %>% group_by(condition) %>%
                     filter(muRatio < (mean(muRatio)+(sd.crit*sd(muRatio)))) %>%
                     filter(muRatio > (mean(muRatio)-(sd.crit*sd(muRatio)))) 

## --------------------------------------------------------------------------
# add in the tract data and tidy up
tract.dat <- read.csv(tract_data) %>% select(-X) 
tract.dat$sub <- as.factor(tract.dat$sub)
reg.dat.CV <- inner_join(CV.dat, tract.dat, by=c('sub')) %>%
              unique() %>% na.omit
reg.dat.P <- inner_join(p.chnge, tract.dat, by=c('sub')) %>%
              unique()  %>% na.omit()
reg.dat.RT <- inner_join(RT.dat, tract.dat, by=c('sub')) %>%
              unique() %>% na.omit() %>% filter(!sess %in% 'Post' )

## --------------------------------------------------------------------------
# count how many participants we have for pre and post data
counts.CV <- reg.dat.CV %>% group_by(mult_cond) %>%
                     summarise(N=length(CV))

counts.P <- reg.dat.P %>% group_by(condition) %>%
                     summarise(N=length(cvRatio))

counts.RT <- reg.dat.RT %>% group_by(condition) %>%
                         summarise(N=length(muRatio))

## --------------------------------------------------------------------------
# plot the relationship between each regressor and the DV
ggplot(reg.dat.CV, aes(x=cort_to_Put, y=CV)) +
       geom_point(aes(color=group)) +
       facet_wrap(~mult_cond)
ggplot(reg.dat.CV, aes(x=cort_to_CN, y=CV)) +
       geom_point(aes(color=group)) + 
       facet_wrap(~mult_cond)


ggplot(reg.dat.P, aes(x=cort_to_Put, y=cvRatio)) +
      geom_point(aes(color=group)) +
      facet_wrap(~condition)
ggplot(reg.dat.P, aes(x=cort_to_CN, y=cvRatio)) +
      geom_point(aes(color=group)) + 
      facet_wrap(~condition)


ggplot(reg.dat.RT, aes(x=cort_to_Put, y=muRatio)) +
  geom_point(aes(color=group)) +
  facet_wrap(~condition)
ggplot(reg.dat.RT, aes(x=cort_to_CN, y=muRatio)) +
  geom_point(aes(color=group)) + 
  facet_wrap(~condition)

## --------------------------------------------------------------------------
# run models on pre-data
CV <- glm(CV ~ cort_to_CN*cort_to_Put*mult_cond, data=reg.dat.CV)
summary(CV)
write_csv(reg.dat.CV, paste(dat.save.loc,'CV-data.csv', sep='/'))

P <- glm(cvRatio ~ cort_to_CN*cort_to_Put*condition, data=reg.dat.P)
summary(P)
write_csv(reg.dat.P, paste(dat.save.loc,'P-data.csv', sep='/'))

RT <- glm(muRatio ~ cort_to_CN*cort_to_Put*condition, data=reg.dat.RT)
summary(RT)
write_csv(reg.dat.RT, paste(dat.save.loc,'RTmu-data.csv', sep='/'))
