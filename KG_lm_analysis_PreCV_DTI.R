# Written by K. Garner, 2020
# perform linear mixed effects models to determine associations between DTI & coefficient of variability
rm(list=ls())

# Load packages, source function files and define path variables
library(tidyverse)
library(cowplot)
library(interactions)
source("R_rainclouds.R")
source("KG_data-wrangling.R")
tract_data = 'dti-data/KG_2factSol_subdata.csv'

## --------------------------------------------------------------------------
# load behavioural data and relabel
# load CV and learning rate params and bind together into single dataframe
fpath <- paste(getwd(), 'cleaned-data', sep='/')# path for attaining data 
CV.dat <- read.csv(paste(fpath, "../cleaned-data/CV-all-subs.csv", sep="/"))
CV.dat$group <- NA
CV.dat$group[CV.dat$sub < 200] = "practice"
CV.dat$group[CV.dat$sub > 199] = "control"
CV.dat$group <- factor(CV.dat$group, levels=c("practice", "control"))
CV.dat$cog_cond <- c("lo", "med", "hi")
CV.dat$cog_cond <- factor(CV.dat$cog_cond, levels=c("lo", "med", "hi"))
CV.dat <- CV.dat %>% select(-mult_cond)
CV.dat$sub <- as.factor(CV.dat$sub)
CV.dat$sess <- factor(CV.dat$sess, levels=c("Pre", "Post"))

learn.dat <- rbind( read.csv( paste(fpath, "practiceGrp-pwr-coeffs.csv", sep="/" ) ),
                    read.csv( paste(fpath, "controlGrp-pwr-coeffs.csv", sep="/" ) )) %>%
  select(-c(model, RMSE, int, slp))
learn.dat$cog_cond <- c("lo", "med", "hi")
learn.dat$cog_cond <- factor(learn.dat$cog_cond, levels=c("lo", "med", "hi"))
learn.dat <- learn.dat %>% select(-cond)
learn.dat$sub <- as.factor(learn.dat$sub)

all.dat <- inner_join(CV.dat, learn.dat, by=c("sub", "cog_cond"))
all.dat$sub <- as.factor(all.dat$sub)


## --------------------------------------------------------------------------
# add in the tract data and tidy up
tract.dat <- read.csv(tract_data) %>% select(-X) 
tract.dat$sub <- as.factor(tract.dat$sub)
reg.dat <- inner_join(CV.dat, tract.dat, by=c('sub')) %>%
              unique() %>% na.omit
reg.dat$sub <- as.factor(reg.dat$sub)


## --------------------------------------------------------------------------
# qqplots of each DV and each IV
draw.qq <- function(x){
    ggplot(reg.dat, aes(sample=eval(parse(text = x)))) +
          stat_qq() + stat_qq_line() +
          facet_wrap(~cog_cond)
  }
lapply(c("RT", "CV", "a", "b", "deriv_a", "deriv_b", "cort_to_Put", "cort_to_CN"), draw.qq)
# some deviations from normality but not overly worried about it

## --------------------------------------------------------------------------
# run models on pre-data
RT.mds <- lapply(unique(reg.dat$cog_cond), function(x) glm(RT ~ cort_to_CN*cort_to_Put*group*sess, data=reg.dat[reg.dat$cog_cond == x,]))
lapply(c(1:3), function(x) summary(RT.mds[[x]]))
# for the RT data, there are significant interactions of group x session, for single, first multi & second multi trials

CV.mds <- lapply(unique(reg.dat$cog_cond), function(x) glm(CV ~ cort_to_CN*cort_to_Put*group*sess+RT, data=reg.dat[reg.dat$cog_cond == x,]))
lapply(c(1:3), function(x) summary(CV.mds[[x]]))


interact_plot(CV.mds[[2]], pred=cort_to_CN, modx=sess, mod2=group, plot.points=TRUE, interval=TRUE,
              colors = wes_palette("IsleofDogs1")[c(1,4)])
interact_plot(CV.mds[[2]], pred=cort_to_CN, modx=sess, mod2=cort_to_Put, plot.points=TRUE, interval=TRUE,
              colors = wes_palette("IsleofDogs1")[c(2,3,5)])
interact_plot(CV.mds[[3]], pred=cort_to_CN, modx=sess, mod2=group, plot.points=TRUE, interval=TRUE,
              colors = wes_palette("IsleofDogs1")[c(1,4)])

# there is a significant slope for the practice group at pre, but for neither group at post, and control did not 
# differ from pre-post

# so why is the practice group different to the control group at Pre? And different to itself at post, and
# different to the control group at post?
# i.e. what is driving this pre-practice difference?

# lets only consider the pre-data, and we will look within each group, before comparing between the two

# 1. look at RT-diffs for each condition at pre and at post, for each group. You would expect that the 
# shorter the duration between single and first-multi, the longer the duration between first and second-multi
RT.diff.dat <- reg.dat %>% group_by(sub, group, sess) %>%
                           summarise(sing2fm = RT[cog_cond == "med"]-RT[cog_cond == "lo"],
                                     fm2sm = RT[cog_cond == "hi"]-RT[cog_cond == "med"])

RT.diff.dat[RT.diff.dat$diff.rat<25,] %>% ggplot(aes(x=group, y=diff.rat, color=group)) +
                                          geom_boxplot() + facet_wrap(~sess)
# here we go
RT.diff.dat %>% ggplot(aes(x=group, y=sing2fm, color=group)) +
                      geom_boxplot() + facet_wrap(~sess)
RT.diff.dat %>% ggplot(aes(x=group, y=fm2sm, color=group)) +
                      geom_boxplot() + facet_wrap(~sess)
# these plots mirror the glm results above, and show that the groups are different pre and post

# 2. Are there response grouping differences?
# visually there appears to be. We can do a t-test of the response grouping measure
# to formally test if the groups are different
with(RT.diff.dat, t.test(diff.rat~group, paired=FALSE, var.equal = FALSE))
with(RT.diff.dat, t.test(sing2fm~group, paired=FALSE, var.equal = FALSE))
with(RT.diff.dat, t.test(fm2sm~group, paired=FALSE, var.equal = FALSE))

# 3. OK, now we know that the distance from single to first multi was larger for the practice group,
# and that first multi to second multi was smaller for practice than control group - suggesting 
# a difference in response strategy. The question is now, does cort_to_CN interact with these values to 
# predict CV
reg.dat <- inner_join(reg.dat, RT.diff.dat, by=c("sub", "sess", "group"))


# in concert with the findings above, the sing2fm variable interacts with the relationship between cort_to_CN and sess for the practice group
sing2fm.mod.p <- lapply(c("med","hi"), function(x) glm(CV ~ cort_to_CN:sing2fm:sess, data=reg.dat[reg.dat$group == "practice" & reg.dat$cog_cond == x,]))
lapply(c(1:2), function(x) summary(sing2fm.mod.p[[x]]))
fm2sm.mod.p <- lapply(c("med","hi"), function(x) glm(CV ~ cort_to_CN:fm2sm:sess, data=reg.dat[reg.dat$group == "practice" & reg.dat$cog_cond == x,]))
lapply(c(1:2), function(x) summary(fm2sm.mod.p[[x]]))
sing2fm.mod.c <- lapply(c("med","hi"), function(x) glm(CV ~ cort_to_CN:sing2fm:sess, data=reg.dat[reg.dat$group == "control" & reg.dat$cog_cond == x,]))
lapply(c(1:2), function(x) summary(sing2fm.mod.c[[x]]))
fm2sm.mod.c <- lapply(c("med","hi"), function(x) glm(CV ~ cort_to_CN:fm2sm:sess, data=reg.dat[reg.dat$group == "control" & reg.dat$cog_cond == x,]))
lapply(c(1:2), function(x) summary(fm2sm.mod.c[[x]]))

interact_plot(sing2fm.mod.p[[1]], pred=cort_to_CN, modx=sess, mod2=sing2fm, plot.points=TRUE, interval=TRUE,
              colors = wes_palette("IsleofDogs1")[c(1,4)])


# NOW LOOK AT a AND B PARAMS
# NOTHING HERE - CAN DISCOUNT - ########################################################################################
learn.dat.p <- reg.dat %>% filter(sess=="Pre") %>% filter(group == "practice")
learn.dat.c <- reg.dat %>% filter(sess=="Pre") %>% filter(group == "control")
a.mds.p <- lapply(unique(learn.dat.p$cog_cond), function(x) glm(a ~ cort_to_CN*cort_to_Put, data=learn.dat.p[learn.dat.p$cog_cond == x,]))
lapply(c(1:3), function(x) summary(a.mds.p[[x]]))
a.mds.c <- lapply(unique(learn.dat.c$cog_cond), function(x) glm(a ~ cort_to_CN*cort_to_Put, data=learn.dat.c[learn.dat.c$cog_cond == x,]))
lapply(c(1:3), function(x) summary(a.mds.c[[x]]))
b.mds.p <- lapply(unique(learn.dat.p$cog_cond), function(x) glm(b ~ cort_to_CN*cort_to_Put, data=learn.dat.p[learn.dat.p$cog_cond == x,]))
lapply(c(1:3), function(x) summary(b.mds.p[[x]]))
b.mds.c <- lapply(unique(learn.dat.c$cog_cond), function(x) glm(b ~ cort_to_CN*cort_to_Put, data=learn.dat.c[learn.dat.c$cog_cond == x,]))
lapply(c(1:3), function(x) summary(b.mds.c[[x]]))
da.mds.p <- lapply(unique(learn.dat.p$cog_cond), function(x) glm(deriv_a ~ cort_to_CN*cort_to_Put, data=learn.dat.p[learn.dat.p$cog_cond == x,]))
lapply(c(1:3), function(x) summary(da.mds.p[[x]]))
da.mds.c <- lapply(unique(learn.dat.c$cog_cond), function(x) glm(deriv_a ~ cort_to_CN*cort_to_Put, data=learn.dat.c[learn.dat.c$cog_cond == x,]))
lapply(c(1:3), function(x) summary(da.mds.c[[x]]))
db.mds.p <- lapply(unique(learn.dat.p$cog_cond), function(x) glm(deriv_b ~ cort_to_CN*cort_to_Put, data=learn.dat.p[learn.dat.p$cog_cond == x,]))
lapply(c(1:3), function(x) summary(db.mds.p[[x]]))
db.mds.c <- lapply(unique(learn.dat.c$cog_cond), function(x) glm(deriv_b ~ cort_to_CN*cort_to_Put, data=learn.dat.c[learn.dat.c$cog_cond == x,]))
lapply(c(1:3), function(x) summary(db.mds.c[[x]]))


## --------------------------------------------------------------------------
# PLOT THE STATISTICAL CORRELATIONS
# predict data
CV.med <- reg.dat[reg.dat$cog_cond == "med", ]
CV.med$pred <- predict(CV.mds[[2]], CV.med, se.fit=F)

CV.med %>% ggplot(aes(x=cort_to_CN, y=CV)) %>%
                        + geom_point() + geom_line(aes(y=pred))