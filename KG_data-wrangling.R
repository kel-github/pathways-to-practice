# Garner, KG. 2020
# This code contains the functions required to wrangle data into
# tidy form for the project 'pathways-to-practice'

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file
# source files and load packages
# ------------------------------------------------------------------
#library(tidyverse)

# define functions
# ------------------------------------------------------------------
GetDTIData <- function(fpath, tracts){
  # this function will pull out the FA measures from the participant 
  # level FA matrices
  # the function assumes that within the folder defined by fpath, the data
  # are in two subfolders called 'group-control' and 'group-practice'
  # The function also requires that the data files are named according to the
  # convention 'sub_xxxx_WB_Conn_FAMean.txt'
  # the labels file should also be in fpath
  
  # inputs
  # fpath - the file path to the folder that contains the DTI data
  # tracts - a list, each element of the list is a vector, the vector contains the 
  # names of the two regions that start and end the tract of interest
  
  results <- list()
  # first, read in the file names of the FA data, from where we want to pull data from
  sub_fols <- c( 'group-practice', 'group-control' )
  fnames <- lapply(sub_fols, function(x) list.files(paste(fpath, x, sep=""), pattern="FA") )
  fnames <- c( do.call(cbind, fnames) )
  
  # now, for each file, pull out the required data
  GetSubData <- function(f, tracts){
    sub.num.full <- strtoi(strsplit(f, "_")[[1]][2])
    if (sub.num.full < 2000) {
      group = 1 
    } else {
      group = 2  
    }
    sub.num <- round(sub.num.full*.1)
    pre.post <- 1-(sub.num.full %% 2)
    tmp.dat <- read.table(paste(fpath, sub_fols[group], '/', f, sep="") ,header=FALSE)
    labels <- read.table(paste(fpath, 'aal_labels.txt', sep=""), header = FALSE)
    tract.idxs <- lapply(tracts, function(x) match(x, t(labels)))
    FA.vals <- sapply(tract.idxs, function(x) tmp.dat[x[1],x[2]])
    N = length(FA.vals)
    sub.data <- data.frame(sub = rep(sub.num, each=N),
                           group = rep(group, each=N),
                           session = rep(pre.post, each=N),
                           tract_start = sapply(tracts, function(x) x[1]),
                           tract_end = sapply(tracts, function(x) x[2]),
                           FA = FA.vals)
    sub.data
  }
  
  sub.data <- lapply(fnames, GetSubData, tracts = tracts)
  sub.data <- do.call(rbind, sub.data)
}

GetPrePostDataRaw <- function(fpath){
  # this function pulls out the relevant variables for the pre- and post- data, and 
  # puts them in tidy form. 
  data <- read.csv(paste(fpath, 'KGG_mt_scan_data_lf_v1.csv', sep=""), header=T)
  data$sub <- as.factor(data$sub)
  # remove the unnecessary variables and make the data tidy
  data  <- data %>% select( -c(shape_stim, sound_stim, shape_resp, sound_resp))
  RT.data <- data %>% select( c(sub, sess, run, trial_num, cond, shape_RT, sound_RT)) %>%
    pivot_longer(c('shape_RT', 'sound_RT'), names_to = "task", values_to="RT") %>%
    mutate(task = fct_recode(task, 
                             "shape" = "shape_RT",
                             "sound" = "sound_RT"))
  acc.data <- data %>% select( c(sub, sess, run, trial_num, cond, shape_acc, sound_acc)) %>%
    pivot_longer(c('shape_acc', 'sound_acc'), names_to = "task", values_to="acc") %>%
    mutate(task = fct_recode(task, 
                             "shape" = "shape_acc",
                             "sound" = "sound_acc")) 
  data <- inner_join(RT.data, acc.data, by=c("sub", "sess", "run", "trial_num", "cond", "task")) %>%
    drop_na()
  data
  
}

GetPrePostClean <- function(fpath){
  # this function does as GetPrePostDataRaw but then also filters by participant to remove every
  # response time that is < 250 ms, or > 2.5 stdevs of the participant mean. The output data is
  # uncollapsed - i.e. all trials, with all incorrect trials removed.
  data <- GetPrePostDataRaw(fpath)
  data$sess <- as.factor(data$sess)
  data$cond <- as.factor(data$cond)
  
  # now get correct responses and filter outliers
  data <- data %>% group_by(sub, sess, cond) %>%
                   filter(acc == 1) %>%
                   filter(RT > .25) %>%
                   filter(RT < (mean(RT) + (2.5*sd(RT))))
  
  data
}
 
RecodeMultiData <- function(data){
  # this function takes the multitasking data,
  # filters out any multitask trials where both tasks were not performed correctly
  # it then adds condition classification mult_cond -> single (single task), first-multi (first task executed)
  # and second multi (second task executed)
  data$trial_num <- as.factor(data$trial_num)
  data$run <- as.factor(data$run)
  data <-  data %>% group_by(sub, sess, run, trial_num) %>%
                    filter(cond == "1" | cond == "2" | (cond == "3" & length(RT) > 1)) %>%
                    mutate(mult_cond = ifelse(cond == "3", ifelse(RT == max(RT), "multi-second", "multi-first"), "single"))
  
  data
}

GetPracticeDataMulti <- function(fpath){
  # read in the practice multitask data, tidy up, and number the trials/blocks by each condition
  # fpath = the folder location of the data
  # outputs a dataframe
  
  # notes about data: 2 blocks per session
  # will make a new column that breaks up the total number of blocks into one vector
  # For each session, participants completed 56 blocks of 18 trials, for a total of
  # 1,008 trials, resulting in 3,024 training trials overall. To ensure that participants retained familiarity with the timings of the task as presented in the
  # scanner, between two and four of the blocks in each session used long
  # ITI timings.
  # will therefore split each condition into 9 x 112 trials, and clean from that 
  data <- read.csv( paste(fpath, 'train_group_training_data.csv', sep="/"), header=TRUE )
  # now make it tidy
  RT.data <- data %>% select( c(sub, block, rest_block, trial_num, cond, shape_RT, sound_RT)) %>%
                      pivot_longer(c('shape_RT', 'sound_RT'), names_to = "task", values_to="RT") %>%
                      mutate(task = fct_recode(task, 
                                               "shape" = "shape_RT",
                                               "sound" = "sound_RT"))
  
  acc.data <- data %>% select( c(sub, block, rest_block, trial_num, cond, shape_stim, shape_resp, sound_stim, sound_resp)) %>%
                       mutate(shape_acc = (shape_stim == shape_resp)*1,
                              sound_acc = (sound_stim == sound_resp)*1 ) %>%
                       select( c(sub, block, rest_block, trial_num, cond, shape_acc, sound_acc) ) %>%
                       pivot_longer(c('shape_acc', 'sound_acc'), names_to = "task", values_to="acc") %>%
                       mutate(task = fct_recode(task, 
                                                "shape" = "shape_acc",
                                                "sound" = "sound_acc")) 
  # hook up the RT and the acc data, and make sure the trial order is correct
  data <- inner_join(RT.data, acc.data, by=c("sub", "block", "rest_block", "trial_num", "cond", "task")) %>%
          arrange(sub, cond, block, rest_block, trial_num) %>%
          mutate(cond_trial = rep(1:1008, each = 2, times = 3*50),
                 cond_block = rep(1:9, each = 112*2, times= 3*50)) %>%
          select(c(sub, cond_block, cond_trial, cond, task, RT, acc)) %>%
          drop_na()
  
  data
}


GetPracticeDataVisSearch <- function(fpath){
  # read in the visual search data, tidy up, and number the trials/blocks by each condition
  # fpath = the folder location of the data
  # outputs a dataframe
  
  # notes about data: 2 blocks per session
  # will make a new column that breaks up the total number of blocks into one vector
  # For each session, participants completed 56 blocks of 18 trials, for a total of
  # 1,008 trials, resulting in 3,024 training trials overall. To ensure that participants retained familiarity with the timings of the task as presented in the
  # scanner, between two and four of the blocks in each session used long
  # ITI timings.
  # will therefore split each condition into 9 x 112 trials, and clean from that 
  data <- read.csv( paste(fpath, 'control_group_training_data.csv', sep="/"), header=TRUE ) 
  
  # now make it tidy
  data <- data %>% mutate(acc = (targ_or == resp)*1 ) %>%
          select( c(sub, block, rest_block, trial_num, set_size, RT, acc) ) %>%
          arrange(sub, set_size, block, rest_block, trial_num) %>%
          mutate(cond_trial = rep(1:1008, times = 3*50),
                 cond_block = rep(1:9, each = 112, times= 3*50)) %>%
          select(c(sub, cond_block, cond_trial, set_size, RT, acc)) %>%
          rename(cond = set_size)
  data
}


GetPracticeMultiClean <- function(data){
  # clean up the raw practice multitasking data by removing innacurate 
  # responses and RT's < 200 ms | > 2*stdevs from 
  # the mean for that subject, condition, and block
  # inputs
  # data = a data.frame output by GetPracticeDataMulti
  # outputs
  # clean.data
  
  clean.data <- data %>% filter(acc == 1) %>%
                filter( RT > .250 ) %>%
                group_by( sub, cond_block, cond ) %>%
                filter( RT < (mean(RT)+2.5*sd(RT)))
   clean.data
}


RecodePracticeMultiData <- function(data){
  # this function takes the practice multitasking data,
  # filters out any multitask trials where both tasks were not performed correctly
  # it then adds condition classification mult_cond -> single (single task), first-multi (first task executed)
  # and second multi (second task executed)
  # the input is the dataframe output by GetPracticeMultiClean
  data <-  data %>% group_by(sub, cond_block, cond, cond_trial) %>%
                    filter(cond == "1" | cond == "2" | (cond == "3" & length(RT) > 1)) %>%
                    mutate(mult_cond = ifelse(cond == "3", ifelse(RT == max(RT), "multi-second", "multi-first"), "single"))
  data
}

GetPracticeVisSearchClean <- function(data){
  # take the practice visual search data, 
  # filter out innaccurate trials
  # filter out any RTs that are < 200 ms or are > 2.5*stdevs from the mean
  # inputs = dataframe output by GetPracticeDataVisSearch
  # outputs = dataframe
  
  clean.data <- data %>% filter(acc == 1) %>%
                filter(RT > .25) %>%
                group_by( sub, cond_block, cond ) %>%
                filter( RT < ( mean(RT) + 2.5*sd(RT)) )
  clean.data
}


get.coef.exps <- function(model){
  # the input is a mixed model output from lmer
  # assuming a function of y = exp(I+cond)*trial_num^beta
  out <- data.frame(sub=rownames(coef(model)$sub),
                    int=exp(coef(model)$sub[,3]),
                    x=exp(coef(model)$sub[,1]),
                    y=exp(coef(model)$sub[,2]))
  out
}

fit.mod.sub.level.multidat <- function(data, sub){
  # this function takes the dataframe labelled 'roll.mu.multi' and a 
  # subject index and outputs a dataframe that contains
  # the RMSE and parameters of the power and exponential functions
  # fit to the practice data.
  # NOTE: the trials regressor is scaled!
  
  fit.dat <- data[data$sub == sub, ]
  # scale trial number and ditch unneeded columns
  fit.dat$cond_trial = fit.dat$cond_trial/100
  fit.dat <- fit.dat %>% select(-c(cond, task, acc)) %>% na.omit()  
  
  # fit power model (the output contains the fitted values)
  pwr.fit <- lapply(levels(fit.dat$mult_cond), function(x) lm(log(move_mu)~log(cond_trial), data=fit.dat[fit.dat$mult_cond == x, ]))
  exp.fit <- lapply(levels(fit.dat$mult_cond), function(x) lm(log(move_mu)~cond_trial, data=fit.dat[fit.dat$mult_cond == x, ]))
  
  # make a data frame of the parameters of interest
  params <- data.frame(model=c("pwr", "pwr", "pwr", "exp", "exp", "exp"),
                       cond=rep(c("S", "FM", "SM"), times=2),
                       RMSE=c(sapply(1:3, function(x) sum(sqrt(pwr.fit[[x]]$residuals^2))),
                              sapply(1:3, function(x) sum(sqrt(exp.fit[[x]]$residuals^2)))),
                       int=c(sapply(1:3, function(x) pwr.fit[[x]]$coefficients[1]),
                             sapply(1:3, function(x) exp.fit[[x]]$coefficients[1])),
                       slp=c(sapply(1:3, function(x) pwr.fit[[x]]$coefficients[2]),
                             sapply(1:3, function(x) exp.fit[[x]]$coefficients[2])))
  params$sub = sub
  params
}


fit.mod.sub.level.VS <- function(data, sub){
  # this function takes the dataframe labelled 'roll.mu.vis.search' and a 
  # subject index and outputs a dataframe that contains
  # the RMSE and parameters of the power and exponential functions
  # fit to the practice data.
  # NOTE: the trials regressor is scaled!
  
  fit.dat <- data[data$sub == sub, ]
  # scale trial number and ditch unneeded columns
  fit.dat$cond_trial = fit.dat$cond_trial/100
  fit.dat <- fit.dat %>% na.omit()  
  
  # fit power model (the output contains the fitted values)
  pwr.fit <- lapply(levels(fit.dat$cond), function(x) lm(log(move_mu)~log(cond_trial), data=fit.dat[fit.dat$cond == x, ]))
  exp.fit <- lapply(levels(fit.dat$cond), function(x) lm(log(move_mu)~cond_trial, data=fit.dat[fit.dat$cond == x, ]))
  
  # make a data frame of the parameters of interest
  params <- data.frame(model=c("pwr", "pwr", "pwr", "exp", "exp", "exp"),
                       cond=rep(c("s8", "s12", "s16"), times=2),
                       RMSE=c(sapply(1:3, function(x) sum(sqrt(pwr.fit[[x]]$residuals^2))),
                              sapply(1:3, function(x) sum(sqrt(exp.fit[[x]]$residuals^2)))),
                       int=c(sapply(1:3, function(x) pwr.fit[[x]]$coefficients[1]),
                             sapply(1:3, function(x) exp.fit[[x]]$coefficients[1])),
                       slp=c(sapply(1:3, function(x) pwr.fit[[x]]$coefficients[2]),
                             sapply(1:3, function(x) exp.fit[[x]]$coefficients[2])))
  params$sub = sub
  params
}


plot.multi.fit <- function(data, params, sub){
  # data = roll.mu.multi
  # params = the fis from fit.mod.sub.level.multidat
  # sub = the subject number for whom a plot is required
  data = data[data$sub == sub, ]
  params = params[params$sub == sub, ]
  
  # first predict the data for each model
  data$pwr = NA
  data$exp = NA
  mult_conds = rep(levels(data$mult_cond), times=2)
  funs = rep(c("pwr", "exp"), each=3)
  p_conds = params$cond
  
  pred.pwr.condition <- function(x,y,z, data, params){
    exp(params$int[params$model == y & params$cond == z]) * ((data$cond_trial[data$mult_cond == x]/100)^params$slp[params$model == y & params$cond == z])
  }
  
  pred.exp.condition <- function(x,y,z, data, params){
    exp(params$int[params$model == y & params$cond == z]) * exp((data$cond_trial[data$mult_cond == x]/100)*params$slp[params$model == y & params$cond == z])
  }
  
  for (i in 1:3) data$pwr[data$mult_cond == mult_conds[i]] = pred.pwr.condition(mult_conds[i], "pwr", p_conds[i], data, params)
  for (i in 1:3) data$exp[data$mult_cond == mult_conds[i]] = pred.exp.condition(mult_conds[i], "exp", p_conds[i], data, params)
  
  # now plot the data
  pwr.p <- data %>% select(-c(cond, task, acc)) %>%
                na.omit() %>% ggplot(aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
                geom_point() +
                geom_line(aes(x=cond_trial, y=pwr, group=mult_cond, color=mult_cond)) 
  
  # now plot the data
  exp.p <- data %>% select(-c(cond, task, acc)) %>%
                na.omit() %>% ggplot(aes(x=cond_trial, y=move_mu, group=mult_cond, color=mult_cond)) +
                geom_point() +
                geom_line(aes(x=cond_trial, y=exp, group=mult_cond, color=mult_cond))
  grid.arrange(pwr.p, exp.p)
}

plot.VS.fit <- function(data, params, sub){
  # data = roll.mu.multi
  # params = the fis from fit.mod.sub.level.multidat
  # sub = the subject number for whom a plot is required
  data = data[data$sub == sub, ]
  params = params[params$sub == sub, ]
  
  # first predict the data for each model
  data$pwr = NA
  data$exp = NA
  conds = rep(levels(data$cond), times=2)
  funs = rep(c("pwr", "exp"), each=3)
  p_conds = params$cond
  
  pred.pwr.condition <- function(x,y,z, data, params){
    exp(params$int[params$model == y & params$cond == z]) * ((data$cond_trial[data$cond == x]/100)^params$slp[params$model == y & params$cond == z])
  }
  
  pred.exp.condition <- function(x,y,z, data, params){
    exp(params$int[params$model == y & params$cond == z]) * exp((data$cond_trial[data$cond == x]/100)*params$slp[params$model == y & params$cond == z])
  }
  
  for (i in 1:3) data$pwr[data$cond == conds[i]] = pred.pwr.condition(conds[i], "pwr", p_conds[i], data, params)
  for (i in 1:3) data$exp[data$cond == conds[i]] = pred.exp.condition(conds[i], "exp", p_conds[i], data, params)
  
  # now plot the data
  pwr.p <- data %>% na.omit() %>% ggplot(aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
                geom_point() +
                geom_line(aes(x=cond_trial, y=pwr, group=cond, color=cond)) 
  
  # now plot the data
  exp.p <- data %>% na.omit() %>% ggplot(aes(x=cond_trial, y=move_mu, group=cond, color=cond)) +
                geom_point() +
                geom_line(aes(x=cond_trial, y=exp, group=cond, color=cond))
  grid.arrange(pwr.p, exp.p)
}

