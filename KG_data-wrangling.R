# Garner, KG. 2020
# This code contains the functions required to wrangle data into
# tidy form for the project 'pathways-to-practice'

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file
# source files and load packages
# ------------------------------------------------------------------
library(tidyverse)

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

###### Notes to add to dti-explore files
# 3. boxplots and qqplots - assess for outliers and normality
# --------------------------------------------------------------------------------
# see https://ggplot2.tidyverse.org/reference/geom_boxplot.html
# and https://ggplot2.tidyverse.org/reference/geom_qq.html


# 4. exclude outliers
# --------------------------------------------------------------------------------
# use filter https://dplyr.tidyverse.org/reference/filter.html


# 5. Plot final distributions as a raincloud 
# --------------------------------------------------------------------------------
# example code here https://www-ncbi-nlm-nih-gov.ezproxy.library.uq.edu.au/pmc/articles/PMC6480976/
