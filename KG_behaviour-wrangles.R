# Written by K. Garner, 2020
# Wrangle pre/post data for further analysis
# --------------------------------------------------------------------------
datPath <-  'raw-behav-data/'

## --------------------------------------------------------------------------
# Data prep

# load raw session 1 & 2 data, use this data to calculate accuracy per condition
dat.raw <- GetPrePostDataRaw(datPath)

# load session 1 & 2 data,  using the function that cleans RTs, and removes incorrect responses
dat <- GetPrePostClean(datPath)

# this function takes the multitasking data,
# filters out any multitask trials where both tasks were not performed correctly
# it then adds condition classification mult_cond -> single (single task), first-multi (first task executed)
# and second multi (second task executed)
dat.recoded <- RecodeMultiData(dat)
dat.recoded$mult_cond <- factor(dat.recoded$mult_cond, levels=c("single", "multi-first", "multi-second"))



