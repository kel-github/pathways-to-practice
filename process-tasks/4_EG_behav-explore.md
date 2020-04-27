# Multitasking behavioural data - exploratory analysis, EG

Now we will conduct our exploratory analysis of the behavioural multitasking data, taken from the scanning sessions of Garner & Dux, 2015

## coding
--To start, open EG_behaviour-exploratory.R, run the code as far as the end of the 'load raw data section.

1. To start, relabel and organise the factors in the raw.data dataframe

2. We first need to examine the accuracy data across participants for each of the conditions from _Session 1_. First make a new dataframe that calculates accuracy by condition and participant for session 1 only. Then make another dataframe that contains the group level mean accuracies for each condition. You can use the [group_by](https://dplyr.tidyverse.org/reference/group_by.html) and [summarise](https://dplyr.tidyverse.org/reference/summarise.html) functions for this.

2. Decide whether we need to set a minimum accuracy critria (for example, see Garner & Dux, 2015). Note it in this section of the code.

3. Now run the lines of code to get the 'clean.data'. The first function (GetPrePostClean) cleans each participant's data (retrieves only accurate responses, removes any response times that are < 250 ms or > than the mean + (2.5*std), for each condition. The second function (RecodeMultiData) further cleans and relabels the data so that a) the data no longer contains multitask trials where only 1 of the tasks was performed correctly, and b) relabels trials so that they are either 'single' or 'multi-first' (multitask trial, first task performed), or 'multi-second'

4. Filter participants so that those who scored below the minimum accuracy are removed, using filter (see Section 5.2 of this [chapter](https://r4ds.had.co.nz/transform.html)). Recode and reorder the factors of the resulting dataframe as necessary. 

5. Now we make our potential DV's of interest! Using 'group_by' and 'summarise' compute the mean, variance, [signal to noise ratio, (see alternative definition)](https://en.wikipedia.org/wiki/Signal-to-noise_ratio) and the [coefficient of variation](https://www.investopedia.com/terms/c/coefficientofvariation.asp) for each participant, condition, and session

6. Perform visual checks of the data using boxplots, and qq-plots, as you did for the tractography data

7. Exclude any outliers

8. From the examination of the data, we will decide on a DV of interest. Now compute the ratio between each multitask condition and the single task condition (multi-x / single) using the 'group_by' and 'summarise' functions

9. Perform a reliability analysis on the ratios, by correlating the measure between the two sessions, for the control group only

10. Plot your final distribution as a raincloud plot, save it in the folder 'plots' as YOURINITIALS_behav-plots. Example code is available [here](https://www-ncbi-nlm-nih-gov.ezproxy.library.uq.edu.au/pmc/articles/PMC6480976/)

11. Save clean data as a csv file in cleaned-data as YOURINITIALS_var-cleaned.csv. Details of the write.csv function are [here](https://www.rdocumentation.org/packages/AlphaPart/versions/0.8.1/topics/write.csv)

**Once you have done this, add your code file, and your new .csv file to your repository. Now repeat the process of making the request to add your changes to the project repository**



 