# Practice behavioural data - exploratory analysis, GM

Now we will conduct our exploratory analysis of the behavioural practice data, for the multitask and visual search paradigms, taken from the practice sessions of Garner & Dux, 2015

## coding
--To start, open GM_prac-behaviour-exploratory.R, run the code as far as the end of the 'load raw data section.

1. To start, relabel and organise the factors in the raw.etc dataframes

2. We first need to examine the accuracy data across participants for each of the conditions across each block of trials. First make a new dataframe that calculates accuracy by block, condition and participant for each task. Then make another dataframe that contains the group level mean accuracies for each condition x block. You can use the [group_by](https://dplyr.tidyverse.org/reference/group_by.html) and [summarise](https://dplyr.tidyverse.org/reference/summarise.html) functions for this.

2. Decide whether we need to set a minimum accuracy critria (for example, see Garner & Dux, 2015). Note it in this section of the code.

3. Now run the lines of code to get the 'clean.[].datas'. The first function (GetPracticeMultiClean) cleans each participant's data (retrieves only accurate responses, removes any response times that are < 250 ms or > than the mean + (2.5*std), for each block and condition. The second function (RecodePracticeMultiData) further cleans and relabels the data so that a) the data no longer contains multitask trials where only 1 of the tasks was performed correctly, and b) relabels trials so that they are either 'single' or 'multi-first' (multitask trial, first task performed), or 'multi-second'. The 'GetPracticeVisSearchClean' function performs the same cleaning functions as outlined for the multitask data above.

4. Filter participants so that those who scored below the minimum accuracy are removed, using filter (see Section 5.2 of this [chapter](https://r4ds.had.co.nz/transform.html)). 

5. Lets make some summary statistics to get a better idea of the data. For each task, participant x block x condition, calculate the mean RT (again use group_by and summarise)

6. Perform visual checks of the data using boxplots, as you did for the tractography data

7. Exclude any outliers

8. Now we calculate moving averages of each participant's RTs in each condition. A moving average gives us a more stable estimate of the average response time over time. See https://en.wikipedia.org/wiki/Moving_average. You can run the existing code to do this for you. 

9. Now we will test the effect of our moving average window, while getting a feel for how the data looks at the participant level. Select 10 subjects from each task at random, and plot, for each participant, a line graph of their moving average RT, with trials on the x-axis, moving mu on the y-axis. You want a separate line for each condition and a shaded area around the line to indicate standard error
   e.g. http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
   e.g. https://ggplot2.tidyverse.org/reference/geom_ribbon.html
   the [facet_wrap](https://ggplot2.tidyverse.org/reference/facet_wrap.html) function will also be a big help here 

10. Save clean data as a csv file in cleaned-data as YOURINITIALS_var-cleaned.csv. Details of the write.csv function are [here](https://www.rdocumentation.org/packages/AlphaPart/versions/0.8.1/topics/write.csv)

**Once you have done this, add your code file, and your new .csv file to your repository. Now repeat the process of making the request to add your changes to the project repository**



 