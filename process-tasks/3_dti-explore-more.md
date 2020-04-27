# DTI data - exploratory analysis, next steps

Now that you have performed your initial data import and coding changes, lets finish our exploratory analysis of the DTI data

## coding
--To start, open YOURINTITIALS_dti-exploratory.R and re-run all the code written so far.

1. Perform some simple visualisations of the _session 1_ data to identify outliers and determine the normality of the data - for example [boxplots](https://ggplot2.tidyverse.org/reference/geom_boxplot.html) and [qqplots](https://ggplot2.tidyverse.org/reference/geom_qq.html). 

2. Decide whether to exclude any outlliers. If so, remove them from the data frame. [The filter function will help.](https://dplyr.tidyverse.org/reference/filter.html)

3. Perform a reliability analysis - i.e. for those subjects for whom we have _session 1_ and _session 2_ data, perform correlation analyses for each tract. Higher correlations = higher reliability. See Danielle Navarro's [book](https://learningstatisticswithr.com/) for help with coding correlation tests.

4. Plot your final distribution as a raincloud plot, save it in the folder 'plots' as YOURINITIALS_dti-rainclouds. Example code is available [here](https://www-ncbi-nlm-nih-gov.ezproxy.library.uq.edu.au/pmc/articles/PMC6480976/) 

5. Save clean data as a csv file in cleaned-data as YOURINITIALS_dti-cleaned.csv. Details of the write.csv function are [here](https://www.rdocumentation.org/packages/AlphaPart/versions/0.8.1/topics/write.csv)

**Once you have done this, add your code file, and your new .csv file to your repository. Now repeat the process of making the request to add your changes to the project repository**