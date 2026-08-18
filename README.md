# NYC-Parks_Dashboard
R Shiny App for analysis and figure production for environmental compliance reports on NYC wetlands


#How to download this app on your computer:

Download as a ZIP File.

1. Click the green Code button near the top right of the repository page.

2. Click Download ZIP from the drop-down menu.

3. To analyze new data, download the Master .csv file from the marsh project on Sharepoint. 

4. Save this .csv file under the /data folder in the Dashboard ZIP download. 


#Running the app:

1. To get started open the app.R file in R Studio.

2. Click the Run App button with a green, right-pointing arrow located at the top right of the panel containing the code.

3. The app should pop up in a separate window pane. 

4. Under Upload Wetland Data, click Browse and navigate to the /data folder inside the ZIP folder containing the app project.

5. Select the .csv file containing the Master monitoring data for the marsh you are analyzing. 

6. Under Response Variable, select from the drop down menu the response variable you would like to run analyses for. Default is total percent cover.

7. Under Treatments, the different treatment types found in the data set will be populated. If you are interested in analyzing across all treatments, leave all boxes checked. 
If you are interested in running an analysis between a subset of treatments, deselect the box(es) you would like to leave out. 
NOTE: At least two treatment groups must be selected!

8. Marsh types found the data set will be populated here. This information is derived from the "area" column. Usually this is HM for high marsh 
and LM for low marsh in the reports. To run analyses for each marsh type, click onthe other marsh type and hit delete, leaving the marsh type 
you would like to analyze. To add a marsh type back in, click into the box and select the marsh type you would like to add in from the drop down menu. 
NOTE: It is recommended to run HM and LM analyses separately, as the expected values for different response variables are different and comparing low marsh
to high marsh metrics is likely not informative since low marsh is expected to be dominated by a different set of vegetative species and invertebrate
presence is also different. Running LM and HM together can obscure the results and reduce ability to detect metric improvements over time. 

9. Under Analysis Type, select Single Year analysis if you would like to run an ANOVA or Generalized Linear Model within a season/calendar year.
The model tests response variable by treatment. 
Select, Across Years if you would like to run an ANOVA or GLM over time, across all available seasons/calendar years in the data set so far.
The model tests response variable by treatment and the interaction of time. 

10. Your selections are reflected in the three boxes at the top of the app. This is just to double check your selections are what you intended.
You can see which marsh types are selected, what years, and what the sample size is (how many plots).

11. Click Run Analysis to run the analysis based on your selections. It may take a few seconds, but then the tabs in the app should be populated.


#What each tab in the app provides:

1. Data distribution - 
  a histogram of the selected response variable for each of the selected treatments. The histogram is useful for understanding 
where data is lacking and helping give context to the results of the analyses.

2. Boxplot - 
  provides a visual cue for the median, interquartile range, whiskers, and outliers.
  
3. Diagnostics and Assumptions - 
  If the residuals are distributed normally enough to run an ANOVA, most of the black dots should fall on or near to the red line in the QQ Plot. 
Some deviation at the far ends is acceptable. There is no real hard rule for accepting normality, use you best judgement on how well the residuals 
fit the line. In the Residuals vs. Fitted plot, points should be scattered around the horizontal line at 0 and not create and distinct pattern or shapes. 
For some data, there may be distinct diagonal lines. This should be alright as long as the spread on the y axis is fairly even. 
Shapiro-Wilk tests for a normal data distribution. If this is significant, inspect the QQ Plot and Residuals to determine if a non-parametric test is more appropriate. 
Levene's test tests for equal variances across samples. Levene's test is only reported for 'Single Year' analysis, as repeated measures over time violates assumptions of the test. 
For 'Across Years' the bottom box in this panel should say NULL. If Levene's is significant, non-parametric tests should be used. 

4. ANOVA - 
  Part of the output from 'Run Analysis' of the parameters selected in the left-hand panel. You can change selections in the left-hand panel, select 'Run Analysis' again
and the ANOVA will update for those parameters. See the panel for notes on how to interpret the columns in the ANOVA output table.

5. Estimated Marginal Means - 
  The other half of the output from 'Run Analysis'. Two types of tables will be printed $emmeans and $contrasts. See the tab for interpretting the 
table columns. One of each table will be printed for each year included in the analysis.
  
6. Interpretation - 
  This panel highlights any significant effects from the ANOVA and which post-hoc comparisons specifically were signifcant from emmeans.  

7. Non-parametric tests -
  When adequate normality is not met in the Diagnostics and Assumptions tab, this tab allows you the option of running a non-parametric test on the data. 
See further explanation in the tab. A Wilcoxon rank-sum test will run for Single Year analysis and a Kruskal Wallis will run if Across Years is elected. 

8. Elevation - 
  Box and whisker plots are generated for each treatment's NAVD88 (ft) elevation measures. Tidal and planting benchmarks for the figure can be altered manually
using the text boxes below the generated figure. The text alterations should be reflected live in the figure. Below that are options for 
dimensions and resolutions for downloading the figure as a .pdf by clicking the Download Figure button. 

9. Bar graphs for year comparisons -
  Bar graphs are generated for the response variable selected in the left-hand panel. Bars represent the mean for that group and include
standard error bars. The printed numerical value of the mean can be toggled off an on in the chart by clicking the check box next to 'Show mean values'. 
Years to display can also be edited by deleting or selecting back years to include in the chart. All years can be included or specific changes in mean
can be inspected in a simpler grpah by comparing just two consecutive years, for example. Click 'Update Plots' to see the new chart after altering the year inputs. 

10. Count comparisons -
  The final tab is separated from the left-hand panel entirely and is used to generate count comparisons within a single year. The year can be selected from the drop-down
Year menu. Count Variables can be added in by clicking into the variable box and selecting from the drop-down menu. Printed mean values can be toggled on and off
with the check box. 


#Amendments to the code:

If you need to add measurement variables from the data sheet that are not currently in the app
drop-down menu, you can manually add them following these steps:

1. Open the app.R file in R Studio.

2. Under the heading "#Naming of R-cleaned variables"" from the data sheet, add in the R-cleaned variable name to the list,
surrounded by parentheses. Make sure to add a comma after the previous item in the list. 

3. You can check the R-cleaned variable name once you have loaded your data set into the app. In the R Studio Console tab,
on the bottom left, the first six lines of all columns of the data set will print along with the clean names of each column.


If you need to add in marsh types other than LM and HM:

1. Open the app_functions.R file in R Studio.

2. Under the 'preprocess_data' function there is a header called "Create marsh from area". Add the name of the marsh type in quotes to the end of the list,
in this format: grepl("Grassland", area, ignore.case = TRUE) ~ "Grassland"; see the example in the code. 

3. Scroll down to the header "Keep only High and Low Marsh observations" and add the new marsh type to the list in quotes. 


If you would like to add additional treatments types beyond "restored", "reference", and "cluster":

1. Open the app_functions.R file in R Studio.

2. Under the 'preprocess_data' function there is a header called "Always derive treatment from area". Add the name of the treatment in quotes to the list just beneath "Cluster",
use this format grepl("cluster", area, ignore.case = TRUE) ~ "Cluster"; see the example in the code. 

3. Scroll down to the header "Set Reference as baseline treatment" and add the new treatment type to the list in quotes. 


#Prompts for AI to help with code: