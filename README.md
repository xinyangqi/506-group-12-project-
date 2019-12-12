# 506-group-12-project

## Data and substantial question
  This project uses NHANES data (2005-2006) to answer the question whether people with more physial activity gets longer sleep. 
  
## Group members and tools 
  Jingwen Xiao (R--dplyr & mgcv), Ting-Wei Lin (R--data.table & splines) and Xinyang Qi (Python--pandas & statistic models)

## Outline
Our current outline includes the following items:
  - Data cleaning, including variable selection, handle of missing value and some level of factor recoding. 
  - Model building. Current main model is spline regression, with some early attempt on OLS and mixed models. 
  - Comparison using summary statistic metrics and/or cross validation with other models implied, depending on the goodness of fit. 
  - Visualization and comments on the results. Tools for this part are probably ggplot2/base for R and matplotlib for Python. 

## Uploaded Files 
 
 **Xinyang Qi**: Under "xinyang" file
 - 506_group_project_Xinyang Qi.ipynb (The version of Python solutions. Contains data cleaning, variable selection based on OLS outcomes and covariance matrices, and a primary GLS solution using **statistic models**. Current GLS model includes a cubic spline modification for activity duration.)
  
 **Jingwen Xiao**: Under "Jingwen Xiao file" file
 - group_project_1.R (The first version of R solutions using packages **dplyr** and **mgcv**. Contains data cleaning, variable recoding and selection based on primary GAM model outcomes and a simple OLS for reference. Current GAM model includes factor variables and smooth splines up to k = 3.)
 - group_project_1.Rmd (Corresponding Rmarkdown file.)
 - group_project_1.html (Corresponding HTML file. Have to be downloaded for previewing.)
 - group_project_1.1.R (A slightly modified version assessing the effect of single variable **pad**. Contains graphs for three versions of processed dataset.)
 - group_project_2.R (The second version with more revision/addition:
   - Modify the restrictions on valid respondents.
   - Cancel restrictions on **k**.
   - Add a jitter to response **sleep** to mimic real-life situation.
   - Make attempts towards cross-vaildation, though somewhat failed.
   - Calculate the RMSEs. )
 - group_project_3.R (The third version with minor revision from group member suggestions.)
 - group_project_Jingwen_Xiao.R (The final version with full analysis and comments.)
 - group_project_Jingwen_Xiao.Rmd (Corresponding Rmarkdown file.)
 - group_project_Jingwen_Xiao.html (Corresponding HTML file. Have to be downloaded for previewing.)
 - data and variable description.Rmd (Data/variable description for the report.)
 
 **Ting-Wei Lin**: Under "TingWeiLin" file
  - group_project_tingwei.R (The last version of R file, contains R solutions using packages **data.table** and **splines**. Contains data cleaning, variable selection based on OLS outcomes and an OLS model keeping the main variables of interest.)
  - group_project_tingwei.Rmd (Corresponding Rmarkdown file.)
  - group_project.Rmd (The Rmd file, the combination of the group projects)
  - group_project.html (Corresponding HTML file. Have to be downloaded for previewing.)

## Collaboration

Most ideas are share via communication APP and emails.
For group member reviews and discussions, see the **issues** section.
Past group meeting dates:
- Nov. 23rd
- Nov. 25th
- Dec. 3th
- Dec. 8th
- Dec. 11th
