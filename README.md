# 506-group-12-project

## Data and substantial question
  This project uses NHANES data (2005-2006) to answer the question whether people with more physial activity gets longer sleep. 
  
## Group members and tools 
  Jingwen Xiao (R--dplyr & mgcv), Tingwei Lin (R--data.table & splines) and Xinyang Qi (Python--pandas & statistic models)

## Outline
Our current outline includes the following items:
  - Data cleaning, including variable selection, handle of missing value and some level of factor recoding. **(Mostly completed)**
  - Model building. Current main model is spline regression, with some early attempt on OLS and mixed models. **(In progress)**
  - Comparison using summary statistic metrics and/or cross validation with other models implied, depending on the goodness of fit. **(To do)**
  - Visualization and comments on the results. Tools for this part are probably ggplot2/base for R and matplotlib for Python. **(To do)**

## Uploaded Files 
 
 **Xinyang Qi**:
 - 506 group project.ipynb (The first version of Python solutions. Contains data cleaning, variable selection based on OLS outcomes and covariance matrices, and a primary GLS solution using **statistic models**. Current GLS model includes a cubic spline modification for activity duration.)
  
 **Jingwen Xiao**:
 - group_project_1.R (The first version of R solutions using packages **dplyr** and **mgcv**. Contains data cleaning, variable recoding and selection based on primary GAM model outcomes and a simple OLS for reference. Current GAM model includes factor variables and smooth splines up to k = 3.)
 - group_project_1.Rmd (Corresponding Rmarkdown file.)
 - group_project_1.html (Corresponding HTML file. Have to be downloaded for previewing.)
 
 **Ting-Wei Lin**:
 - group_project.R (The first version of R file, contains R solutions using packages **data.table** and **splines**. Contains data cleaning, variable selection based on OLS outcomes and an OLS model keeping the main variables of interest.)
 - group_project.html (Corresponding HTML file. Have to be downloaded for previewing.)

## Collaboration

Most ideas are share via communication APP and emails so far. Decent review files will be updated in the following days.
