'
  Script: Run all files.
  Description: Run the files in this script in the order provided for reproducibility of the analyses, from the raw data, to publication-ready figures
  
  Author: Valeria Bonapersona
  
  Input: output of data_cleaning.R 
  Output: analyzable dataset
  
'


'
Import manually coded file with relational data structure, and clean it to one dataframe
'
source("src/data_cleaning.R")


'
Uses the output from data_cleaning.R to prepare a dataset ready for analysis
'
source("src/data_preparation.R")


'
Removal outliers >> you should run it when you want the actual data for analysis
'

'
Visualizations
'
source("src/visualizations.R")
