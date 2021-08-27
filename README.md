# apa_table-for-mixed-models-with-multiple-imputations
This is a demonstration of how to using papaja::apa_table() to format tables with fixed and random effects. 

I use the following R packages: 
1. mitools - For data and results extraction
2. Amelia - For multiple imputation
3. tidyverse - For data wrangling
4. lme4 - For multilevel models
5. broom.mixed - For extracting results
6. papaja - For creating an APA-formatted manuscript with the table.

The following files are included in this repository:
1. Prep code.R - Loads dataset, runs multiple imputation, runs multilevel models, extracts results, saves extracted results in a dataframe ("PISATable.RData")
2. Create table.Rmd - Loads in "PISATable.RData" and uses papaja to format and render the table into a PDF ("Output.pdf")
3. apa7.csl - Needed to format manuscript according to APA 7 guidelines. 
4. PISATable.RData - Output of "Prep code.R" and input into "Create table.Rmd"
5. Output.pdf - Final papaja output

If you have a more elegant solution, please let me know! 


