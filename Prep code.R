library(mitools) #data and results extraction
library(Amelia) #multiple imputation
library(tidyverse) #data wrangling
library(lme4) #multilevel model
library(broom.mixed) #tidying for mixed models

#Load Data#
data("pisamaths") #from the mitools package

#Impute Data#

pisamaths %>% 
  select(SCHOOLID, STIDSTD, ST04Q01, MATHEFF, PV1MATH:PV5MATH, W_FSTUWT) -> pisamaths #only selecting the variables I use in the demonstration

MI <- amelia(pisamaths, #dataset to impute
             m = 5, #number of imputations
             idvars = c(2,10),#ID variables and any variables you do not want to impute. Include those that do not have any missing data. 
             noms = 3,#categorical variables to impute
             cs = "SCHOOLID") #variable to cluster by.
#All variables not listed in the imputation syntax are continuous variables to impute.

#Note: I would usually save the output of 'MI' so that I do not have to re-impute data each time, but not doing that here.
#If you choose to do so, you can run the following:
#setwd("C:/Users/~/Imputed Data")
#write.amelia(obj=MI, file.stem = "imp_dat")
#pisa_MI<- list()
#filename <- list.files(path = "C:/Users/~/Imputed Data")
#for(i in 1:length(filename)){
#  pisa_MI[[i]] <- read.csv(filename[[i]])
#}

#Prepare imputed data for analyses#
pisa_MI<- MI[[1]] 

for (i in 1:5){
  MATHACH <- paste("PV",i,"MATH",sep="") #distributes 1 plausible value to each of the five imputations
  pisa_MI[[i]] %>%
    dplyr::select(SCHOOLID, STIDSTD, ST04Q01, MATHEFF, W_FSTUWT,
                  !!MATHACH) -> pisa_MI[[i]] 
}

for (i in 1:5){
  names(pisa_MI[[i]])[grep("PV\\dMATH", names(pisa_MI[[i]]))] <- "MATHACH" #gives the same name to each of the PVs
  pisa_MI[[i]] %>% 
  mutate_at(vars(MATHEFF, MATHACH), scale) -> pisa_MI[[i]]#standardize continuous vars
  
} 


pisa_MI <- imputationList(pisa_MI) #create imputation list to use for lmer models


#Models#
Model1 <- with(pisa_MI,
               lmer(MATHEFF ~ MATHACH + (1|SCHOOLID), weights = W_FSTUWT, control = lmerControl(optimizer = "Nelder_Mead")))

Model2 <- with(pisa_MI,
               lmer(MATHEFF ~ MATHACH + ST04Q01 + (1|SCHOOLID), weights = W_FSTUWT, control = lmerControl(optimizer = "Nelder_Mead")))

Model3 <- with(pisa_MI,
               lmer(MATHEFF ~ MATHACH + ST04Q01 + (1 + MATHACH|SCHOOLID), weights = W_FSTUWT, control = lmerControl(optimizer = "Nelder_Mead")))
#Note: There are some singularity/convergence issues with the last model.
#Although it doesn't matter for current purposes, I used allFit() to see which optimizer worked best. 

#Extract Results#
m_list <- list(Model1, Model2, Model3) #list of lmer models using MI
fixed <- list() #create empty list to add fixed effects into
random <- list() #create empty list to add random effects into

for (i in 1:3) { #for each of the models
  MIextract(m_list[[i]], fun = fixef) -> coef #extract fixed effects
  MIextract(m_list[[i]], fun = vcov) -> varcov #extract variance-covariance matrix
  lapply(varcov, as.matrix) -> varcov
  summary(MIcombine(coef, varcov)) -> fixed[[i]] #combine fixed effect estimates and standard errors
  tibble::rownames_to_column(fixed[[i]]) -> fixed[[i]] #add a column with names of IVs
  fixed[[i]] %>% 
    select(c(rowname, results, se)) -> fixed[[i]] #select only IV names, estimates, and SEs
  m_list[[i]] %>% 
    map_dfr(tidy) %>% #create dataframe of all random effects
    filter(str_detect(term, "^sd__")) %>% #only retain standard deviations
    group_by(group, term) %>% 
    dplyr::summarise(results = mean(estimate)) %>% #average the SDs for a random effect across model with each imputation
    unite("rowname", 1:2) -> random[[i]] #this is useful if you have data with >2 nesting levels.
}

t.1 <- reduce(fixed, full_join, by = "rowname") #transform list of dataframes to one single df with all fixed effects across the 3 models
t.2 <- reduce(random, full_join, by = "rowname") #do same for random effects
t <- full_join(t.1, t.2) #combine fixed and random effects in one df

#I find it helpful to save 't' (or any final results table/figure you want to include in your MS) to an .RData file.
#and then load it into the R Markdown file, especially if there is a lot of data wrangling and analyses to do.

save(t, file = "C:/Users/~/PISATable.RData")

