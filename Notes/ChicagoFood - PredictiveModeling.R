## ----setup, include=FALSE------------------------------------------------
##caret book // http://topepo.github.io/caret/index.html
## common data manipulation  tidyverse
## magrittr - https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
## Skimmr - https://ropensci.org/blog/blog/2017/07/11/skimr
## https://www.amazon.co.uk/s/ref=sr_st_price-asc-rank?keywords=applied+predictive+modeling+kuhn&fst=as%3Aoff&rh=n%3A266239%2Ck%3Aapplied+predictive+modeling+kuhn&qid=1507895308&sort=price-asc-rank

install.packages(c('learnr', 'tidyverse', 'modelr', 'DBI', 'odbc', 'caret'))
install.packages(c('broom', 'FFTrees'), dependencies = TRUE)
install.packages(c('caret'), dependencies = TRUE)
require(learnr)

#includes basic data manipulation packages	
require(tidyverse)
require(modelr)
require(DBI)
require(odbc)
require(broom)
require (FFTrees)
require(caret)

## ----dbconn, eval=FALSE, echo=TRUE---------------------------------------
##steph locke prefers b/c open platform
require(DBI)
require(odbc)

driver = "ODBC Driver 13 for SQL Server"
server = "advr.database.windows.net"
database = "advr"
uid = "lockedata"
pwd = "zll+.?=g8JA11111"

dbConn <- dbConnect(odbc(), driver = driver, server = server, database = database,
    uid = uid, pwd = pwd)

## ----getdata-----------
iris <- dbReadTable(dbConn, "chicagofood")

#skimr - https://ropensci.org/blog/blog/2017/07/11/skimr
#Lookup AssertR - http://www.onthelambda.com/wp-content/uploads/2015/01/assertr.html
summary(iris)

## ----getdata-hint--------------------------------------------------------
chicagofood <- dbReadTable(dbConn, "chicagofood")
summary(chicagofood)


## ----tweaks-------------------------------------------
chicagofood %>% 
  mutate(
    pastSerious = pmin(pastSerious, 1),
    pastCritical = pmin(pastCritical, 1),
    ageAtInspection = ifelse(ageAtInspection > 4, 1L, 0L),
    consumption_on_premises_incidental_activity,
    heat_burglary = pmin(heat_burglary, 70),
    heat_sanitation = pmin(heat_sanitation, 70),
    heat_garbage = pmin(heat_garbage, 50),
    criticalFound=pmin(1, criticalCount),
    fail_flag=fail_flag) %>% 
  select(-pass_flag)->
  chicagofood

## ----summary, exercise=TRUE, exercise.eval=TRUE--------------------------
summary(iris)

## ----summary-hint, exercise.eval=TRUE------------------------------------
summary(chicagofood)

## ----analysedata-----------------------------------------------------------
library(tidyverse)

## dplyr ,  magrittr (pipe)
## univariate statistics / how data relates to one varible
chicagofood %>% 
  count(fail_flag, Inspector_Assigned) %>% 
  group_by(Inspector_Assigned) %>% 
  mutate(prop=scales::percent(n/sum(n))) %>%
  select(-n) %>% 
  spread(fail_flag,prop) ##pivot


## ----chart-----------------------------------------------------------
##use lubridate library
chicagofood %>% 
  group_by(Month=lubridate::month(Inspection_Date),Facility_Type) %>% 
  summarise(failrate=mean(fail_flag)) %>% 
  ggplot(aes(Month, failrate, 
             group=Facility_Type, colour=Facility_Type))+
  geom_line()

#saveRDS() -- will save R environment
#loadRDS() -- save to file or SQL Server

## ----sample---------------------------
#imputation - predict missing values
library(modelr)

iris %>% 
  resample_partition(c("train"=.7, "test"=.3)) ->
  irissample

irissample %>% 
  pluck("train") ->
  iris_train

irissample %>% 
  pluck("test") ->
  iris_test

## ----sample-hint---------------------------------------------------------
library(modelr)

chicagofood %>% 
  resample_partition(c("train"=.7, "test"=.3)) ->
  chicagofoodsample

chicagofoodsample %>% 
  pluck("train") ->
  chicagofood_train

chicagofoodsample %>% 
  pluck("test") ->
  chicagofood_test

## ----echo=FALSE----------------------------------------------------------
library(modelr)

chicagofood %>% 
  resample_partition(c("train"=.7, "test"=.3)) ->
  chicagofoodsample

chicagofoodsample %>% 
  pluck("train") ->
  chicagofood_train

chicagofoodsample %>% 
  pluck("test") ->
  chicagofood_test

## ----decisiontree-----------------------------------------------------------
#?select

chicagofood_train %>% 
  as_data_frame() %>% 
  select(-ends_with("ID"), -LICENSE_DESCRIPTION, 
         -(Inspection_Date:License), -ends_with("Count"), -ends_with("Found")) %>%
  FFTrees(fail_flag ~ ., ., do.comp=FALSE) ->
  dtree_model

plot(dtree_model)

## ----simple-glm-----------------------
## historically a stepwise regression to find most predictive variable
## -- then which variable added to minimise error
## -- keep adding variables till either n point reached or marginal information gain is minimal

## principal component / variable redction??
## use package caret for variable reduction  --- use GLMNET as an alternative to Feature REduction (multiply by 0)
require(caret)
chicagofood_train %>%
	as_data_frame() %>%
	select.if(is numeric) %>%
	cor( use = "complete.obs") %>%
	findCorrelation(cutoff=.8)

chicagofood_train %>%
	as_data_frame() %>%
	nearZeroVar()

chicagofood_train %>%
	as_dat_frame() %>%
	preProcess(methods = c("Scale", "Center", "pca", "nvz", "corr")) %>%


myglm<- glm(fail_flag ~ ageAtInspection + pastSerious, chicagofood , family="binomial")

## ----dtreefit-------------------------------------------------
chicagofood_test %>% 
  as_data_frame() %>% 
  add_predictions(dtree_model, "dtree") ->
  chicagofood_test

## ----addpred, exercise=TRUE,  exercise.lines = 5-------------------------

## ----addpred-hint--------------------------------------------------------
chicagofood_test %>% 
  as_data_frame() %>% 
  add_predictions(myglm, "glm") ->
  chicagofood_test

## ----confusionmatrix-----------------------------------------------------------
confusionMatrix(as.numeric(chicagofood_test$dtree), 
                chicagofood_test$fail_flag)

## ----echo=FALSE----------------------------------------------------------
classify<-function(x) as.numeric(x>0)

## ----glmpred, exercise=TRUE,  exercise.lines = 5-------------------------

## ----glmpred-hint--------------------------------------------------------
confusionMatrix(classify(chicagofood_test$glm), 
                chicagofood_test$fail_flag)
