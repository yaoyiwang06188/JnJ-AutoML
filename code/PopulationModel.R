#Import all of our libraries
library(rJava)
options(java.parameters = "-Xmx8048m")
library(RJDBC)
library(DBI)
library(vcd)
library(grid)
library(devtools)
library(tidyverse)
library(Hmisc)
library(vcd)
library(dismo)
library(dplyr)
library(MLmetrics)
library(Boruta)
library(caret)
library(e1071)
library(xgboost) #xgboost
library(ranger) #fast random forest
library(LiblineaR) # regularized logistic regression

## --------------------------------
## General Variables
## --------------------------------
redShiftDriver <- JDBC("com.amazon.redshift.jdbc41.Driver", 
                       "RedshiftJDBC41-1.2.8.1005.jar", identifier.quote="`")
# # put Redshift jar file in your home directory
resultSet <- data.frame()

#SAVED IN .Renviron file
rhealthDBName <- "saf" ## Set dataset you want to connect to (full list provided here:  https://jnj.sharepoint.com/sites/PHM-GCSP-RND/RWE/Pages/rHEALTH-Database-Connectivity.aspx)
# 
# 
# ## --------------------------------
# ## Connection (do not change)
# ## --------------------------------
connectionString <- paste("jdbc:redshift://rhealth-prod-4.cldcoxyrkflo.us-east-1.redshift.amazonaws.com:5439/", rhealthDBName,"?ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory", sep="")
conn <- dbConnect(redShiftDriver, connectionString, Sys.getenv("username"),
                  Sys.getenv("password"))
# 
res <- dbSendQuery(conn,'select * from scratch_col_hipfx_saf.np_col_hipfx_ads_v9 xxx') 
# # xxx is the data that you are getting from the scratch space
# 
# 
# #Loading Data

df_medicare <- fetch(res,n = -1) 

#removing hospitals that will be removed from hospital model

#creating a vector of providers that have more than 100 observations
df_medicare_keep <- df_medicare %>% group_by(prvdr_num) %>% summarise(num = length(prvdr_num)) %>%
  filter(num > 99) %>% pull(prvdr_num)

#filtering for only providers with the 100 observations
df_medicare <- df_medicare %>% filter(prvdr_num %in% df_medicare_keep)


##removing rows that dont have enough positive observations
#getting lists of hospitals that have at least 10 positive observations for er visit and filter out those that do nott
#er_90days is either 0 or 1 (boolean)
er_keep = df_medicare %>% group_by(prvdr_num) %>% summarise(sum_val = sum(er_90days)) %>% filter(sum_val >= 10) %>% pull(prvdr_num)
df_medicare <- df_medicare %>% filter(prvdr_num %in% er_keep)


#getting lists of hospitals that have at least 10 positive observations for mortality and filter out those that do nott
#mortality_90 is either 0 or 1 (boolean)
m_keep = df_medicare %>% group_by(prvdr_num) %>% summarise(sum_val = sum(mortality_90)) %>% filter(sum_val >= 10) %>% pull(prvdr_num)
df_medicare <- df_medicare %>% filter(prvdr_num %in% m_keep)

#getting lists of hospitals that have at least 10 positive observations for readmission and filter out those that do nott
#readm_flag 90 days is either 0 or 1 (boolean)
r_keep = df_medicare %>% group_by(prvdr_num) %>% summarise(sum_val = sum(readm_flag)) %>% filter(sum_val >= 10) %>% pull(prvdr_num)
df_medicare <- df_medicare %>% filter(prvdr_num %in% r_keep)


# -----------------------------
#Removing columns that leak info / have no possibility of being helpful
#Note that more columns were removed when compared to the hospital model,
#in order to not run into memory issues

columns_to_remove <- c('version_id', 'desy_sort_key', 're_claim_no', 'follow_up_end_dt', 'er_claim_no', 'claim_no',
                       're_clm_admsn_dt', 're_nch_bene_dschrg_dt', 're_clm_utlztn_day_cnt_365','re_clm_pmt_amt_365','re_pdx_365',
                       're_ppx_365','re_adm_source_365','re_disc_status_365','re_pdx_desc_365', 'X1',
                       're_ppx_desc_365','date_of_death_365','mortality_365','er_365','dayser_claim_no_365',
                       'er_rev_cntr_dt_365','er_clm_thru_dt_365','er_prncpal_dgns_cd_365','er_pdx_desc_365',
                       'er_hcpcs_cd_365','er_hcpcs_desc_365', 'cont_enroll_flag_365f','hmo_enroll_flag_365f',
                       'mortality_365_up','re_claim_no_365_up','readm_flag_365_up','re_clm_admsn_dt_365_up','re_nch_bene_dschrg_dt_365_up',
                       're_prvdr_num_365_up','re_clm_utlztn_day_cnt_365_up','re_clm_pmt_amt_365_up','re_pdx_365_up','re_ppx_365_up',
                       're_adm_source_365_up','re_disc_status_365_up','re_pdx_desc_365_up','re_ppx_desc_365_up','er_365days_uper_claim_no_365_up',
                       'er_rev_cntr_dt_365_up','er_clm_thru_dt_365_up','er_prncpal_dgns_cd_365_up','er_pdx_desc_365_up',
                       'er_hcpcs_cd_365_up','er_hcpcs_desc_365_up', 'er_claim_no_365_up','er_365days_up', 're_prvdr_num_365',
                       'follow_up_end_dt_365', 're_claim_no_365', 'readm_flag_365', 're_clm_admsn_dt_365',
                       're_nch_bene_dschrg_dt_365', 're_prvdr_num_3650','er_365days', 'er_claim_no_365',
                       'er_clm_thru_dt', 'er_rev_cntr_dt', 'er_prncpal_dgns_cd', 'er_pdx_desc', 'er_hcpcs_cd',
                       'er_hcpcs_desc', 're_prvdr_num', 're_clm_utlztn_day_cnt', 're_clm_pmt_amt', 're_pdx',
                       're_ppx', 're_adm_source', 're_disc_status', 're_pdx_desc', 're_ppx_desc', 
                       'valid_date_of_death_1825b_89f', 'cont_enroll_flag_1825b_89f',
                       'hmo_enroll_flag_1825b_89f', 'elix_score_1825_days_b', 'cci_score_1825_days_b',
                       'fci_score_1825_days_b', 'pdx_desc', 'ppx_desc','at_physn_npi', 'op_physn_npi', 'prvdr_cbsa_desc', 'prvdr_cbsa',
                       'prvdr_cbsa_desc', 'prvdr_msa_desc','hospital_name')


df_medicare <- df_medicare[, !(colnames(df_medicare) %in% columns_to_remove)]

#this line is sampling, which allows us to sample the data to the point the model can run
#set.seed(1)
#df_medicare<-sample_frac(df_medicare, 0.2)


#changing character to factor
df_medicare[sapply(df_medicare, is.character)] <- lapply(df_medicare[sapply(df_medicare, is.character)], 
                                                         as.factor)

#calculating variance of each column by group
df_variance <- df_medicare %>% summarize_all(funs(var), na.rm = T) 

#summing variance accross groups
columns_to_keep <- df_variance[,names(df_variance)]  %>% summarize_all(funs(sum), na.rm = T)

#adding provider num back
columns_to_keep <- columns_to_keep %>% mutate(prvdr_num = 1)

#checking if column has a non zero variance
columns_to_keep = columns_to_keep != 0

#getting column names of columns with non zero variance
columns_to_keep = colnames(columns_to_keep)[columns_to_keep]

#removing unnecessary columns
df_medicare <- df_medicare[,names(df_medicare) %in% columns_to_keep]

#setting -999 to NA
df_medicare[df_medicare == -999] = NA


#creating variables - feature engineering
df_medicare <- df_medicare %>% mutate(age_interval = cut(age, breaks = c(0,65,70,75,80,85,90,95,100)),
                                      diagnosis_type = case_when(substr(prncpal_dgns_cd,1,1) == "S" ~ "S",
                                                                 substr(prncpal_dgns_cd,1,1) == "M" ~ "M",
                                                                 TRUE ~ "Other"),
                                      # index_loss: days in hospital
                                      los_interval = cut(index_los, breaks = c(0,1,2,5,10,20,40,80,160,320)),
                                      # clm_admsn_dt: first readmit date
                                      month_clm = as.numeric(substr(clm_admsn_dt,6,7)),
                                      # nch_bene_dschrg_dt: first discharge date for the first readmission
                                      month_dsch = as.numeric(substr(nch_bene_dschrg_dt,6,7)),
                                      
                                      season_clm = as.factor(case_when(month_clm <= 3 ~ "Winter",
                                                                       month_clm >= 10 ~ "Fall",
                                                                       month_clm > 3 & month_clm < 7 ~ "Spring",
                                                                       TRUE ~ "Summer")),
                                      season_dsch = as.factor(case_when(month_dsch <= 3 ~ "Winter",
                                                                        month_dsch >= 10 ~ "Fall",
                                                                        month_dsch > 3 & month_dsch < 7 ~ "Spring",
                                                                        TRUE ~ "Summer")),
                                      # number of days between the first readmit date and the first entry's 1/1/year
                                      time_term_clm = as.numeric(substr(clm_admsn_dt,9,10)) + 
                                        month_clm * 30 + 365 * (yr_adm - min(yr_adm)),
                                      # number of days between the discharge date of the first readmit date and the first entry's 1/1/year
                                      time_term_dsch = as.numeric(substr(nch_bene_dschrg_dt,9,10)) + 
                                        month_dsch * 30 + 365 * (yr_disch - min(yr_disch)),
)

#Removing some additional columns that were made redundant by the feature engineering
df_medicare <- df_medicare[, !(colnames(df_medicare) %in% c('clm_admsn_dt', 'nch_bene_dschrg_dt', 'month_clm', 'month_dsch'))]

#Want to ensure that the factors are correctly identified as factors
df_medicare$clm_drg_cd <- df_medicare$clm_drg_cd %>% as.factor()
df_medicare$ot_physn_npi <- df_medicare$ot_physn_npi %>% as.factor()
df_medicare$yr_adm <- df_medicare$yr_adm %>% as.factor()
df_medicare$yr_disch <- df_medicare$yr_disch %>% as.factor()
df_medicare$fy <- df_medicare$fy %>% as.factor()
df_medicare$gndr_cd <- df_medicare$gndr_cd %>% as.factor()
df_medicare$bene_race_cd <- df_medicare$bene_race_cd %>% as.factor()


#label variables, target y
label_variables <- c('mortality_90', 'readm_flag', 'er_90days')


#We use this to save our scores
model_scores <- c('xgb_LogLoss','xgb_AUC','xgb_acc',
                  'rf_LogLoss','rf_AUC','rf_acc',
                  #'logistic_LogLoss','logistic_AUC','logistic_acc',
                  'reglogistic_LogLoss','reglogistic_AUC','reglogistic_acc',
                  'xgb_eta','xgb_max_depth','xgb_colsample_bytree','xgb_subsample','xgb_nrounds',
                  'rf_mtry','rf_splitrule',
                  'reglogistic_loss', 'reglogistic_epsilon',
                  'reglogistic_cost')

columns_to_keep <- df_medicare %>% colnames()
columns_to_keep <- columns_to_keep[!(columns_to_keep %in% label_variables)]

#making empty data frame to store results
results <- data.frame( matrix(,1,length(model_scores) + 1) )
colnames(results) <- c(model_scores)

#we want to save a csv of our results (we will write to these in the for loop below)
write.csv(results, 'results_mortality_90_full_set.csv', row.names = F)
write.csv(results, 'results_er_90days_full_set.csv', row.names = F)
write.csv(results, 'results_readm_flag_full_set.csv', row.names = F)

#####CREATING THE POPULATION MODEL
#---------------------------------

#remove columns with NA
df_medicare <- df_medicare[,df_medicare %>% is.na() %>% colSums() == 0]

#calculating variance in hospital similar to above
df_variance <- df_medicare %>% summarize_all(funs(var), na.rm = T) 
columns_to_keep_2 <- df_variance %>% summarize_all(funs(sum), na.rm = T) != 0

#getting column names of columns with non zero variance
columns_to_keep_2 = colnames(columns_to_keep_2)[columns_to_keep_2]

#ensure that each target has 10 positive observations (they do)
label_variables_hospital <- (df_medicare[,label_variables] %>% colSums() >= 10) & 
  ((1 - df_medicare[,label_variables]) %>% colSums() >= 10)

label_variables_hospital <- names(label_variables_hospital)[label_variables_hospital]
df_medicare_temp <- df_medicare

#This generates the models for each target variable
for(lab in label_variables_hospital){
  
  iteration_start_time <- Sys.time()  
  #setting random seed & setting folds
  nfolds = 5 
  set.seed(1)
  
  #Seperating into X and y
  X <- df_medicare_temp[,columns_to_keep_2]
  
  ## replacing X$prvdr_num by mean target variable 
  #This is a potential method to represent pvdr num, but we found that
  #it leaked target information
  #prvdr_num_target_mapping <- X %>% group_by(prvdr_num) %>%summarise(mean_target= mean(mortality_90))
  #X <- merge(X, prvdr_num_target_mapping, by="prvdr_num")
  
  # remove column 'prvdr_num'
  X <- X[, !(colnames(X) %in% c("prvdr_num"))]
  
  #removing labels
  # X <- X[,!(names(X) %in% label_variables)]   ### instead of removing here, remove after extracting y label variable !!!!
  
  #form data into folds
  y <- X %>% pull(lab)
  X <- X[,!(names(X) %in% label_variables)]
  # need to confirm the data type of kfold, whether it will do cross validation 
  fold <- kfold(y, k = nfolds, by = y)
  
  # Feature Selection based on Boruta Alg ????????
  X_train_all_features <- X[fold != nfolds,]
  y_train <- y[fold != nfolds]
  boruta_start_time <- Sys.time()
  sigtest <- Boruta(y_train~. , data = cbind(X_train_all_features,
                                             y_train), holdHistory=FALSE, doTrace=2)   # reduce memory footprint: https://cran.r-project.org/web/packages/Boruta/vignettes/inahurry.pdf
  boruta_end_time <- Sys.time()
  print(boruta_start_time - boruta_end_time)
  cat("Boruta Finishes.")
  features <- getSelectedAttributes(sigtest) 
  X = X[,features]
  
  # save selected features
  write.csv(features, paste0('boruta_features_', lab,'_full_set.csv'), row.names = F)
  
  # create the controller which will be used to train different model types using caret
  fitControl <- trainControl(method = "cv",
                             number = 4,
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  
  #Get X Train / XTest
  X_train <- X[fold != nfolds,]
  X_ranger_train <- cbind(X_train, y_train)
  X_test <- X[fold == nfolds,]
  y_test <- y[fold == nfolds]
  X_ranger_test <- cbind(X_test, y_test)
  combined_train <- cbind(X_train, y_train)
  
  #print(paste0('Currently on Logistic Model for label: ',lab))
  
  #TAKES TOO MUCH MEMORY:
  
  # -------------------------GLM model-------------------------
  #logistic <- glm(formula = y_train ~ ., data = combined_train, family = 'binomial')
  
  #for(cat_name in logistic$xlevels %>% names()){
  #  logistic$xlevels[cat_name] <- levels(X_train)
  #}
  
  #logistic_y_pred <- predict.glm(logistic, type = 'response', newdata = X_test)
  #logistic_y_true <- y_test
  
  #results['logistic_LogLoss'] = LogLoss(logistic_y_pred, logistic_y_true)
  #results['logistic_AUC'] = AUC(logistic_y_pred, logistic_y_true)
  #results['logistic_acc'] = Accuracy(round(logistic_y_pred), logistic_y_true)
  
  #print('Completed Logistic Model')
  
  
  #now we update the y_train to be a factor
  combined_train$y_train <- as.factor(combined_train$y_train)
  levels(combined_train$y_train) <- c("Class_0", "Class_1")
  
  # -------------------------reglogistic model-------------------------
  
  print(paste0('Currently on RegLogistic Model for label: ',lab))
  reglogistic_start_time <- Sys.time()
  regLogisticFit <- train(y_train ~ .,
                          data = combined_train,
                          method = "regLogistic",
                          trControl = fitControl,
                          metric="ROC")
  
  # optimal random forest (ranger) params
  results['reglogistic_epsilon'] <- regLogisticFit$finalModel$tuneValue$epsilon
  results['reglogistic_loss'] <- regLogisticFit$finalModel$tuneValue$loss
  results['reglogistic_cost'] <- regLogisticFit$finalModel$tuneValue$cost
  
  # predict on validation data(test data?????)
  reglogistic_prediction <- predict(regLogisticFit,
                                    newdata = X_test, type = "prob")
  reglogistic_y_pred = reglogistic_prediction$Class_1
  reglogistic_y_true = y_test
  
  # get results
  results['reglogistic_LogLoss'] = LogLoss(reglogistic_y_pred,
                                           reglogistic_y_true)
  results['reglogistic_AUC'] = AUC(reglogistic_y_pred, reglogistic_y_true)
  results['reglogistic_acc'] = Accuracy(round(reglogistic_y_pred),
                                        reglogistic_y_true)
  print('Completed Reglogistic Model')
  reglogistic_end_time <- Sys.time()
  
  # -------------------------XGBoost model-------------------------
  
  print(paste0('Currently on XGBoost for label: ',lab))
  xgboost_start_time <- Sys.time()
  xgbTreeFit <- train(y_train ~ .,
                      data = combined_train,
                      method = "xgbTree",
                      trControl = fitControl,
                      metric="ROC",
                      verbose=TRUE)
  
  print('XGBoost model completed')
  
  # optimal xgboost params
  results['xgb_eta'] <- xgbTreeFit$finalModel$tuneValue$eta
  results['xgb_max_depth'] <- xgbTreeFit$finalModel$tuneValue$max_depth
  results['xgb_colsample_bytree'] <- xgbTreeFit$finalModel$tuneValue$colsample_bytree
  results['xgb_subsample'] <- xgbTreeFit$finalModel$tuneValue$subsample
  results['xgb_nrounds'] <- xgbTreeFit$finalModel$tuneValue$nrounds
  
  # predict on validation data
  xgb_prediction <- predict(xgbTreeFit, newdata = X_test, type = "prob")
  xgb_y_pred = xgb_prediction$Class_1
  xgb_y_true = y_test
  
  # get results
  results['xgb_LogLoss'] = LogLoss(xgb_y_pred, xgb_y_true)
  results['xgb_AUC'] = AUC(xgb_y_pred, xgb_y_true)
  results['xgb_acc'] = Accuracy(round(xgb_y_pred), xgb_y_true)
  
  
  print(paste0('Currently on RF for label: ',lab))
  xgboost_end_time <- Sys.time()
  
  
  # -------------------------Random Forest (ranger) model-------------------------
  rf_start_time <- Sys.time()
  rfFit<- train(y_train ~ .,
                data = combined_train,
                method = "ranger",
                trControl = fitControl,
                metric="ROC")
  
  # optimal random forest (ranger) params
  results['rf_mtry'] <- rfFit$finalModel$tuneValue$mtry
  results['rf_splitrule'] <- rfFit$finalModel$tuneValue$splitrule
  
  # predict on validation data
  rf_prediction <- predict(rfFit, newdata = X_test, type = "prob")
  rf_y_pred = rf_prediction$Class_1
  rf_y_true = y_test
  
  # get results
  results['rf_LogLoss'] = LogLoss(rf_y_pred, rf_y_true)
  results['rf_AUC'] = AUC(rf_y_pred, rf_y_true)
  results['rf_acc'] = Accuracy(round(rf_y_pred), rf_y_true)
  
  print('Completed RF Model')
  rf_end_time <- Sys.time()
  
  #Save every time we complete a label
  iteration_end_time <- Sys.time()  
  write.table(results, paste0('results_', lab,'_full_set.csv'), append = T, row.names = F, sep=',',col.names = F)
  cat('Current Label: ', lab)
  cat("Boruta finishes in %s mins.", boruta_end_time - boruta_start_time)
  cat("RegLogistic finishes in %s mins.", reglogistic_end_time - reglogistic_start_time)
  cat("Xgboost finishes in %s mins.", xgboost_end_time - xgboost_start_time)
  
  cat("Random Forest finishes in %s mins.", rf_end_time - rf_start_time)
  cat('The entire iteration finishes in %s mins. ', iteration_end_time - iteration_start_time)
}
