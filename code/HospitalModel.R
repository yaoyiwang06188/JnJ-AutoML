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
library(LiblineaR)
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
rhealthDBName <- "saf"                ## Set dataset you want to connect to (full list provided here:  https://jnj.sharepoint.com/sites/PHM-GCSP-RND/RWE/Pages/rHEALTH-Database-Connectivity.aspx)
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


#creating a vector of providers that have more than 100 observations
df_medicare_keep <- df_medicare %>% group_by(prvdr_num) %>% summarise(num = length(prvdr_num)) %>%
  filter(num > 99) %>% pull(prvdr_num)

#filtering for only providers with the 100 observations
df_medicare <- df_medicare %>% filter(prvdr_num %in% df_medicare_keep)


##removing rows that dont have enough positive observations
#getting lists of hospitals that have at least 10 positive observations for er visit and filter out those that do nott
er_keep = df_medicare %>% group_by(prvdr_num) %>% summarise(sum_val = sum(er_90days)) %>% filter(sum_val >= 10) %>% pull(prvdr_num)
df_medicare <- df_medicare %>% filter(prvdr_num %in% er_keep)


#getting lists of hospitals that have at least 10 positive observations for mortality and filter out those that do nott
m_keep = df_medicare %>% group_by(prvdr_num) %>% summarise(sum_val = sum(mortality_90)) %>% filter(sum_val >= 10) %>% pull(prvdr_num)
df_medicare <- df_medicare %>% filter(prvdr_num %in% m_keep)

#getting lists of hospitals that have at least 10 positive observations for readmission and filter out those that do nott
r_keep = df_medicare %>% group_by(prvdr_num) %>% summarise(sum_val = sum(readm_flag)) %>% filter(sum_val >= 10) %>% pull(prvdr_num)
df_medicare <- df_medicare %>% filter(prvdr_num %in% r_keep)

#removing columns that leak info / have no possibility of being helpful
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
                       'fci_score_1825_days_b')


df_medicare <- df_medicare[, !(colnames(df_medicare) %in% columns_to_remove)]


#changing character to factor
df_medicare[sapply(df_medicare, is.character)] <- lapply(df_medicare[sapply(df_medicare, is.character)], 
                                                         as.factor)

##removing columns that have no variance accross provider number
#calculating variance of each column by group
df_variance <- df_medicare %>% group_by(prvdr_num) %>% summarize_all(funs(var), na.rm = T) 

#summing variance accross groups
columns_to_keep <- df_variance[,names(df_variance) != "prvdr_num"] %>% ungroup() %>% summarize_all(funs(sum), na.rm = T)

#adding provider num and ensuring it is not removed by 0 variance rule
columns_to_keep <- columns_to_keep %>% mutate(prvdr_num = 1)

#checking if column has a non zero variance
columns_to_keep = columns_to_keep != 0

#getting column names of columns with non zero variance
columns_to_keep = colnames(columns_to_keep)[columns_to_keep]

#keeping only columns with variance
df_medicare <- df_medicare[,names(df_medicare) %in% columns_to_keep]

#setting -999 to NA
df_medicare[df_medicare == -999] = NA


#creating variables
df_medicare <- df_medicare %>% mutate(#interval for age. Broken into 5 year intervals and under 65
                                      # correlation between age and age_interval?????
                                      age_interval = cut(age, breaks = c(0,65,70,75,80,85,90,95,100)),
                                      
                                      #creating a simpler version of diagnosis type
                                      diagnosis_type = case_when(substr(prncpal_dgns_cd,1,1) == "S" ~ "S",
                                                                 substr(prncpal_dgns_cd,1,1) == "M" ~ "M",
                                                                 TRUE ~ "Other"),
                                      
                                      #creating interval variable for length of stay
                                      los_interval = cut(index_los, breaks = c(0,1,2,5,10,20,40,80,160,320)),
                                      
                                      #making time series terms from dates
                                      month_clm = as.numeric(substr(clm_admsn_dt,6,7)),
                                      month_dsch = as.numeric(substr(nch_bene_dschrg_dt,6,7)),
                                      season_clm = as.factor(case_when(month_clm <= 3 ~ "Winter",
                                                                       month_clm >= 10 ~ "Fall",
                                                                       month_clm > 3 & month_clm < 7 ~ "Spring",
                                                                       TRUE ~ "Summer")),
                                      season_dsch = as.factor(case_when(month_dsch <= 3 ~ "Winter",
                                                                        month_dsch >= 10 ~ "Fall",
                                                                        month_dsch > 3 & month_dsch < 7 ~ "Spring",
                                                                        TRUE ~ "Summer")),
                                      time_term_clm = as.numeric(substr(clm_admsn_dt,9,10)) + 
                                        month_clm * 30 + 365 * (yr_adm - min(yr_adm)),
                                      time_term_dsch = as.numeric(substr(nch_bene_dschrg_dt,9,10)) + 
                                        month_dsch * 30 + 365 * (yr_disch - min(yr_disch))
)

#removing columns that are no longer relevent
#rethink the correlation between time_term_clm, time_term_dsch, season_clm, season_dsch
df_medicare <- df_medicare[, !(colnames(df_medicare) %in% c('clm_admsn_dt', 'nch_bene_dschrg_dt', 'month_clm', 'month_dsch'))]

#setting columns that look numeric that are truly categories as factors
df_medicare$clm_drg_cd <- df_medicare$clm_drg_cd %>% as.factor() #diagnosis type
df_medicare$at_physn_npi <- df_medicare$at_physn_npi %>% as.factor() #physician id
df_medicare$op_physn_npi <- df_medicare$op_physn_npi %>% as.factor() #physician id
df_medicare$ot_physn_npi <- df_medicare$ot_physn_npi %>% as.factor()#physician id
df_medicare$yr_adm <- df_medicare$yr_adm %>% as.factor() #year admission
df_medicare$yr_disch <- df_medicare$yr_disch %>% as.factor() #year discharge
df_medicare$fy <- df_medicare$fy %>% as.factor() #fiscal year
df_medicare$gndr_cd <- df_medicare$gndr_cd %>% as.factor() #gender
df_medicare$bene_race_cd <- df_medicare$bene_race_cd %>% as.factor() #race

#removing columns with any NA
df_medicare <- df_medicare[,df_medicare %>% is.na() %>% colSums() == 0]

#list of providers
providers <- unique(df_medicare$prvdr_num)


#label variables
label_variables <- c('mortality_90', 'readm_flag', 'er_90days')

#what the model will save each iteration
model_scores <- c('xgb_LogLoss','xgb_AUC','xgb_acc',
                  'rf_LogLoss','rf_AUC','rf_acc',
                  'logistic_LogLoss','logistic_AUC','logistic_acc',
                  'reglogistic_LogLoss','reglogistic_AUC','reglogistic_acc',
                  'xgb_eta','xgb_max_depth','xgb_colsample_bytree','xgb_subsample','xgb_nrounds',
                  'rf_mtry','rf_splitrule',
                  'reglogistic_loss', 'reglogistic_epsilon',
                  'reglogistic_cost')

#columns for X
columns_to_keep <- df_medicare %>% colnames()

columns_to_keep <- columns_to_keep[!(columns_to_keep %in% label_variables)]

columns_to_keep <- columns_to_keep[!(columns_to_keep == "prvdr_num")]

#making empty data frame to store results
results <- data.frame( matrix(,1, length(columns_to_keep) +  length(model_scores) + 2) )

#column names of results - will include prvrdr_num, indicators for included columns for each variable, accuracy scores, paramater tuning


colnames(results) <- c('prvdr_num', 'num_patients', columns_to_keep, model_scores)


#writing csv of results to save to, will update each iteration
write.csv(results, 'results_mortality_90.csv', row.names = F)
write.csv(results, 'results_er_90days.csv', row.names = F)
write.csv(results, 'results_readm_flag.csv', row.names = F)

#for loop for auto ML
iter = 1

#to measure time taken
time_start = timeDate::Sys.timeDate()

for (prov in providers[1:116]){
  
  #to measure time taken per iteration
  time_start_iteration = timeDate::Sys.timeDate()
  
  
  print(paste0('Provider: ', iter, '/',length(providers)), quote = F)
  
  iter = iter + 1
  
  #selecting hospital of interest
  df_medicare_hospital <- df_medicare %>% filter(prvdr_num == prov)
  
  df_medicare_hospital <- droplevels(df_medicare_hospital)
  
  #calculating variance in hospital similar to above
  df_variance <- df_medicare_hospital %>% summarize_all(funs(var), na.rm = T) 
  columns_to_keep_2 <- df_variance %>% summarize_all(funs(sum), na.rm = T) != 0
  
  #getting column names of columns with non zero variance
  columns_to_keep_2 = colnames(columns_to_keep_2)[columns_to_keep_2]
  
  
  #iterating through labels
  for(lab in label_variables){
    
    
    
    
    print(paste0("Fitting to ", lab), quote = F)
    
    
    #X value 
    X <- df_medicare_hospital[,columns_to_keep_2]
    
    #removing labels
    X <- X[,!(names(X) %in% label_variables)]
    
    y <- df_medicare_hospital %>% pull(lab)
    
    #univariate analysis
    set.seed(1)
    
    #predicting
    nfolds = 5 # 80/20 train/test split
    
    #separating training and test set
    #balanced classes
    fold <- kfold(y, k = nfolds, by = y)
    
    X_train_all_features <- X[fold != nfolds,]
    y_train <- y[fold != nfolds]
    
    #using boruta algorithm to get relevant features
    sigtest <- Boruta(y_train~. , data = cbind(X_train_all_features,
                                         y_train))
    
    features <- getSelectedAttributes(sigtest)
    
    
    
    
    #if at least one column passed the multivariate tests:
    if(length(features) > 1){
      
      #selecting only relevant features
      X = X[,features]
      
      #emptying results
      results <- data.frame( matrix(c(rep(0, length(columns_to_keep) +  2), rep(NA, length(model_scores))),1, length(columns_to_keep) +  length(model_scores) + 2) )
      
      #setting column names
      colnames(results) <- c('prvdr_num', 'num_patients', columns_to_keep, model_scores)
      
      results['prvdr_num'] = prov
      #saving number of patients
      results['num_patients'] = nrow(X)
      
      #marking features that are used with 1 indicator
      for(feat in features){
        results[feat] = 1
      }
      
      
      
      
      

      #resaving X_train and saving X_validate and y_validate
      X_train <- X[fold != nfolds,]
      # actual test set for final scoring
      X_validate <- X[fold == nfolds,]
      
      y_validate <- y[fold == nfolds]


      #combining X and y      
      combined_train <- cbind(X_train, y_train)
      
      
      
      # -------------------------GLM model-------------------------
      logistic <- glm(formula = y_train ~ ., data = combined_train, family = 'binomial')
      
      #fixing error where missing levels are present in train, but not in test
      for(cat_name in logistic$xlevels %>% names()){
        logistic$xlevels[cat_name] <- levels(X_train)
      }
      
      #predicting
      logistic_y_pred <- predict.glm(logistic, type = 'response', newdata = X_validate)
      logistic_y_true <- y_validate
      
      #saving results
      results['logistic_LogLoss'] = LogLoss(logistic_y_pred, logistic_y_true)
      results['logistic_AUC'] = AUC(logistic_y_pred, logistic_y_true)
      results['logistic_acc'] = Accuracy(round(logistic_y_pred), logistic_y_true)
      
      
      
      #reformating y to work with parameter search functions
      combined_train$y_train <- as.factor(combined_train$y_train)
      levels(combined_train$y_train) <- c("Class_0", "Class_1")
      
      # create the controller which will be used to train different model types using caret
      # cross validation
      fitControl <- trainControl(method = "cv",
                                 number = 5,
                                 classProbs = TRUE,
                                 summaryFunction = twoClassSummary)
      
      # -------------------------reglogistic model-------------------------
      regLogisticFit <- train(y_train ~ .,
                          data = combined_train,
                          method = "regLogistic",
                          trControl = fitControl,
                          metric="ROC")
      
      
       
      # optimal random forest (ranger) params
      results['reglogistic_epsilon'] <- regLogisticFit$finalModel$tuneValue$epsilon
      results['reglogistic_loss'] <- regLogisticFit$finalModel$tuneValue$loss
      results['reglogistic_cost'] <- regLogisticFit$finalModel$tuneValue$cost
       
      # predict on validation data
      reglogistic_prediction <- predict(regLogisticFit,
                                        newdata = X_validate, type = "prob")
      reglogistic_y_pred = reglogistic_prediction$Class_1
      reglogistic_y_true = y_validate
      
      # get results
      results['reglogistic_LogLoss'] = LogLoss(reglogistic_y_pred,
                                               reglogistic_y_true)
      results['reglogistic_AUC'] = AUC(reglogistic_y_pred, reglogistic_y_true)
      results['reglogistic_acc'] = Accuracy(round(reglogistic_y_pred),
                                      reglogistic_y_true)
      
      
      # -------------------------XGBoost model-------------------------
      

      xgbTreeFit <- train(y_train ~ .,
                          data = combined_train,
                          method = "xgbTree",
                          trControl = fitControl,
                          metric="ROC")
      
      # optimal xgboost params
      results['xgb_eta'] <- xgbTreeFit$finalModel$tuneValue$eta
      results['xgb_max_depth'] <- xgbTreeFit$finalModel$tuneValue$max_depth
      results['xgb_colsample_bytree'] <- xgbTreeFit$finalModel$tuneValue$colsample_bytree
      results['xgb_subsample'] <- xgbTreeFit$finalModel$tuneValue$subsample
      results['xgb_nrounds'] <- xgbTreeFit$finalModel$tuneValue$nrounds
      
      # predict on validation data
      xgb_prediction <- predict(xgbTreeFit, newdata = X_validate, type = "prob")
      xgb_y_pred = xgb_prediction$Class_1
      xgb_y_true = y_validate
      
      # get results
      results['xgb_LogLoss'] = LogLoss(xgb_y_pred, xgb_y_true)
      results['xgb_AUC'] = AUC(xgb_y_pred, xgb_y_true)
      results['xgb_acc'] = Accuracy(round(xgb_y_pred), xgb_y_true)


      # -------------------------Random Forest (ranger) model-------------------------
      

      rfFit<- train(y_train ~ .,
                     data = combined_train,
                     method = "ranger",
                     trControl = fitControl,
                     metric="ROC")
      
      # optimal random forest (ranger) params
      results['rf_mtry'] <- rfFit$finalModel$tuneValue$mtry
      results['rf_splitrule'] <- rfFit$finalModel$tuneValue$splitrule

      # predict on validation data
      rf_prediction <- predict(rfFit, newdata = X_validate, type = "prob")
      rf_y_pred = rf_prediction$Class_1
      rf_y_true = y_validate
      
      # get results
      results['rf_LogLoss'] = LogLoss(rf_y_pred, rf_y_true)
      results['rf_AUC'] = AUC(rf_y_pred, rf_y_true)
      results['rf_acc'] = Accuracy(round(rf_y_pred), rf_y_true)
      
      
      
    }
  
    #saving results to row in csv
    write.table(results, paste0('results_', lab,'.csv'), append = T, row.names = F, sep=',',col.names = F)
    
    
    
    
  }
  
  #displaying time statistics
  time_end_iteration = timeDate::Sys.timeDate()
  print('',quote = F)
  print('Time Iteration:',quote = F)
  print(time_end_iteration - time_start_iteration)
  
  print('',quote = F)
  print('Time Total:',quote = F)
  print(time_end_iteration - time_start)
}

#final time taken
print(time_end_iteration - time_start)
