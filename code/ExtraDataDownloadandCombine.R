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
library(fastDummies)
library(dplyr)


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


download.file('https://data.medicare.gov/api/views/ynj2-r877/rows.csv?accessType=DOWNLOAD&sorting=true', 'complications_and_deaths.csv')
df_complications <- read_csv('complications_and_deaths.csv')

download.file('https://data.medicare.gov/api/views/9n3s-kdb3/rows.csv?accessType=DOWNLOAD&sorting=true', 'readmissions.csv')
df_hospital_readmissions <- read_csv('readmissions.csv')

download.file('https://data.medicare.gov/api/views/dgck-syfz/rows.csv?accessType=DOWNLOAD', 'HCAHPS.csv')
df_HCAHPS <- read_csv('HCAHPS.csv')

#not enough data
#download.file('https://data.medicare.gov/api/views/tqkv-mgxq/rows.csv?accessType=DOWNLOAD&sorting=true', 'comprehensive_joint.csv')
#df_comprehensive_joint <- read_csv('comprehensive_joint.csv')

#no provider id / not enough data
#df_Trauma <- fromJSON('https://opendata.arcgis.com/datasets/c56882e5caaa4ebd8e90a29adfaf24d0_1.geojson')


#selecting only relevant variables
df_medicare[df_medicare == -999] = NA


selected_variables_medicare_hospital <- c('prvdr_num', 'index_los', 'age', 'prov_vol_annual', 'prov_vol_per_month', 
                        'prvdr_home_hha_vol_month', 'prvdr_home_hha_vol_per', 'prvdr_home_hha_vol',  'prvdr_cola',
                        'prvdr_resident_to_bed_ratio', 'prvdr_rday', 'prvdr_beds', 'prvdr_dshpct', 'prvdr_dshopg', 
                        'prvdr_dshcpg', 'clm_pps_cptl_drg_wt_num', 'op_phy_cum_exp', 'prior_inp_only_los',
                        'prior_irf_los', 'prior_ltcf_los', 'prior_snf_los', 'prior_hha_visits', 'prior_out_visits',
                        'gndr_cd', 'bene_race_cd', 'provider_type', 'prvdr_wi', 
                        'prvdr_urgeo', 'prvdr_teaching_status',
                        'prvdr_region', 'clm_utlztn_day_cnt', 'disc_status', 
                        'adm_source', 'clm_drg_cd', 'prncpal_dgns_cd',
                        'cci_score_1825_days_b', 'elix_score_1825_days_b', 'fci_score_1825_days_b',
                        'grp_1','grp_2','grp_3','grp_4','grp_5','grp_6','grp_7','grp_8','grp_9','grp_10',
                        'unemp_percentage','no_of_snfs','ma_pen_percent','snf_per_capita')


df_medicare_hospital <- df_medicare[,selected_variables_medicare_hospital]

selected_variables_medicare_patient <- c("prvdr_num", "readm_flag", "mortality_90",
                                         "er_90days")

df_medicare_patient <- df_medicare[,selected_variables_medicare_patient]

#categoricial bedsize

#https://www.icd10data.com/ICD10CM/Codes

df_medicare_hospital <- df_medicare_hospital %>% mutate(diagnosis_type = case_when(substr(prncpal_dgns_cd,1,1) == "S" ~ "S",
                                                                 substr(prncpal_dgns_cd,1,1) == "M" ~ "M",
                                                                 TRUE ~ "Other"),
                                      age_interval = cut(age, breaks = c(0,65,70,75,80,85,90,95,100)),
                                      num_beds_cat = cut(prvdr_beds, 5))

df_medicare_hospital <- df_medicare_hospital[,-which(names(df_medicare_hospital) %in% c('prncpal_dgns_cd', 'age', 'prvdr_beds'))]

categorical_variables <- c('gndr_cd', 'bene_race_cd', 'provider_type', 'diagnosis_type',
                           'prvdr_urgeo', 'prvdr_div_code', 'prvdr_teaching_status',
                           'prvdr_region', 'num_beds_cat',
                           'disc_status', 'adm_source', 'age_interval',
                           'clm_drg_cd')

#One-hot Encoding
df_medicare_hospital <- dummy_cols(df_medicare_hospital,select_columns = categorical_variables)

#remove original categorical variable
df_medicare_hospital <- df_medicare_hospital[, !names(df_medicare_hospital) %in% categorical_variables]

#get column mean by 'prvdr_num(hospital ID)'
df_medicare_hospital <- df_medicare_hospital %>% group_by(prvdr_num) %>% summarise_all(funs(mean), na.rm = TRUE)

#hospital X, patient y join by 'prvdr_num'
df_medicare <- inner_join(df_medicare_patient, df_medicare_hospital)

#cleaning df_complications
selected_variables_complications <- c("Facility ID", "Measure ID", "Compared to National", "Denominator",
                                      "Score", "Lower Estimate", "Higher Estimate")

df_complications <- df_complications[,selected_variables_complications]

#Stole this from stack overflow
# https://community.rstudio.com/t/spread-with-multiple-value-columns/5378
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  
  # break value vector into quotes
  
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

#setting NA
df_complications[df_complications == "Not Available"] = NA

#changing values to numeric
df_complications$Score <- as.numeric(df_complications$Score)
df_complications$Denominator <- as.numeric(df_complications$Denominator)
df_complications$`Lower Estimate` <- as.numeric(df_complications$`Lower Estimate`)
df_complications$`Higher Estimate` <- as.numeric(df_complications$`Higher Estimate`)

#spreading correct values
# `Measure ID` = key, c("Compared to National", "Denominator", "Score", "Lower Estimate", "Higher Estimate") = value
# create new columns: Measure ID_Compared to National,Measure ID_Denominator,....
df_complications <- df_complications %>% myspread(`Measure ID`, c("Compared to National", "Denominator", "Score", "Lower Estimate", "Higher Estimate"))

#setting values to numeric that are set as character by spread command
for(i in seq(2,96,5)){
  df_complications[,i+1] <- lapply(df_complications[,i+1], as.numeric)
  df_complications[,i+2] <- lapply(df_complications[,i+2], as.numeric)
  df_complications[,i+3] <- lapply(df_complications[,i+3], as.numeric)
  df_complications[,i+4] <- lapply(df_complications[,i+4], as.numeric)
}


#cleaning df_hospital_readmissions

df_hospital_readmissions <- df_hospital_readmissions %>% filter(`Measure Name` == "READM_30_HIP_KNEE_HRRP")
df_hospital_readmissions <- df_hospital_readmissions[,c('Facility ID', 'Number of Discharges', 'Excess Readmission Ratio', 'Predicted Readmission Rate',
                                                        'Expected Readmission Rate', 'Number of Readmissions')]  


df_hospital_readmissions[df_hospital_readmissions == "Not Available"] = NA

#making the columns numeric
df_hospital_readmissions$`Number of Discharges` <- as.numeric(df_hospital_readmissions$`Number of Discharges`)
df_hospital_readmissions$`Excess Readmission Ratio` <- as.numeric(df_hospital_readmissions$`Excess Readmission Ratio`)
df_hospital_readmissions$`Predicted Readmission Rate` <- as.numeric(df_hospital_readmissions$`Predicted Readmission Rate`)
df_hospital_readmissions$`Expected Readmission Rate` <- as.numeric(df_hospital_readmissions$`Expected Readmission Rate`)
df_hospital_readmissions$`Number of Readmissions` <- as.numeric(df_hospital_readmissions$`Number of Readmissions`)

#cleaning HCAHPS

df_HCAHPS <- df_HCAHPS[,c('Facility ID', 'Survey Response Rate Percent',
                       'HCAHPS Question', 'Patient Survey Star Rating',
                       'HCAHPS Answer Percent', 'HCAHPS Linear Mean Value')]

df_HCAHPS[df_HCAHPS == "Not Available"] = NA

#having one numeric score accross all columns
df_HCAHPS <- df_HCAHPS %>% mutate(numeric_score = ifelse(`Patient Survey Star Rating` == "Not Applicable",
                                                         0, as.numeric(`Patient Survey Star Rating`)) + 
                                    
                                    ifelse(`HCAHPS Answer Percent` == "Not Applicable",
                                           0, as.numeric(`HCAHPS Answer Percent`)) +
                                    
                                    ifelse(`HCAHPS Linear Mean Value` == "Not Applicable",
                                           0, as.numeric(`HCAHPS Linear Mean Value`)))

df_HCAHPS <- df_HCAHPS[,c('Facility ID', 'Survey Response Rate Percent', 'HCAHPS Question', 'numeric_score')]

#spreading question and one numeric scores
df_HCAHPS <- df_HCAHPS %>% spread(`HCAHPS Question`, numeric_score)

#joining data
df_total_data <- left_join(df_medicare, df_complications, by = c("prvdr_num" = "Facility ID"))
df_total_data <- left_join(df_total_data, df_hospital_readmissions, by = c("prvdr_num" = "Facility ID"))
df_total_data <- left_join(df_total_data, df_HCAHPS, by = c("prvdr_num" = "Facility ID"))

# Need code to calculate missing percentage
# LIke how many observations have over ___ percent missing variables


write.csv(df_total_data, "/mnt/exports/shared/home/tbliss/ML/df_total_data_1.csv", row.names = F)

