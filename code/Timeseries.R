library(dplyr)
library(ggplot2)

# get the admission date and er_90days info
data_time_ER90 = data[, c("clm_admsn_dt", "er_90days")]
data_time_ER90$clm_admsn_dt = as.Date(data_time_ER90$clm_admsn_dt)
data_time_ER90_summary = group_by(data_time_ER90, clm_admsn_dt)
# group the data by date and get the total number and rate for Patients with ER Record Within the 90 Days Follow Up
data_time_ER90_summary = summarize(data_time_ER90_summary, rate = mean(er_90days), sum = sum(er_90days))
# get rid of last three entries with only a few data points
row_end = nrow(data_time_ER90_summary) - 3
data_time_ER90_summary = data_time_ER90_summary[c(1:row_end), ]
# plot the time series graph
ggplot(data = data_time_ER90_summary, aes(x = clm_admsn_dt, y = sum))+
  geom_line(color = "blue")+
  ggtitle("Number of Patients with ER Record Within the 90 Days Follow Up")+
  labs(x = "Date", y ="Number of patients")
ggplot(data = data_time_ER90_summary, aes(x = clm_admsn_dt, y = rate))+
  geom_line(color = "black")+
  ggtitle("Rate of Emergency Room Visit")+
  labs(x = "Date", y ="Rate of Emergency Room Visit")
# plot acf and pacf graph
acf_sum = acf(data_time_ER90_summary$sum)
plot(acf_sum, main = "Number of Patients with ER_90 Record")
pacf_sum = pacf(data_time_ER90_summary$sum)
plot(pacf_sum, main = "Number of Patients with ER_90 Record")
acf_rate = acf(data_time_ER90_summary$rate)
plot(acf_rate, main = "Rate of Emergency Room Visit")
pacf_rate = pacf(data_time_ER90_summary$rate)
plot(pacf_rate, main = "Rate of Emergency Room Visit")

#re_admit

data_time_readmit90 = data[, c("clm_admsn_dt", "readm_flag")]
data_time_readmit90$clm_admsn_dt = as.Date(data_time_readmit90$clm_admsn_dt)
data_time_readmit90_summary = group_by(data_time_readmit90, clm_admsn_dt)
# group the data by date and get the total number and rate for Patients with ER Record Within the 90 Days Follow Up
data_time_readmit90_summary = summarize(data_time_readmit90_summary, rate = mean(readm_flag), sum = sum(readm_flag))

# get rid of last three entries with only a few data points
row_end = nrow(data_time_readmit90_summary) - 3
data_time_readmit90_summary = data_time_readmit90_summary[c(1:row_end), ]

# plot the time series graph
ggplot(data = data_time_readmit90_summary, aes(x = clm_admsn_dt, y = sum))+
  geom_line(color = "blue")+
  ggtitle("Number of Patients with readmit90 Record")+
  labs(x = "Date", y ="Number of patients")
ggplot(data = data_time_readmit90_summary, aes(x = clm_admsn_dt, y = rate))+
  geom_line(color = "black")+
  ggtitle("Rate of Readmission")+
  labs(x = "Date", y ="Rate of Readmission")
# plot acf and pacf graph
acf_sum = acf(data_time_readmit90_summary$sum)
plot(acf_sum, main = "Number of Patients with readmit_90 Record")
pacf_sum = pacf(data_time_readmit90_summary$sum)
plot(pacf_sum, main = "Number of Patients with readmit_90 Record")
acf_rate = acf(data_time_readmit90_summary$rate)
plot(acf_rate, main = "Rate of Readmission")
pacf_rate = pacf(data_time_readmit90_summary$rate)
plot(pacf_rate, main = "Rate of Readmission")


#mortality

data_time_mortality_90 = data[, c("clm_admsn_dt", "mortality_90")]
data_time_mortality_90$survive = (data_time_mortality_90$mortality_90 - 1)* -1
data_time_mortality_90$clm_admsn_dt = as.Date(data_time_mortality_90$clm_admsn_dt)
data_time_mortality_90_summary = group_by(data_time_mortality_90, clm_admsn_dt)
# group the data by date and get the total number and rate for Patients with ER Record Within the 90 Days Follow Up
data_time_mortality_90_summary = summarize(data_time_mortality_90_summary, 
                                           su_rate = mean(survive), su_sum = sum(survive),
                                           rate = mean(mortality_90), sum = sum(mortality_90))
# get rid of last three entries with only a few data points
row_end = nrow(data_time_mortality_90_summary) - 3
data_time_mortality_90_summary = data_time_mortality_90_summary[c(1:row_end), ]

# plot the time series graph
ggplot(data = data_time_mortality_90_summary, aes(x = clm_admsn_dt, y = sum))+
  geom_line(color = "blue")+
  ggtitle("Number of Patients Die within 90 days")+
  labs(x = "Date", y ="Number of patients")
ggplot(data = data_time_mortality_90_summary, aes(x = clm_admsn_dt, y = rate))+
  geom_line(color = "black")+
  ggtitle("Rate of Mortality")+
  labs(x = "Date", y ="Rate of Mortality")
# plot acf and pacf graph
acf_sum = acf(data_time_mortality_90_summary$sum)
plot(acf_sum, main = "Number of Patients Die within 90 days")
pacf_sum = pacf(data_time_mortality_90_summary$sum)
plot(pacf_sum, main = "Number of Patients Die within 90 days")
acf_rate = acf(data_time_mortality_90_summary$rate)
plot(acf_rate, main = "Rate of Mortality")
pacf_rate = pacf(data_time_mortality_90_summary$rate)
plot(pacf_rate, main = "Rate of Mortality")

# number of patients

data_time = select(data, c("clm_admsn_dt"))
data_time$patients = 1
data_time$clm_admsn_dt = as.Date(data_time$clm_admsn_dt)
data_time_summary = group_by(data_time, clm_admsn_dt)
# group the data by date and get the total number and rate for Patients with ER Record Within the 90 Days Follow Up
data_time_summary = summarize(data_time_summary, sum = sum(patients))
# get rid of last three entries with only a few data points
row_end = nrow(data_time_summary) - 3
data_time_summary = data_time_summary[c(1:row_end), ]

# plot the time series graph
ggplot(data = data_time_summary, aes(x = clm_admsn_dt, y = sum))+
  geom_line(color = "blue")+
  ggtitle("Number of Patients")+
  labs(x = "Date", y ="Number of patients")

# plot acf and pacf graph
acf_sum = acf(data_time_summary$sum)
plot(acf_sum, main = "Number of Patients")
pacf_sum = pacf(data_time_summary$sum)
plot(pacf_sum, main = "Number of Patients")


# plot_timeseries <- function(data, target){
#   #get the new data frame with only date of admission and target variable
#   data_target = select(data, clm_admsn_dt, target)
#   data_target = rename(data_target, target1 = target)
#   data_target$clm_admsn_dt = as.Date(data_target$clm_admsn_dt)
#   #summarize the data to obtain total number of target and rate of a given date
#   data_time_target_summary = group_by(data_target, clm_admsn_dt)
#   data_time_target_summary = summarize(data_time_target_summary, rate = mean(target1), 
#                                        sum = sum(target1))
#   # data_time_target_summary = data_target %>%
#   #   group_by(clm_admsn_dt) %>%
#   #   summarize(rate = mean(data_target_summary[,2]), sum = sum(data_target_summary[,2]))
#   title1 = paste("Number of Patients with Positive ", target, " within the 90 Days Follow Up", sep ="")
#   #plot time series data relate to sum
#   print(colnames(data_time_target_summary))
#   p = ggplot(data = data_time_target_summary, aes(x = clm_admsn_dt, y = sum))+
#     geom_line(color = "blue")+
#     ggtitle(title1)+
#     labs(x = "Date", y ="Number of patients")
#   plot(p)
#   acfplot = acf(data_time_target_summary$sum)
#   title_acf = paste("Total Number of ", target, "Time Series ACF", sep =" ")
#   plot(acfplot, main = title_acf)
#   # title_pacf = paste("Total Number of ", target, "Time Series PACF", sep =" ")
#   # pacfplot = pacf(data_time_target_summary$sum)
#   # plot(pacfplot, main = title_pacf)
# 
#   #plot time series data relate to rate
#   title2 = paste("Rate of Patients with Positive ", target, " within the 90 Days Follow Up", sep ="")
#   ggplot(data = data_time_ER90_summary, aes(x = clm_admsn_dt, y = rate))+
#     geom_line(color = "black")+
#     ggtitle(title2)+
#     labs(x = "Date", y ="Number of patients")
#   acfplot2 = acf(data_time_target_summary$rate)
#   title_acf2 = paste("Rate of ", target, "Time Series ACF", sep =" ")
#   plot(acfplot2, main = title_acf2)
#   # title_pacf2 = paste("Rate of ", target, "Time Series PACF", sep =" ")
#   # pacfplot2 = pacf(data_time_target_summary$rate)
#   # plot(pacfplot2, main = title_pacf2)
# }
# plot_timeseries(data, "er_90days")
