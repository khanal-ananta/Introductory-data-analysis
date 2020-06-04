#----------------------------------------------------------------------------------------------------------------
#-----------------------Confidence,Prognosis,Tolerence interval-------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

remove(list = ls())

# (1 ): ---------------------------------------------------------------------------------------------------------

# given data is

data <- c(905, 827, 2100, 1647, 978, 841, 2024, 1597, 1638, 1214, 845, 1925, 1376, 1351, 1417, 1365, 1475)
n <- 17


# (a): ---------------------------------------------------------------------------------------------------------
# find the confidence interval

# finding mean
mean_data <- mean(data)
mean_data   # 1383.824

#finding stardard deviation
sd_data <- sd(data)
sd_data  # 412.5887

# finding stardard error
stand_error <- sd_data/sqrt(n)
stand_error  # 100.0674

# finding t value at 95 % of confidence
t_value <- qt(0.05 / 2, df = 16, lower.tail = FALSE)  # for two  tailed test
t_value  # 2.119905

# finding margin of error
margin_error <- t_value * stand_error
margin_error   # 212.1335

# so confidence interval can be calculated as
# confidence interval = mean +- margin of error
confidence_interval_lower <- mean_data - margin_error
confidence_interval_lower  # 1171.69

confidence_interval_upper <- mean_data + margin_error
confidence_interval_upper   # 1595.957

# so confidence interval is 1171.69 to 1595.957
# we are 95 % sure that the mean value is somewhere between 1171.69 to 1595.957


# (b): ---------------------------------------------------------------------------------------------------------
# find the prognosis interval
# It is the area in which a new value will be

# standard error for 95 % confidence interval is calculate as
stand_error2 <-  sd_data *sqrt(1+(1 / n))
stand_error2  # 424.5502

# finding t(n-1 value)
t_value2 <- qt(0.05 / 2, df=16, lower.tail = FALSE)
t_value2   # 2.119905   

# finding margin of error
margin_error2 <- t_value2 * stand_error2
margin_error2  # 900.0063

# so prognosis interval can be calculated as
# prognosis interval = mean +- margin of error
prognosis_interval_lower <- mean_data - margin_error2
prognosis_interval_lower  # 483.8172

prognosis_interval_upper <- mean_data + margin_error2
prognosis_interval_upper   # 2283.83

# so prognosis interval is 483.8172 to 2283.83



# (c): ---------------------------------------------------------------------------------------------------------
# find the tolerence interval
# A tolerance interval is that interval, in which for a given confidence level, a certain percentage(specific
# proportion) of new values will lie.

# from the table given in the question for the confidence of 95 % we get
lamda_p1 <- 2.44   # this if for p = .90
lamda_p2 <- 2.90   # # this if for p = .95

# for p = .90 
tolerence_interval_lower1 <- mean_data - lamda_p1 * sd_data
tolerence_interval_lower1  # 377.1072

tolerence_interval_upper1 <- mean_data + lamda_p1 * sd_data
tolerence_interval_upper1  # 2390.54

# so tolerence interval is 377.1072 to 2390.54 for p = .90


# for p = .95
tolerence_interval_lower2 <- mean_data - lamda_p2 * sd_data
tolerence_interval_lower2  # 187.3164

tolerence_interval_upper2 <- mean_data + lamda_p2 * sd_data
tolerence_interval_upper2  # 2580.331

# so tolerence interval is 187.3164 to 2580.331 for p = .95






