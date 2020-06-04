#----------------------------------------------------------------------------------------------------------------
#-----------------------F-test---------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

remove(list = ls())

#################################################################################################################

# test statistics F = S1^2/S2^2 

################################################################################################################

# (1 ): ---------------------------------------------------------------------------------------------------------
# F-Test for comparing variances of two samples: Do the F-Test to compare the variances of the two samples 
# in assignment 4.

# note F-test is used to compare varience between two samples
# given data in the question in assignment 4
probe1<- c(201, 138, 132, 117, 177, 168, 178, 104)
probe1

probe2<- c(127, 60, 79, 63, 105, 82, 57, 72)
probe2

common_mean <-  151.875 
common_sd <- 29.52088

# (a): ---------------------------------------------------------------------------------------------------------
# Calculate the ratio s1^2 /s2^2 , then use fvert() to find the p-value. 


# finding the varience of the two values
var_1 <- var(probe1)
var_1     # 1154.696
var_2 <- var(probe2)
var_2    # 588.2679

# ratio of varience whihc is actually the calculated F-value
var_ratio <- var_1 / var_2
var_ratio   # 1.962875

F_calc <- var_ratio
F_calc    # 1.962875

# lower.tail = FALSE is used for right hand test
F_cric <- qf(0.05, df1 = 7, df2 = 7, lower.tail = FALSE)
F_cric   # 3.787044

# since f_cric > F_calc in right tail test we accept the null hypothesis

# calculating p value
# note P value is always the probability density distribution of corresponding q value 
P_value <- pf(F_calc, df1 = 7, df2 = 7, lower.tail = FALSE)
P_value  #  0.1967443  



# (b): ---------------------------------------------------------------------------------------------------------
# for α = 5%, 1%, 0,5% find the critical F-values.

#for alpha = 5%
alpha <- 0.05
F1_left <- qf(0.05, df1 = 7, df2 = 7, lower.tail = TRUE) # TRUE for left tail test
F1_left # 0.2640582

F1_right <- qf(0.05, df1 = 7, df2 = 7, lower.tail = FALSE) # FALSE for right tail test
F1_right  # 3.787044

F1_TWO <- qf(0.05/2, df1 = 7, df2 = 7, lower.tail = FALSE) # FALSE for two tail test and alpha divide by 2
F1_TWO  # 4.994909


#for alpha = 1%
alpha <- 0.01
F2_left <- qf(0.01, df1 = 7, df2 = 7, lower.tail = TRUE) # TRUE for left tail test
F2_left # 0.1430036

F2_right <- qf(0.01, df1 = 7, df2 = 7, lower.tail = FALSE) # FALSE for right tail test
F2_right  # 6.992833

F2_TWO <- qf(0.01/2, df1 = 7, df2 = 7, lower.tail = FALSE) # FALSE for two tail test and alpha divide by 2
F2_TWO  # 8.885389



#for alpha = 0.005%
alpha <- 0.005
F2_left <- qf(0.005, df1 = 7, df2 = 7, lower.tail = TRUE) # TRUE for left tail test
F2_left # 0.1125443

F2_right <- qf(0.005, df1 = 7, df2 = 7, lower.tail = FALSE) # FALSE for right tail test
F2_right  # 8.885389

F2_TWO <- qf(0.005/2, df1 = 7, df2 = 7, lower.tail = FALSE) # FALSE for two tail test and alpha divide by 2
F2_TWO  # 11.18808


# (2): ---------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
# F-Test for comparing inter-sample variance with intra-sample variance - again use the data from assignment 4,
# calculate the following sums of squares (SS): SS_intra = Σ i=1,16 (x_i – x _sample-mean )^2
# , where x sample-mean is the mean of the sample, x_i is in.
# SS_inter = 8 times (x _1st-sample-mean – x_grand-mean )^2 + 8 times (x_2nd-sample-mean – x_grand-mean )^2

# first sample mean from both samples
sample_mean1 <- mean(probe1)
sample_mean1  # 151.875

sample_mean2 <- mean(probe2)
sample_mean2  # 80.625


# finding square of difference of sample 1 that is probe1
# defining a vector for storing square of the difference of the data and the mean i.e square
S_intra1 <- vector(mode = "logical", length = length(probe1))
for (i in 1:length(probe1))
  {
  S_intra1[i] <-(probe1[i] - sample_mean1)^2
  }
S_intra1  # 2413.2656  192.5156  395.0156 1216.2656  631.2656  260.0156  682.5156 2292.0156

# finding square of difference of sample 2 that is probe2
# defining a vector for storing square of the difference of the data and the mean i.e square
S_intra2 <- vector(mode = "logical", length = length(probe2))

for (i in 1:length(probe2))
{
  S_intra2[i] <-(probe2[i] - sample_mean2)^2
}
S_intra2  # 2150.640625  425.390625    2.640625  310.640625  594.140625    1.890625  558.140625   74.390625

# finding the sum of all the square of the difference calcualted above i.e square sum
SS_intra <- sum(c(S_intra1, S_intra2))
SS_intra  # 12200.75


# finding SS_inter(total data in one column)
total_data <- c(probe1, probe2)
total_data

# note that mean of the means is different from total grand mean
x_grand_mean <- mean(total_data)
x_grand_mean # 116.25 

SS_inter <- 8 * (sample_mean1 - x_grand_mean)^2 + 8 * (sample_mean2 - x_grand_mean)^2
SS_inter   # 20306.25

# (b): ---------------------------------------------------------------------------------------------------------
# Then calculate variance_intra = SS_intra / DF_intra , DF = degrees of freedom, DF_intra =14
# variance_inter = SS_inter / DF_inter , DF inter = 1.

DF_intra <- 16-1-1
variance_intra <- SS_intra / DF_intra
variance_intra  # 871.4821

DF_inter <- 2-1  # we are taking 2 samples
variance_inter <- SS_inter / DF_inter
variance_inter  # 20306.25


# (c): ---------------------------------------------------------------------------------------------------------
# Now do the F-Test (as in ex. 1).
 
# if we are putting the highest value in nominator then its right tail test and vice versa
F_calculated <- variance_inter / variance_intra
F_calculated # 23.30082

F_critical <- qf(0.05, df1 = 7, df2 = 7, lower.tail = FALSE) # right tail test
F_critical  # 3.787044

# as we know from assisment 4
# In a cleaning process the effect of changing the temperature of the water used is to be investigated. 
# The cleaning process is performed at 60 ◦ and at 80 ◦ , remaining impurities are measured: values are above
# At 0.05 significant level can we conclude that hot water always cleans better ?

# H0: cold water cleans better i.e  mean1 < = mean2
# H1: hot water cleans better i.e  mean1 > mean2

# since F_calculated > F_critical reject null hypothesis i.e accept alternate hypothesis i.e hot water cleans better

# (d): ---------------------------------------------------------------------------------------------------------
# Compare the critical-values with those of the t-Test in assignments 4 and 5.

# in assignment 4 T_critical is calculated as
t_cric = qt(0.05/2, df=14 , lower.tail = FALSE)  # two tail test
t_cric  # 2.144787

# and here we get F_critical as
F_critical <- qf(0.05, df1 = 1, df2 = 14, lower.tail = FALSE) # right tail test
F_critical  # 4.60011

# so we get the f critical values relatively higher
# it may be because of the square we do for calculating varience 
# so we are only getting positive values
# lets us check if the values matches

square_t <- t_cric^2
square_t  # 4.60011

# yes both are same


# comapring chi-square with F distribution
# 


