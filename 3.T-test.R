#----------------------------------------------------------------------------------------------------------------
#-----------------------T-test-----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

remove(list = ls())

###############################################################################################################
# standard deviation = sqrt(sum(x-maen(x))^2/(n-1))

# one-sample t-test is done wheather to test that sample resembles the total population or not.
# in one-sample t(calc) = (sample mean - population mean) / (sample sd/ sqrt(n))

# two sample t-test of independent means is done to see is there is significant difference between 2 samples by 
# comparing means
# common_variance = (sd1^2(n1-1)) +(sd2^2(n2-1))/(n1+n2-2) = (sd1^2+sd2^2)/2 (this is for n1=n2=n)
# t(calc) = mean_difference/(common_sd *(sqrt(2/n)))

###############################################################################################################

# In order to check, if two samples belong to the same population or process, we can use the t-distribution. 
# We shall demonstrate the t-Test with a simple example:
# The aim of the exercise is to check, if the two means of the two samples in the abovescreen-shot are to be 
# considered as significantly different. Follow the following steps:

# given data in the question
probe1<- c(201, 138, 132, 117, 177, 168, 178, 104)
probe1

probe2<- c(127, 60, 79, 63, 105, 82, 57, 72)
probe2

# (1): ---------------------------------------------------------------------------------------------------------
# calculate mean, variance and standard deviation for both samples.
# for first data
mean1 = mean(probe1)
sd1 = sd(probe1)
var1 = var(probe1)
mean1  # 151.875
sd1   # 33.98082
var1  # 1154.696

# for second data
mean2 = mean(probe2)
sd2 = sd(probe2)
var2 = var(probe2)
mean2  # 80.625
sd2    # 24.25423
var2   # 588.2679

# (2):------------------------------------------------------------------------------------------------------------
#  calculate the mean of the two means and their difference
#mean of the two mean
mean_of_means = mean(mean1, mean2)
mean_of_means   #  151.875 

#difference of mean
diff_of_means = mean1 - mean2
diff_of_means   # 71.25

#(3): ------------------------------------------------------------------------------------------------------------
#  calculate the common variance fort the two samples and take square root to get the common standard deviation,
# common variance
common_var = (sd1^2+sd2^2)/2
common_var  #871.4821

# common standard deviation
common_sd = sqrt(common_var)
common_sd #29.52088

#----------------------------------------------------------------------------------------------------------------
# defining null and alternate hypothesis
# null hypothesis is general data that we believe(given or assum data)
# alternate hypothesis is the data that we are going to test(claim or unknown data)

#(4):------------------------------------------------------------------------------------------------------------
# calculate the critical t-value using the EXCEL-funktion TINV (note that the parameter for TINV is not 
# confidence level, but alpha-level for both sided test,
# here we use right hand one tail test.
t_cric = qt(0.05, df=14 , lower.tail = FALSE)  
t_cric  # 1.76131


t_cric_2tail <- qt(0.05/2, df =14, lower.tail = FALSE)
t_cric_2tail    #  2.144787


#(5):---------------------------------------------------------------------------------------------------------
# do the significance test, either by calculating a μ crit or comparing the quotient of the difference of 
# means to s difference of means with t crit .

# so we need to calculate t-emperical or t-calculated 
# 1-way comparing critical t value and calculated t value
t_calc = diff_of_means/(common_sd * sqrt(1/8+1/8))
t_calc  # 4.827092

# 2-way comparing difference of mean with u-critical
u_cric = t_cric * common_sd * sqrt(1/8+1/8)
u_cric   # 25.99771

#(6): ---------------------------------------------------------------------------------------------------------
# Is the result significant?
#first way
final_test = t_calc - t_cric
final_test   #3.065782 
# here t caluclated is greater then t critical so reject null hypothesis
# since the value is posite we get that calculated t(t-calculated) value is greater then critical t(t-critical) value
# reject null hypothesis(H0) 


#second way
diff_of_means  # 71.25
u_cric         # 25.99771

#since the checking interval which is (- infinity, 25.99) and difference of means doesn't lie in that interval
# reject null hypothesis(H0)