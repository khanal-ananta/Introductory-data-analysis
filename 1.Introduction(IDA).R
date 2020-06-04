#----------------------------------------------------------------------------------------------------------------
#-----------------------Introduction-----------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

remove(list = ls())

# (1 ): --------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
# Use MS-Excel® or open office calc to simulate a “corrected process” as it is done in chap 1 of the script.
#  Use the function zufallszahl() (german XL) resp.rand( ) (engl. OO) to generate a column random numbers, 
#  p, between 0 and 1.

# runif() gives the random unifrom distribution
p <- runif(n=10,min=0,max=1) 
p  # 0.7293096 0.4525708 0.1751268 0.7466983 0.1049876 0.8645449 0.6146450 0.5571595 0.3287773 0.4531314
# matrix gives the above numbers in specified form
p1 <- matrix(p, ncol = 1, nrow = , dimnames = list(c(),c("p")))
p1   # shows in one column


# (b): ----------------------------------------------------------------------------------------------------------
# You may use the function standnorminv(p) resp. normsinv( ) to get a column of normally distributed random numbers.
# rnorm gives specified number of normally distributed random numbers
p1 <- (rnorm(10))
p1 #  0.001105352  0.074341324 -0.589520946 -0.568668733 -0.135178615  1.178086997 -1.523566800  0.593946188
   #  0.332950371  1.063099837
p_norm <- matrix(p1, ncol = 1, nrow = , dimnames = list(c(),c("p_norm")))
p_norm  # shows in one column

# (c): ----------------------------------------------------------------------------------------------------------
#  Construct a process that has an expectation value of 14 and an expected standard
# deviation of 1.5. Make a column for this too.
n <- rnorm(10, 14, 1.5)
n   # 13.54372 14.55503 14.40065 13.18622 15.81180 15.74060 15.05032 16.38025 14.83773 12.08511
n_process <- matrix(n, ncol = 1, nrow = , dimnames = list(c(),c("n_process")))
n_process  # display that in one column

# (d): ----------------------------------------------------------------------------------------------------------
# Calculate corrections for the process in (c). (new column).
correction <- n-14
correction # -0.4562759  0.5550282  0.4006482 -0.8137800  1.8118017  1.7406039  1.0503205  
           # 2.3802502  0.8377296 -1.9148883
correction_value <- matrix(correction, ncol = 1, nrow = , dimnames = list(c(),c("n_process_correct")))
correction_value  # shows into one column

# (e): ----------------------------------------------------------------------------------------------------------
# What does the corrected process look like? (new column).
n_process_correct <- vector(mode = "logical", length(correction_value))
# first row must be empty because it will not have any corrected process
# add correction value to the next value of the process
for (i in 2: length(n_process)){
  n_process_correct[i] <- n_process[i] + correction_value[i - 1] 
}
n_process_correct #  15.17238 12.71288 13.09972 12.93080 13.10134 14.32261 14.47529 12.97507 15.25562


# (f): ---------------------------------------------------------------------------------------------------------
#  Compare mean value and standard deviation of the original and the “corrected”.process. (add a line underneath 
# the data for the two columns)
old_mean <- mean(n_process)
old_mean # 14.08106
new_mean <- mean(n_process_correct[2 : 10])
new_mean   # 14.09998

old_sd <- sd(n_process)
old_sd # 1.50715
new_sd <- sd(n_process_correct[2 : 10])
new_sd  # 2.548525

# (f): ---------------------------------------------------------------------------------------------------------
#  What would you expect?

# We try to correct the process by first substracting mean from output and add the earlier mean difference
# to the next output 
# The corrected process will one output less than original but that does not matter.

# When we see the mean and standard deviation of the corrected process, even though the mean is nearly the same,
# the standard deviation has worsened. Which means induced correction has actually  worsened the process.


# (2 ): --------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
# Simulate a die using the functions zufallszahl()/rand( ) and abrunden(p;0)/round(p;0). Simulate a process 
# that yields the mean of two dice, another one that simulates the mean of 4 dice. (You should have a 
# column in your Excel sheet for each.) Compare the means and the standard deviations. Again: what would you expect.

# process for 2 dice
set.seed(1) 
diceThrow1 <- sample(1:6, 100, replace = TRUE) # throw dice 100 times
diceThrow1
diceThrow2 <- sample(1:6, 100, replace = TRUE) 
diceThrow2
dice_mean_all_2 <- (diceThrow1 + diceThrow2) / 2
dice_mean_all_2

first_mean <- mean(dice_mean_all_2)
first_mean  # 3.61

first_sd <- sd(dice_mean_all_2)
first_sd  # 1.133913

# process for 4 dice
set.seed(1) 
diceThrow3 <- sample(1:6, 100, replace = TRUE) # throw dice 100 times
diceThrow3
diceThrow4 <- sample(1:6, 100, replace = TRUE) 
diceThrow4
diceThrow5 <- sample(1:6, 100, replace = TRUE) # throw dice 100 times
diceThrow5
diceThrow6 <- sample(1:6, 100, replace = TRUE) 
diceThrow6
dice_mean_all_4 <- (diceThrow3 + diceThrow4 + diceThrow5 + diceThrow6) / 4
dice_mean_all_4

second_mean <- mean(dice_mean_all_4)
second_mean  # 3.445

second_sd <- sd(dice_mean_all_4)
second_sd  # 0.8605994



# (b): ---------------------------------------------------------------------------------------------------------
#  Generate at least 100 data (per column) in the way described in (a). Construct frequency tables and 
#  then a frequency diagram.

set.seed(1) # always create the same sample output
diceThrow7 <- sample(1:6, 100, replace = TRUE)
diceThrow7
diceThrow8 <- sample(1:6, 100, replace = TRUE)
diceThrow8
mean_78 <- (diceThrow7 + diceThrow8) / 2
mean_78

# run for the first time
# install.packages("plyr")  # install the package called plyr
library(plyr)  # import the plyr package
require(plyr)  # another way of importing package
data_set <- count(mean_78)
data_set # this gives the frequency table

barplot(data_set$freq, names.arg = data_set$x, main = "frequecy diagram") # frequency diagram
hist(mean_78) # this also gives the frequecy table but on the range basis

# (c): ---------------------------------------------------------------------------------------------------------
# Construct the diagram for the cumulative frequency.
cum_frq <- cumsum(data_set$freq)  # cumsum fuction gives commulative frequency
cum_frq  # gives in table form

plot(cum_frq) # just gives in the dot form

barplot(cum_frq , main = "cumulative frequency diagram") # this will give in the bar form


# (3 ): --------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
# Again generate 0-1-random numbers. Exponentiate them (in a second column).Calculate mean, median, 
# standard deviation and skewness.

# generate 100 data
newRand <- runif(n=100,min=0,max=1)
newRand
# exponenent all above data
expNewRand <- exp(newRand)
expNewRand

summary(newRand) # gives max, min, mean, first quartile, second(median) and third quatile.
sd(newRand)      # 0.2987247
#  install.packages("moments")  # only do it at first time
require(moments)
skewness(newRand) # 0.1538236

summary(expNewRand)
sd(expNewRand)  # 0.4957223
skewness(expNewRand)  # 0.5019533


# (4 ): --------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
# This time generate 80 columns with 100 normally distributed random numbers (as in Exercise 1). Calculate  
# the 80 means and standard deviations, and construct the frequency diagrams (as in 2(b))

set.seed(1)
options(max.print = 2000000)  # remove print error
mat_rand <- matrix(data = rnorm(100), nrow = 100, ncol = 80)
mat_rand

# Use colMeans to calculate mean of individual column of matrix
colMeans(mat_rand) # gives the 80 means value for each column
apply(mat_rand, 2, mean) # this also gives mean for all column
mean(mat_rand[1])

# First argument is the matrix, second argument is whether row or column of matrix; 
apply(mat_rand, 2, sd) # will display all the column stardard deviation


