#----------------------------------------------------------------------------------------------------------------
#-----------------------Normal Distribution Curve and Capability Index------------------------------------------
#----------------------------------------------------------------------------------------------------------------

remove(list = ls())

################################################################################################################
# capability index = to measure the ability of a process to produce output within customer’s specification limits.
# In simple words, it measures producer’s capability to produce a product within customer’s tolerance range.
# cpk = (Gup -Glo)/(6 * sigma_max) where Gup and Glo are the upper and lower value given by the customer.
# cp_up <- (Gup-G_mean)/(3* sigma)  upper capability index
# cp_lo <- (G_mean-Glo)/(3*sigma) lower capability index
################################################################################################################

# (1 ): --------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
#construt a table from –6 to +6 with a spacing of 0,5 outside of +/- 3 and 0,1 inside of +/-3
u1 <- c(seq(-6,-3,.5),seq(-2.9,2.9,.1),seq(3,6,.5))
u1
table_1 <- matrix(u1, ncol = 1, nrow = , dimnames = list(c(),c("u")))
table_2 <-format(table_1,scientific = FALSE)   # remove scientific notaion that is exponential in r
table_3 <-format(round(table_1,2), nsmall = 2) # round off to two decimal place
table_3


# (b):  -------------------------------------------------------------------------------------------------------
# construct next column with standard normal distribution of above value in standard normal distribution u = 0, 
# and sd =1 dnorm gives the probability at certain value
nor_value <- dnorm(u1, mean = 0, sd = 1) 
nor_value
final_nor_value <- matrix(nor_value, ncol = 1, nrow = , dimnames = list(c(),c("p")))
final_nor_value


# (c): ----------------------------------------------------------------------------------------------------------
# Plot the function of the cumulative standard normal distribution as a line plot.
plot(nor_value, type = "l")
plot(u1,pnorm(u1))            




# (2): ---------------------------------------------------------------------------------------------------------
# Assume your production process produces parts of average size of μ = 12 with standard deviation of σ = 0,4.
# data given in question
u <- 12
s <- 0.4

# (a): ---------------------------------------------------------------------------------------------------------
#probability of getting a part that lies between 11 and 12
#pnorm gives cumulative probality distribution till that point from -infinity
p_till_11<- pnorm(11, mean = u, sd = s, lower.tail = TRUE) 
p_till_11  # 0.02275013
p_till_12 <- pnorm(12, mean = u, sd = s, lower.tail = TRUE) 
p_till_12  # 0.6914625
p_bet_11_12 <- p_till_12 - p_till_11
p_bet_11_12 # 0.4937903



# (b): ---------------------------------------------------------------------------------------------------------
#probability of getting a part that lies between 11.2 and 12.8
p_bet_11.2_12.8 <- pnorm(12.8, u, s, lower.tail = TRUE)-pnorm(11.2, u, s, lower.tail = TRUE)
p_bet_11.2_12.8   # 0.9544997


# (c): --------------------------------------------------------------------------------------------------------
# within what symmetrical limit will the next part be with probability >99% default lower tail is TRUE that 
# is P(X<=x) 99.5 % of symmetric system corresponds to the 99 % of assymmetric value
# v(symmetrical) = v(assymetrical)((1+p)/2) i.e v(symm)(95)=v(assym)(97.5)
p_gre_99.5perc_low <- qnorm(.005, mean = 12, sd = 0.4, lower.tail = TRUE)  
p_gre_99.5perc_low #10.96967
p_gre_99.5perc_up <- qnorm(.995,mean = 12, sd = 0.4, lower.tail = TRUE) 
p_gre_99.5perc_up # 13.03033

# note in normal distribution lower.tail = true means p(X<x)


# (3) : -------------------------------------------------------------------------------------------------------
# Your customer wants to be 99% sure, that your parts are between G lo =11 and G up =13.
# given data in the question 
Glo <- 11    #lower value
Gup <- 13    #upper value
G_mean <- (11+13)/2
G_mean  # 12
prob <- 0.99    #customer wants to be 99% sure(probability)

# (a): --------------------------------------------------------------------------------------------------------
# What's the largest σ that your process may have? Careful again: you have to think! Where must μ be for σ 
# to be largest? Once you've decided that, find σ, using standnorm....() resp. norms....().
# 99% of symmetric system corresponds to the 99.5% of assymmetric value
# using standard normal distribution
p_up <- 0.995
val_99.5 <- qnorm(0.995,mean = 0, sd = 1, lower.tail = TRUE)
val_99.5  # 2.575829
# for standardize what we must do is  new u(x) = (x-u) / σ(s.d) 
#upperlimit = mean_value+ val_99.5*sigma
#sigma = (upperlimit -mean value) / val_99.5
sigma_max <- (13 - 12)/ val_99.5
sigma_max    # 0.3882245

# (b): -----------------------------------------------------------------------------------------------------------
# Assuming the value of μ, that you found in (a), calculate the so-called capability index: c p = (G up - G lo )/6σ
# capability index
# ability of the process to produce output within customer's specification limit
cp <- (Gup -Glo)/(6 * sigma_max)
cp  # 0.8586098

# (c): ------------------------------------------------------------------------------------------------------------
# Note that in the case above μ= (G up +G lo )/2. This need not always be the case. In generalc p,up is 
# defined as (G up -μ)/3σ, and c p,low as (μ-G lo )/3σ.
#upper capability index i.e cp_up
cp_up <- (Gup-G_mean)/(3* sigma_max)
cp_up   #0.8586098
# lower capability index i.e cp_lo
cp_lo <- (G_mean-Glo)/(3*sigma_max)
cp_lo  #0.8586098
# since cp = cp_up = cp_lo = 0.8586098 they are same 

# (d): -----------------------------------------------------------------------------------------------------------
# Assuming μ moves to 11,8, what happens to c p,up = c p,low ? How would you define a better capability index, 
# call it c pk ? Calculate the largest σ that your process may have for cpk to be at least 1,333.
# when u = 11.8 what happens to cp cp_lo and cp_up
u <- 11.8
sigma_11.8 <- (Gup-u) / val_99.5
sigma_11.8  # 0.4658694
cp_up1 <- (Gup-u)/(3* sigma_11.8)
cp_up1 # 0.8586098
cp_lo1 <- (u-Glo)/(3*sigma_11.8)
cp_lo1  # 0.5724065

# therefore cp_up1!=cp_lo1!=cp

#how can you define better capability index


#calculate largest sigma for cpk to have at least 1.333
#cp_lo1 <- (u-Glo)/(3*sigma_max)
cpk <- 1.333
# taking the lower value
sigma_large1 <- ( 11.8 - 11) /(cpk * 3)
sigma_large1  # 0.20005


sigma_large2 <- ( 13 - 11.8 ) /(cpk * 3)
sigma_large2  # 0.20005

# taking the upper value

# can be at max .300075

# sometimes cpk can be negative that means the mean is outside larger or samller value.




