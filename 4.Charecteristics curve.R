#----------------------------------------------------------------------------------------------------------------
#----------------------Characteristic curve-------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

remove(list = ls())

###############################################################################################################
# type-I(alpha-error) : reject null hypothesis when its true
# type-II(beta-error) : accept null hypothesis when its false
# from previous exercise t(calc) = mean_difference/(common_sd *(sqrt(2/n)))
# μ(cric) = t(cric) * sd * sqrt(2/n)
# actually u critical is difference of means of two samples so this plays important role
# oc is ploted as u in x-axis and the probability of occurance of u with respect to n in y-axis
# error I lies in the upper part of the curve where as error II lies in lower part

################################################################################################################

# (1 ): --------------------------------------------------------------------------------------------------------
# (a): ---------------------------------------------------------------------------------------------------------
#Determine μ(krit) for sample sizes n = 8, 16, 32, 64, 128 und 256. Use the standard deviation from assignment 4 
sd <-29.52088
sd

#for n =8
n1 <- 8
t1_cric <- qt(0.05, 14, lower.tail = FALSE) 
t1_cric   #1.76131
u1_cric <- t1_cric * sd * sqrt(2/n1)
u1_cric  #25.99771

# for n =16
n2 <- 16
t2_cric <- qt(0.05, 30, lower.tail = FALSE)
t2_cric   # 1.697261
u2_cric <- t2_cric * sd * sqrt(2/n2)
u2_cric  # 17.71466

# for n = 32
n3 <- 32
t3_cric <- qt(0.05, 62, lower.tail = FALSE)
t3_cric   # 1.669804
u3_cric <- t3_cric * sd * sqrt(2/n3)
u3_cric  # 12.32352

# for n = 64
n4 <- 64
t4_cric <- qt(0.05, 162, lower.tail = FALSE)
t4_cric   # 1.654314
u4_cric <- t4_cric * sd * sqrt(2/n4)
u4_cric  #  8.633209

# for n = 128
n5 <- 128
t5_cric <- qt(0.05, 254, lower.tail = FALSE)
t5_cric   # 1.650875
u5_cric <- t5_cric * sd * sqrt(2/n5)
u5_cric  #  6.09191

# for n = 256
n6 <- 256
t6_cric <- qt(0.05, 610, lower.tail = FALSE)
t6_cric   # 1.647355
u6_cric <- t6_cric * sd * sqrt(2/n6)
u6_cric  #  4.298447


# (b).-------------------------------------------------------------------------------------------------------
# Calculate p(μ,n), i.e. the probability of accepting the null hypotheses given μ, 
# and fill out the fields in the table.

# this value is given on the question
mu <- seq(-10,75,by=0.1)

# for n = 8
# here we are using pt(tcalc, df)
# first term refers to calculated t(t is actually difference of mean) value as in above exercise and 
# gives probability of occurence
p1 <- pt(((u1_cric-mu)* sqrt(n1))/(sd *sqrt(2)), 14)
p1   # this gives a series of data each time mu value increases

# for n = 16
p2 <- pt(((u2_cric-mu)* sqrt(n2))/(sd *sqrt(2)), 30)
p2

# for n = 32
p3 <- pt(((u3_cric-mu)* sqrt(n3))/(sd *sqrt(2)), 62)
p3

# for n = 64
p4 <- pt(((u4_cric-mu)* sqrt(n4))/(sd *sqrt(2)), 126 )
p4

# for n = 128
p5 <- pt(((u5_cric-mu)* sqrt(n5))/(sd *sqrt(2)), 254 )
p5

# for n = 256
p6 <- pt(((u6_cric-mu)* sqrt(n6))/(sd *sqrt(2)), 610 )
p6

# (c).-------------------------------------------------------------------------------------------------------
# Use the assistent to draw the OC-curves for n = 8, 16, 32 und 64.

plot(mu, p1, type = "l")
lines(p2, type = "l")
lines(p3, type = "l")
lines(p4, type = "l")
lines(p5, type = "l")
lines(p6, type = "l")

# here as n increase the curve shrink better containing less area so difference of mean is less i think.


# (2).-------------------------------------------------------------------------------------------------------
# (a): ------------------------------------------------------------------------------------------------------
# Determine the sample sizes to get β<50% und β<10% (see table at botton left). To do this, change n-init 
# for β<50% and β<10%, until it is greater than n-ergebnis ist.

# for β<50%
common_sd <-29.52088 # form exercise 4
alfa <- 0.05
beta <- 0.5
mean_diff <- 71.25 # form exercise 4

# for n = 2
df <- 2 * 2 - 2
n_calc1 <- ((qt(alfa, df)+qt(beta, df))^2 * common_sd^2 * 2)/mean_diff^2
n_calc1 # 2.927388
# since 2 >= 2.927388 is not true so the value of n = 2 is not suitable

# for n = 3
df1 <- 2 *3 - 2
n_calc2 <- ((qt(alfa, df1)+qt(beta, df1))^2 * common_sd^2 * 2)/mean_diff^2
n_calc2 # 1.560382
# since 3 >= 1.560382 is true so the value of n => 3 is correct value which can be choosen


# for β<10%
common_sd <-29.52088 # form exercise 4
alfa <- 0.05
beta1 <- 0.1
mean_diff <- 71.25 # form exercise 4

# for n = 2
df <- 2 * 2 - 2
n_calc1 <- ((qt(alfa, df)+qt(beta1, df))^2 * common_sd^2 * 2)/mean_diff^2
n_calc1 # 7.928933
# since 2 >= 7.928933 is not true so the value of n = 2 is not suitable

# for n = 3
df1 <- 2 *3 - 2
n_calc2 <- ((qt(alfa, df1)+qt(beta1, df1))^2 * common_sd^2 * 2)/mean_diff^2
n_calc2 # 4.611895
# since 2 >= 4.611895 is not true so the value of n = 3 is also incorrect 

# for n = 3
df2 <- 2 *3 - 2
n_calc2 <- ((qt(alfa, df2)+qt(beta1, df2))^2 * common_sd^2 * 2)/mean_diff^2
n_calc2 # 4.611895
# since 3 >= 4.611895 is not true so the value of n = 3 is also incorrect 

# for n = 4
df3 <- 2 *4 - 2
n_calc3 <- ((qt(alfa, df3)+qt(beta1, df3))^2 * common_sd^2 * 2)/mean_diff^2
n_calc3 # 3.929221
# since 4 >= 3.929221 is  true so the value of n => 4 is correct 




