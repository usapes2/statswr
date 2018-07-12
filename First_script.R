# Example 4.1
# A lot containing 7 components is sampled by a quality inspector; the lot contains
# 4 good components and 3 defective components. A sample of 3 is taken by the
# inspector. Find the expected value of the number of good components in this
# sample.

###Solution :

# # Let X represent the number of good components in the sample. 
# The probability distribution of X is
# f(x) = choose(4,x)*choose(3,3-x)/choose(7,3)

x <- c(0,1,2,3)

# Probability distribution vector
f <- choose(4,x)*choose(3,3-x)/choose(7,3)
exp_val = sum(x*f)
#Answer
round(exp_val,1)

#Example 4.2
# A salesperson for a medical device company has two appointments on a given day.
# At the first appointment, he believes that he has a 70% chance to make the deal,
# from which he can earn $1000 commission if successful. On the other hand, he
# thinks he only has a 40% chance to make the deal at the second appointment,
# from which, if successful, he can make $1500. What is his expected commission
# based on his own probability belief? Assume that the appointment results are
# independent of each other.

## Solution

#let p1 = 0.7 and p2 = 0.4 success 
p1 <- 0.7
p2 <- 0.4
commition_val <- c(0,1000,1500,2500)
# First, we know that the salesperson, for the two appointments, can have 4 possible
# commission totals: $0, $1000, $1500, and $2500. We then need to calculate their
# associated probabilities. By independence, we obtain
f1 <- (1-p1)*(1-p2) # 0 $
f2 <- p1*(1-p2)     # 1000 $
f3 <- (1-p1)*p2     # 1500 $
f4 <- p1 * p2       # 2500$

f<- c(f1,f2,f3,f4)

Expected_commision <- sum (commition_val * f)



#Ex4.1

# The probability distribution of X, the number of
# imperfections per 10 meters of a synthetic fabric in continuous rolls of uniform width, is given in Exercise 3.13
# on page 92 as
# x 0 1 2 3 4
# f(x) 0.41 0.37 0.16 0.05 0.01
# Find the average number of imperfections per 10 meters of this fabric.
###Solutions
x<-c(0:4)
f<-c(0.41, 0.37, 0.16, 0.05, 0.01)
avg_number_of_imp <- sum(x*f)
avg_number_of_imp

#Ex4.2
###Solution
x<-(0:3)
f<-choose(3,x)*((1/4)^x)*(3/4)^(3-x)
mean_of_x = sum(x*f)
mean_of_x

# Ex 4.4
# A coin is biased such that a head is three times
# as likely to occur as a tail. Find the expected number
# of tails when this coin is tossed twice.

###Solution

p_h <- 3/4
p_t <- 1/4

# There are 4 possibilites HH HT TH TT 
# Let X be the r.v representing the number of tails per event ( 2 tosses ).
x<- (0:2)
f<- c(p_h*p_h,2*p_h*p_t,p_t*p_t)
exp_numberOfTails<-sum(x*f)
exp_numberOfTails
# Ex 4.7
# 
# By investing in a particular stock, a person can
# make a profit in one year of $4000 with probability 0.3
# or take a loss of $1000 with probability 0.7. What is
# this person’s expected gain?

x<-c(4000,-1000)
f<-c(0.3,0.7)

exp_gain<-sum(x*f)
exp_gain
# Ex 4.13
# The density function of the continuous random
# variable X, the total number of hours, in units of 100
# hours, that a family runs a vacuum cleaner over a pe-
#   riod of one year, is given in Exercise 3.7 on page 92
# # as
# Find the average number of hours per year that families
# run their vacuum cleaners.

## define the integrated function
integrand1 <- function(y) {y^2} # 0 to 1
integrand2 <- function(z) {(2-z)*z} # 1 to 2
integrand3 <- 0         #else


integrate(integrand1,0,1)$value+integrate(integrand2,1,2)$value
#ex 4.18
x<-(0:3)
f<-choose(3,x)*((1/4)^x)*(3/4)^(3-x)
mean_of_x = sum(x^2*f)
mean_of_x
9/8

#Ex 4.29

# Exercise 3.29 on page 93 dealt with an impor-
#   tant particle size distribution characterized by
# (a) Plot the density function.
# (b) Give the mean particle size.

integrand1 <- function(y) {3*y^(-4)} # 1 to inf
curve(integrand1,1,4)
integrand1 <- function(y) {(3*y^(-4))*y} # 1 to inf
integrate(integrand1,1,Inf)$value

#Example 4.11
x<-(0:3)
g<-2*x+3
f<-c(1/4,1/8,1/2,1/8)
sum(f)
mean_gOfx <- sum(g*f)
mean_gOfx  

var_gOfx <- sum (((g-mean_gOfx)^2)*f)
var_gOfx
# ex 4.34
# Let X be a random variable with the following
# probability distribution:
#   x −2 3 5
# f(x) 0.3 0.2 0.5
# Find the standard deviation of X.

x<- c(-2,3,5)
f_x <- c(0.3,0.2,0.5)
mean_x <- sum(x*f_x)
var <- sum((x^2)*f_x) - mean_x^2
sqrt(var) # standard deviation.
# 
# 
# The random variable X, representing the num-
#   ber of errors per 100 lines of software code, has the
# following probability distribution:
#   x 2 3 4 5 6
# f(x) 0.01 0.25 0.4 0.3 0.04
# Using Theorem 4.2 on page 121, find the variance of
# X.

x<- c(2,3,4,5,6)
f_x <- c(0.01,0.25,0.4,0.3,0.04)
mean_x <- sum(x*f_x)
var <- sum((x^2)*f_x) - mean_x^2
# 
# 
# For a laboratory assignment, if the equipment is
# working, the density function of the observed outcome
# X is
# f(x) =
#   ? 2(1 − x),
# 0 < x < 1,
# 0, otherwise.
# Find the variance and standard deviation of X.



integrand1 <- function(y) {2*(1-y)*y} # 0 to 1
mean_<-integrate(integrand1,0,1)$value
integrand1 <- function(y) {2*(1-y)*y^2} # 0 to 1
mean_xsqred<-integrate(integrand1,0,1)$value

var<-mean_xsqred-mean_^2
sqrt(var) #Standart deviation
