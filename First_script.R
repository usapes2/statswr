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


# 4.57 Let X be a random variable with the following
# probability distribution: Find E(X) and E(X 2 ) and then, using these values,
#evaluate E[(2X + 1) 2 ].

x <- c(-3,6,9)
f_x<-c(1/6,1/2,1/3)

E_x <- sum(f_x*x)
E_x
E_x2 <- sum(f_x*x^2)
E_x2
#(2x_1)^2 = 4x^2 +4x +1 

ans<- 4*E_x2 +4*E_x +1
ans
# 
# 
# 4.65 Let X represent the number that occurs when a
# red die is tossed and Y the number that occurs when
# a green die is tossed. Find
# (a) E(X + Y );
# (b) E(X − Y );
# (c) E(XY ).

x<-c(1,2,3,4,5,6)
f_x<-c(1/6,1/6,1/6,1/6,1/6,1/6)

#a
2*sum(x*f_x)
#b
0
#c
sum(x*f_x)^2
# Example Binomial Distribution
# Strictly speaking, the Bernoulli process must possess the following properties:
# 1. The experiment consists of repeated trials.
# 2. Each trial results in an outcome that may be classified as a success or a failure.
# 3. The probability of success, denoted by p, remains constant from trial to trial.
# 4. The repeated trials are independent.
# 
# The number X of successes in n Bernoulli trials is called a binomial random
# variable. The probability distribution of this discrete random variable is called
# the binomial distribution, and its values will be denoted by b(x;n,p) since they
# depend on the number of trials and the probability of a success on a given trial.


# The probability that a certain kind of component will survive a shock test is 3/4.
# Find the probability that exactly 2 of the next 4 components tested survive.

dbinom(2,size=4,prob=3/4)
# 
# The probability that a patient recovers from a rare blood disease is 0.4. If 15 people
# are known to have contracted this disease, what is the probability that (a) at least
# 10 survive, (b) from 3 to 8 survive, and (c) exactly 5 survive?

#a 
1-pbinom(9,size=15,0.4) # pbinom is cumulative probability function for binomial distribution pbinom
#b
sum(dbinom(3:8,size = 15,0.4))
#c
dbinom(5,size = 15,0.4)

# Example 
# A large chain retailer purchases a certain kind of electronic device from a manu-
#   facturer. The manufacturer indicates that the defective rate of the device is 3%.
# (a) The inspector randomly picks 20 items from a shipment. What is the proba-
#   bility that there will be at least one defective item among these 20?
#   (b) Suppose that the retailer receives 10 shipments in a month and the inspector
# randomly tests 20 devices per shipment. What is the probability that there
# will be exactly 3 shipments each containing at least one defective device among
# the 20 that are selected and tested from the shipment?

#a Let p be the success - defective item p = 0.03
1-dbinom(0,size = 20,prob = 0.03)
#b
p <- 1-dbinom(0,size = 20,prob = 0.03)
dbinom(3,size=10,prob = p)
# 
# Ex 5.8 
# According to a study published by a group of Uni-
#   versity of Massachusetts sociologists, approximately
# 60% of the Valium users in the state of Massachusetts
# first took Valium for psychological problems. Find the
# probability that among the next 8 users from this state
# who are interviewed,
# (a) exactly 3 began taking Valium for psychological
# problems;
# (b) at least 5 began taking Valium for problems that
# were not psychological

#a
dbinom(3,size = 8, prob = 0.6)
#b
sum(dbinom(5:8,size = 8, prob = 0.6))


# 
# 5.10 A nationwide survey of college seniors by the
# University of Michigan revealed that almost 70% dis-
#   approve of daily pot smoking, according to a report in
# Parade. If 12 seniors are selected at random and asked
# their opinion, find the probability that the number who
# disapprove of smoking pot daily is
# (a) anywhere from 7 to 9;
# (b) at most 5;
# (c) not less than 8

#a

sum(dbinom(7:9,size = 12,prob=0.7))

#b

pbinom(5,size = 12,prob =0.7)

#c
sum(dbinom(8:12,size = 12,prob=0.7))

# 
# The probability that a patient recovers from a
# delicate heart operation is 0.9. What is the probabil-
#   ity that exactly 5 of the next 7 patients having this
# operation survive?

dbinom(5,size = 7,prob = 0.9)

# 
# 
# In testing a certain kind of truck tire over rugged
# terrain, it is found that 25% of the trucks fail to com-
#   plete the test run without a blowout. Of the next 15
# trucks tested, find the probability that
# 
# how many of the 15 trucks
# would you expect to have blowouts?

# mean = n*p 
15 * .25

# What is the variance of the number of blowouts ex-
#perienced by the 15 trucks? What does that mean?

15*.25*.75
