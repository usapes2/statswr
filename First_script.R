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
