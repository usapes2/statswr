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
