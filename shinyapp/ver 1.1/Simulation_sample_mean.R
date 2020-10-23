## simulation code to demonstrate 
## 1. sampling variation of the sample means of different samples from the same population
## 2. SE: quantify variability of sample means across different samples
## 3. compare the "real" SE from many samples with the estimated SE based on one single sample
## 4. Central Limit Theorem

##1. draw sample of sample size n from a normal population with mean u0 and standard deviation s0

n <- 100 # you can change n, u0 and s0 as you wish
u0 <- 1
s0 <- 2


samp <- rnorm(n, mean=u0, sd=s0)
str(samp)
mean(samp) #sample mean, compare this with u0
var(samp)^0.5 #sample standard deviation, compare with s0

# let's repeat the sampling 10 times and examine how the sample results vary 

for (t in 1:10) {
  print(paste("The results from ", t, "th sample"))
  samp <- rnorm(n, mean=u0, sd=s0)
  # take a peak of the sample
  str(samp)
  print(paste("sample mean, compare this with", u0))
  print(mean(samp))
  print(paste("sample standard deviation, compare this with", s0))
  print(var(samp)^0.5)
}

##2.Examine sampling distribution and calculate SE
# first create a vector that will save sample means from 500 different samples
# you can change 500 to even a larger number
samp.mean.vec<-rep(0, 500)

for (i in 1:500) {
  samp<-rnorm(n, mean=u0, sd=s0)
  samp.mean.vec[i]<-mean(samp)
}
# take a look at the first 30 sample means
print(samp.mean.vec[1:30])
# the sampling distribution of sample means
hist(samp.mean.vec)
abline(v=u0, col="red")

##3. Compare SE from many samples with estimated SE from one sample
# the mean of the sample means, compare it with u0
mean(samp.mean.vec)
# the standard deviation of the sample means
SE.pop <- var(samp.mean.vec)^0.5
SE.pop
### Important!
### This value is the standard error of the sample means 
### when we have access to multiple copies of the samples

# In a real data analysis, we only work with one sample, 
# so we estimate the SE of the sample mean
# take one sample
samp <- rnorm(n, mean=u0, sd=s0)
xbar <-mean(samp)
xbar.se <- sqrt(var(samp)/n)
xbar.se # the SE we use in real data analysis, is an estimate of SE.pop
SE.pop # the SE calculated following the actual meaning of the SE


##4. CLT thereom for large sample size, 
##sample mean ~Normal(population mean, var/n)

hist(samp.mean.vec, freq=FALSE)
x0 <- sort(samp.mean.vec) # sort the values so I can plot the density line
# overlay a normal distribution with mean u0, variance s0^2/n
lines(x0,dnorm(x0, mean=u0, sd=s0/n^0.5), col="red")

## CLT actually works for nonormal data
# let's try binomial distribution
# toss a bag of ten coins, each with probability of heads of 0.3, 
# record the total number of heads
# obtain a sample of size n (tossing the bag of ten coins n times), 
# the total number of heads for each toss is recorded in "samp"
samp.binom <- rbinom(n, size=10,prob=0.5)
# the mean of binomial distribution is prob*size
0.5* 10 # mean of the population
mean(samp.binom)  # sample mean
# the variance of binomial distribution is prob*(1-prob)*size
0.5*0.5*10 # variance of the population
var(samp.binom) # sample variance

## generate 500 such samples
samp.binom.mean.vec<-rep(0, 500)

for (i in 1:500) {
  samp.binom<-rbinom(n, size=10,prob=0.5)
  samp.binom.mean.vec[i]<-mean(samp.binom)
}

hist(samp.binom.mean.vec, freq=FALSE)
x0 <- sort(samp.binom.mean.vec) # sort the values so I can plot the density line
# overlay a normal distribution with mean u0, variance s0^2/n
lines(x0,dnorm(x0, mean=0.5*10, sd=sqrt(0.5*0.5*10/n)), col="red")

