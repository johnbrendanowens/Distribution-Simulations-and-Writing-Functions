
#6.1

n=100
p=0.02
m=1000

par(mfrow = c(1, 2))
binom.sample = rbinom(m, n, p)
hist(binom.sample, prob = T, main = "n = 100")
curve(dnorm(x, n*p, sqrt(n*p*(1-p))), add = T)

p2=0.2

binom.sample2 = rbinom(m, n, p2)
hist(binom.sample2, prob = T, main = "n = 100")
curve(dnorm(x, n*p2, sqrt(n*p2*(1-p2))), add = T)

qqnorm(binom.sample, main = "Normal QQ Plot for Bin(100, 0.02)")
qqline(binom.sample)

qqnorm(binom.sample2, main = "Normal QQ Plot for Bin(100, 0.2)")
qqline(binom.sample2)

#6.6

par(mfrow = c(1, 1))
c.distribution = function(m=500, n=10){
mean.est=c();
median.est=c()
for(i in 1:m) {
  x = rt(n, df=3)
  mean.est[i] = mean(x)
  median.est[i] = median(x)
}
data.frame(mean.est, median.est)
}

c.distribution.t3 = c.distribution()
attach(c.distribution.t3)
boxplot(mean.est, median.est, names = c("Mean", "Median"), ylab = "Estimate", main = "S
ampling Distributions of Mean and Median")

summary (c.distribution.t3)

sapply(c.distribution.t3, sd)

density(mean.est)
density(median.est)

plot(density(mean.est), xlim = c(-3, 2.5), type = "l", ylim = c(0, 1.1),
     yaxs = "i", xlab = "Estimate", ylab = "Density",
     main = "Sampling Distributions of Mean and Median from t(3)")
lines(density(median.est), lty = 2)
legend(-3, 1.05, c("Mean","Median"), lty = 1:2)

detach(c.distribution.t3)
#6.8
xbar = c();std = c()
for(i in 1:500) {
  sam = rt(10, 3)
  xbar [i] = mean(sam); std[i] = sd(sam)
}
plot(xbar,std,main = "Mean vs. Std.Dev for t(3)")

xbar = c();std = c()
for(i in 1:500) {
  sam = rnorm(10)
  xbar [i] = mean(sam); std[i] = sd(sam)
  }
plot(xbar,std, main ="Mean vs. Std.Dev for N(0,1)")
cor(xbar,std)


xbar = c();std = c()
for(i in 1:500) {
  sam = rexp(10, 1)
  xbar [i] = mean(sam); std[i] = sd(sam)
}
plot(xbar,std, main ="Mean vs. Std.Dev for Exp (1)")
cor(xbar,std)


#E4
#Function to find COnfidence Intervals
confidenceinterval = function (xmean, s, alpha, n) {
  error <- qt(1-alpha/2,df=n-1)*s/sqrt(n)
  left <- xmean-error
  right <- xmean+error
  level = alpha*100
  cat(" Left Bound =", left, fill = T)
  cat("Right Bound = ", right, fill = T)
  cat("Confidence Level = ", level, fill = T)
}

xmean = 50
alpha = .10
s = 2
n = 20

confidenceinterval (xmean, s, alpha, n) 

#E5

#Function to find Zeroes of a equation

 findzeros = function(f, f1) {
   x=0
  
   lastx = 1000
   zero = polyroot(f(x))
   while(abs(x-lastx)>1.0e-8) {
     lastx = x
     x = x -f(x)/f1(x)
     cat("Current x =",x, "Function = ",f(x), "Derivative = ",f1(x), fill = T)
     }
  cat ("Zero at x =", zero, fill = T)
   x
   }
f= function(x) x^2-sin(x)
f1 = function(x) 2*x-cos(x)
findzeros(f, f1)

findzeros = function(f, f1) {
  x=999
  
  lastx = 1000
  zero = polyroot(f(x))
  while(abs(x-lastx)>1.0e-8) {
    lastx = x
    x = x -f(x)/f1(x)
    cat("Current x =",x, "Function = ",f(x), "Derivative = ",f1(x), fill = T)
  }
  cat ("Zero at x =", zero, fill = T)
  x
}
f= function(x) x^2-sin(x)
f1 = function(x) 2*x-cos(x)
findzeros(f, f1)
