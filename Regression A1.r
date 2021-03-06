

#Read in data
file1 <-"http://www.math.mcgill.ca/yyang/regression/data/a1-1.txt"
data1 <-read.table(file1,header=TRUE)
file2 <-"http://www.math.mcgill.ca/yyang/regression/data/a1-2.txt"
data2 <-read.table(file2,header=TRUE)
file3 <-"http://www.math.mcgill.ca/yyang/regression/data/a1-3.txt"
data3 <-read.table(file3,header=TRUE)

#DATA SET 1 
#Question 1a(i)
x1 <-data1$x
y1 <-data1$y
fit1 <-lm(y1~x1, data = data1) 
coef(fit1)

#Question 1a(ii)
plot1 <-plot(x1, y1)
abline(coef(fit1), col = "blue")

#Question 1a(iii)
residualPlot <-plot(x1,residuals(fit1), main = "Residuals of Fit 1")
abline(h=0, col='red')

#Question 1a(iv)

#We can see that there are a few outlier points, however generally speaking the
#residuals demonstrate the general assumptions that zero is the mean for all x
#and that there is a constant variance for all x. 

#DATA SET 2 
#Question 1a(i)
x2 <- data2$x
y2 <- data2$y

fit2 <-lm(y2~x2,data=data2) #Least squares fit
coef(fit2)      #Get the parameter estimates

#Question 1a(ii)
plot2 <-plot(x2, y2)
abline(coef(fit2), col = "blue")

#Question 1a(iii)
residualPlot2 <-plot(x2,residuals(fit2), main = title("Residuals of Fit 2"))
abline(h=0, col='red')

#Question 1a(iv)

#DATA SET 3 
#Question 1a(i)
x3 <- data3$x
y3 <- data3$y
fit3 <-lm(y3~x3,data=data3) #Least squares fit
coef(fit3)      #Get the parameter estimates

#Question 1a(ii)
plot3 <-plot(x3, data3$y3)
abline(coef(fit3), col = "blue")

#Question 1a(iii)
residualPlot3 <-plot(x3,residuals(fit3), main = title("Residuals of Fit 3"))
abline(h=0, col='red')

#Question 1a(iv)

#Question 1b(i) 
file1 <-"http://www.math.mcgill.ca/yyang/regression/data/a1-1.txt"
data1 <-read.table(file1,header=TRUE)
x1 <-data1$x
y1 <-data1$y
#Shift and scale models, I am choosing values for m and l 
m <- 2
l <- 0.5 

shiftx <- x1-m
rescalex <- x1*l
shiftFit <- lm(y1~shiftx)
rescaleFit <- lm(y1~rescalex)
fit1 <-lm(y1~x1, data = data1) 

coef(shiftFit)
coef(rescaleFit)

#Now we want to repeat the computations for the parameter estimas for beta hat 0 and beta hat 1
beta0 = coef(fit1)[1]
beta1 = coef(fit1)[2]
betahat0_shift = beta0+m*beta1
betahat1_shift = beta1

betahat0_rescale = beta0
betahat1_rescale = beta1*(1/l)

#Now we want to numerically verify

print(coef(fit1)[1] + coef(fit1)[2]*m)

print(coef(fit1)[2]*(1/l))

#Question 6

#Question 6a
x4 <-runif(100, min=-1, max=1)

epsilon <-rnorm(100)

y4 <-5+(3*x4)+epsilon

fit4 <- lm(y4~x4)
plot4 <- plot(x4,y4)
abline(coef(fit4), col = "blue")

#Question 6b 
beta1 <- rep(0,1000)
for(i in 1:1000){
  
x5 <-runif(100, min=-1, max=1)
  
epsilon <-rnorm(100)
  
y5 <-5+(3*x5)+epsilon
  
fit5 <- lm(y5~x5)
  
beta1[i]=coef(fit5)[2]

}

mean(beta1)

hist(beta1)

#Question 6c

beta1 <- rep(0,1000)
for(i in 1:1000){
  
  x6 <-runif(100, min=-1, max=1)
  
  epsilon <-rcauchy(100)
  
  y6 <-5+(3*x6)+epsilon
  
  fit6 <- lm(y6~x6)
  
  beta1[i]=coef(fit6)[2]
  
}

mean(beta1)

hist(beta1)

#The histogram is now spread out from a larger range of negatives to positives, where in the original histogram it was from 2.4 to 3.7 where it was centred at 2.9. 
#Now the new histogram has most of its values close to 100, and a few below zero. The original histogram is normally distributed, but the second histogram is not. 
#The original histogram is strictly positive. Also the frequency of historgram 2 is much larger (800) than the original historgram where epsilon is normally distributed(200). 
#Hence beta 1 is no longer normally distributed. 

#Question 6d

x7 <-runif(100,min=-1,max=1)

delta <-rnorm(100, mean = 0, sd = 2)

epsilon <- rnorm(100)

w <- x7 + delta

y7 <-5+(3*x7)+epsilon

fit7 <- lm(y7~w)

plot7 <- plot(w,y7)

abline(coef(fit7), col = "blue")

#Repeat 1000 times

beta1 <- rep(0,1000)
for (i in 1:1000){
  
  x7 <-runif(100,min=-1,max=1)
  
  delta <-rnorm(100, mean = 0, sd = 2)
  
  epsilon <- rnorm(100)
  
  w <- x7 + delta
  
  y7 <-5+(3*x7)+epsilon
  
  fit7 <- lm(y7~w)
  
  beta1[i] = coef(fit7)[2]
  
}

mean(beta1)
hist(beta1)

#By adding on the errors, the mean of beta1 has drastically changed, and the values/range in which they fall under is much smaller compared to before.
#The histogram is still normally distributed. 