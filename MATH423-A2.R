file1 <- "http://www.math.mcgill.ca/yyang/regression/data/salary.csv"
salary <- read.csv(file1, header = TRUE)
x1 <- salary$SPENDING/1000
y1 <- salary$SALARY
fit.Salary <- lm(y1 ~ x1)
summary(fit.Salary)

#Question 2.1 

plot(x1,residuals(fit.Salary))
abline(h=0,col='red',lty=2)

#Data points are not closely clustered to 0, they are supposed to be. No pattern which is good. A few outliers. 

#Question 2.2
qqnorm(residuals(fit.Salary))
#yes this follows the normal distribution because the line is straight 

#Question 2.3 
fit.Salary <- lm(y1 ~ x1)
residuals.Salary <- residuals(fit.Salary)
fitted.Salary <- fitted(fit.Salary)
sum(residuals.Salary) #sum of residuals 

sum(x1*residuals.Salary) #sum of x times residuals 

sum(fitted.Salary*residuals.Salary) #sum of fitted values times residuals 

#All of these are small, so we're good. 

#Question 2.4
summary(fit.Salary)

#beta hat not: 12129.4
#beta hat one: 3307.6 
#both p values are very small so we can say that the estimates for beta hat not an beta hat one are significant and strong/good estimators. 

#Question 2.6 
error <- qt(0.90,df=49)*311.7 
left <- 3307.6-error #2902.680
right <- 3307.6+error #3712.519 

#Question 3 
file1 <- "http://www.math.mcgill.ca/yyang/regression/data/abalone.csv"
abalone <- read.csv(file1, header = TRUE)

height <- abalone$Height
rings <- abalone$Rings
graph1 <- lm(rings ~ height, data = abalone)
graph2 <- plot(height, rings)
summary(plot)

#Information on Height
hist(height) #dont forget to talk about distribution of the curve

mean(abalone$Height)
median(abalone$Height)
var(abalone$Height)
sd(abalone$Height)
range(abalone$Height)

#Information on Rings 
hist(rings)

mean(abalone$Rings)
median(abalone$Rings)
var(abalone$Rings)
sd(abalone$Rings)
range(abalone$Rings)

#Scatter Plot of data 
scatterPlot <- plot(height, rings) + title("Scatter Plot of Height vs. Rings") 

#Linear Regression Model
graph1 <- lm(rings ~ height, data = abalone)
summary(graph1)

#Scatter Plot with estimated regression function 
scatterPlot1 <-  plot(height, rings) + title("Scatter Plot of Height vs. Rings with Regression Line") 
abline(graph1)

#Assessing Model Assumptions 
residualPlot <- plot(height, residuals(graph1), pch = 16, xlim= c(0, 0.25), ylim = c(-15, 15), xlab = "Heigh (mm)", ylab = "Residuals") + title("Residual Plot")
abline(h =0, col = "blue")

qqplot(height, residuals(graph1), xlim = c(0, 0.25), ylab = "Residuals", xlab ="Heigh (mm)") + title("Q-Q Plot of Residuals vs. Height")

#Model refit
refit <- lm(log(rings)~height, data = abalone)
plot(height, log(rings), main = "Scatterplot of Height vs. log(Rings)", ylab = "log(Rings)", xlab = "Height (mm)", xlim = c(0, 0.25))
abline(coef(refit), col = "red")

#Redo diagnostics
residualsRefit <- plot(height, residuals(refit), pch = 16, xlim= c(0, 0.25), ylim = c(-2, 2), xlab = "Height (mm)", ylab = "Residauls of Refit") + title("Residuals")
abline(h=0, col = "blue")
paste(mean(residuals(refit)))
qqplot(height, residuals(refit), xlim = c(0, 0.25), xlab = "Height (mm)", ylab = "Residuals") + title("Q-Q Plot of Residuals of Refit")

#Summary of refit
summary(refit)

#95% CI for intercept and slope
confint(refit, level = 0.95)

#point estimate at h = 0.128 (confidence interval)
conf.int <- predict(refit, data.frame(height = 0.128), level = 0.95, interval = 'confidence')
conf.int

#h = 0.132 (prediction interval)
pred.int <- predict(refit, data.frame(height = 0.132), level = 0.99, interval = 'prediction')
pred.int