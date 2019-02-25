#QUESTION 2
#putting the data into a table (transpose of the original)
data_matrix <- matrix(c(4,5,25,3,4,20,10,9,57,7,10,50), ncol=3,byrow=TRUE)
colnames(data_matrix) <- c("X1","X2", "Y")

#converting matrix into data frame 
data1 <- as.data.frame(data_matrix)

#Question 2.1 
#fitting multiple linear regression 
multfit <- lm(Y~X1+X2, data=data1)

#summary of multfit 
summary(multfit)

#Question 2.2
Y_matrix = matrix(c(25,20,57,50))
print(Y_matrix)
X_matrix <- matrix(c(1,4,5,1,3,4,1,10,9,1,7,10), ncol=3,byrow=TRUE)
print(X_matrix)
X_transpose <- t(X_matrix)
print(X_transpose)

#XTX
XTX <- X_transpose %*% X_matrix
print(XTX)

#XTX Inverse 
XTX_inverse <- solve(XTX)

print(XTX%*%XTX_inverse)
print(XTX_inverse) 

#XTY 
XTY <- X_transpose%*%Y_matrix
print(XTY)

#beta hat
beta_hat <- XTX_inverse %*% XTY
print(beta_hat)

#Question 2.4 
H_matrix <- X_matrix%*%XTX_inverse%*%X_transpose
print(H_matrix)

#Question 2.5 
res <- residuals(multfit)
resvar <- var(res)
beta_hat_variance <- resvar*(XTX_inverse %*% XTX %*% XTX_inverse)
print(beta_hat_variance)

#Question 4

#loading the data
data(stackloss)
names(stackloss)
## [1] "Air.Flow"   "Water.Temp" "Acid.Conc." "stack.loss"
help(stackloss)

#creating parameters 
y <- stackloss$stack.loss
x1 <- stackloss$Air.Flow
x2 <- stackloss$Water.Temp
x3 <- stackloss$Acid.Conc.

#Question 4.1
plot1 <- plot(stackloss)

#Question 4.2
fit <- lm(y~x1+x2+x3, data=stackloss)
summary(fit)

#Question 4.3 
confidence <- confint(fit, level = 0.9)
print(confidence)

#Question 4.4
prediction <- predict(fit, data.frame(x1= 58, x2 = 20, x3 = 86), level = 0.99)
prediction

#Question 4.5 
#The p-vale of beta 3 is 0.34405 which is greater than alpha = 0.10, therefore we do not reject the null hypothesis at an alpha level of
