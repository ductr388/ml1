install.packages("kknn")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("ggplot2")
# Assignment 1                                            ####
# 1.1                                                     ####
library(kknn)
library(dplyr)
library(ggplot2)

# Read in data
data <- read.csv("optdigits.csv")
# Change to response variable
data <- rename(data, y=X0.26) 
data$y <- as.factor(data$y)

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 

id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,] 

# 1.2                                                     ####
model_kknn_train <-
  kknn(formula = y~.,
     train = train,
     test = train,
     kernel = "rectangular",
     k=30)

# Confusion matrix for train data
conf_mat_train <- table(model_kknn_train$fitted.values, train$y)
# Row is shown as first var, col as second var
conf_mat_train
acc_train <- sum(diag(conf_mat_train)) / sum(conf_mat_train)
miss_train <- 1-acc_train

# Confusion matrix for test data
model_kknn_test <-
  kknn(formula = y~.,
       train = train,
       test = test,
       kernel = "rectangular",
       k=30)

conf_mat_test <- table(model_kknn_test$fitted.values, test$y)
conf_mat_test
acc_test <- sum(diag(conf_mat_test)) / sum(conf_mat_test)
miss_test <- 1-acc_test

# 1.3                                                     ####
y <- train$y 
fit_y <- model_kknn_train$fitted.values
# probabilities given from 0 to 9, index 9 = number 8.
prob_8 <- model_kknn_train$prob[, 9]

data_8 <- data.frame(y = y,
                     fit_y = fit_y,
                     prob = prob_8)



data_8 <- data_8[data_8$y == "8", ]
head(arrange(data_8, prob))

# Change colour palette to black and white
colfunc <- colorRampPalette(c("white", "black"))

plot_8 <- function(index){
  plot <- as.matrix(train[index, -65])
  plot <- matrix(plot, nrow=8, byrow=TRUE)
  heatmap(plot, col=colfunc(16), Colv=NA, Rowv=NA)
}


# Found by reordering data in R data frame.
# Hardest to correctly predict: 1624, 1663, and 229
# Easiest to correctly predict: 1864 and 1811
plot_8(1624)
plot_8(1663)
plot_8(229)

plot_8(1864)
plot_8(1811)

# 1.4                                                     ####


fit_kknn <- function(k){
  model_kknn_train <-
    kknn(formula = y~.,
         train = train,
         test = train,
         kernel = "rectangular",
         k=k)
  
  # Confusion matrix for train data
  conf_mat_train <- table(model_kknn_train$fitted.values, train$y)
  acc_train <- sum(diag(conf_mat_train)) / sum(conf_mat_train)
  miss_train <- 1-acc_train
  
  model_kknn_valid <-
    kknn(formula = y~.,
         train = train,
         test = valid,
         kernel = "rectangular",
         k=k)
  
  # Confusion matrix for train data
  conf_mat_valid <- table(model_kknn_valid$fitted.values, valid$y)
  acc_valid <- sum(diag(conf_mat_valid)) / sum(conf_mat_valid)
  miss_valid <- 1-acc_valid
  result <- c(miss_train, miss_valid)
  return(result)
}

result <- data.frame(train = 0, valid = 0)
for(i in 1:30){
  model <- fit_kknn(i)
  result[i,1] <- model[1]
  result[i,2] <- model[2]
}

result$index <- 1:30

ggplot(result) +
  geom_line(aes(x=index, y=train, colour="train")) +
  geom_line(aes(x=index, y=valid, colour="valid")) +
  scale_color_manual(name = "Y series",
                     values = c("train" = "steelblue", "valid" = "indianred")) +
  theme_bw() +
  labs(x = "k",
       y = "Missclassification rate")

which(result$valid == min(result$valid))

model_test_7 <- 
  kknn(formula = y~.,
     train = train,
     test = test,
     kernel = "rectangular",
     k=7)

conf_mat_test <- table(model_test_7$fitted.values, test$y)
acc_test <- sum(diag(conf_mat_test)) / sum(conf_mat_test)
miss_test <- 1-acc_test
miss_test

# 1.5                                                     ####

cross_entropy <- function(k){
  model_kknn_valid <-
    kknn(formula = y~.,
         train = train,
         test = valid,
         kernel = "rectangular",
         k=k)
  
  y <- as.integer(valid$y)
  
  prob <- c()
  for(i in 1:length(y)){
    prob[i] <- model_kknn_valid$prob[i, y[i]]
  }
  
  value <- -sum(log(prob + 1e-15))
  return(value)
}

results <- c()
for(i in 1:30){
  model <- cross_entropy(i)
  results[i] <- model
}
plot(results)
which(min(results) == results)

# Cross entropy is the log likelihood estimation of the models?

# Assignment 2                                            ####
# 2.1                                                     ####
library(caret)
data <- read.csv("parkinsons.csv")
# subject, age, sex, test_time, and total_UPDRS is not used
data <- data[, -c(1, 2, 3, 4, 6)]
set.seed(12345) 
n=nrow(data)
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]


# Sex is a dummy variable and scaling is not done for sex. 
scaler <- preProcess(train)
trainS <- predict(scaler, train)
testS  <- predict(scaler, train)

# 2.2                                                     ####
# Fit the model without intercept
lm_train <- lm(motor_UPDRS~. -1, trainS)
MSE_train <- sum((trainS$motor_UPDRS - lm_train$fitted.values)^2)/nrow(trainS)

pred_test <- predict(lm_train, testS[,-1])
MSE_test <- sum((testS$motor_UPDRS - pred_test)^2)/nrow(testS)


# 2.3a                                                    ####
loglikelihood <- function(theta, sigma){
  n <- nrow(trainS)
  x <- trainS[, -1]
  value <- -(n/2) * log(2*pi) - (n/2) * log(sigma^2) - (1/(2*sigma^2))*sum((x-theta)^2)
  return(value)
}
loglikelihood(lm_train$coefficients, 1)


# 2.3b                                                    ####
ridge <- function(theta, sigma, lambda){
  value <- loglikelihood(theta, sigma)
  value <- value + sum((lambda*theta)^2)
  return(value)
}


# 2.3c                                                    ####
# Need to derivate log likelihood function wrt to theta and sigma??
gradient <- function(){}

ridge_opt <- function(lambda){}


# 2.3d                                                    ####

# Source https://online.stat.psu.edu/stat508/lesson/5/5.1
df <- function(lambda){
  X <- as.matrix(trainS[,-1])
  value <- sum(diag(X %*% solve(t(X) %*% X + lambda*diag(ncol(X)) ) %*% t(X)))
  return(value)
}
df(0) # Should be same as p, number of parameters
df(22) # Should be less than p.

# Assignment 3                                            ####
data <- read.csv("pima-indians-diabetes.csv", header = FALSE)
library(ggplot2)
data$V9 <- as.factor(data$V9)

# 3.1                                                     ####
ggplot(data) +
  geom_point(aes(x=V8, y=V2, color=V9)) +
  theme_bw() +
  labs(x="Age",
       y="Plasma glucose concentration",
       color="Diabetes") +
  scale_color_discrete(labels=c('No', 'Yes'))
# 3.2                                                     ####
model <- glm(V9 ~ V2 + V8, family = binomial, data=data)
pred_data <- predict(model, new_data = data[, c(2,8)], type="response")
pred_data <- data.frame(pred_data)
pred_data$pred <- ifelse(pred_data$pred_data>=0.5, 2, 1)
pred_data$actual <- as.numeric(data$V9)

miss_class <- (nrow(data) - sum(pred_data$pred == pred_data$actual))/nrow(data)

plot_data <- cbind(data, pred=as.factor(pred_data$pred))

ggplot(plot_data) +
  geom_point(aes(x=V8, y=V2, color=pred)) +
  theme_bw() + 
  labs(x="Age",
       y="Plasma glucose concentration",
       color="Prediction of diabetes") +
  scale_color_discrete(labels=c('No', 'Yes'))

# To solve                                                ####
# 2.3c
# 2.4
# All 3
