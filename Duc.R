# Assignment 1                                            ####
install.packages("kknn")
install.packages("dplyr")
install.packages("ggplot2")
# 1a                                                      ####
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

# 1b                                                      ####
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

# 1c                                                      ####
y <- train$y 
fit_y <- model_kknn_train$fitted.values
# probabilities given from 0 to 9, index 9 = number 8.
prob_8 <- model_kknn_train$prob[, 9]

data_8 <- data.frame(y = y,
                     fit_y = fit_y,
                     prob = prob_8)

data_8 <- data_8[data_8$y == "8", ]

plot_8 <- function(index){
  plot <- as.matrix(train[index, -65])
  plot <- matrix(plot, nrow=8, byrow=TRUE)
  heatmap(plot, Colv=NA, Rowv=NA)
}


# Hardest to correctly predict: 1624, 1663, and 229
# Easiest to correctly predict: 1864 and 1811
plot_8(1624)
plot_8(1663)
plot_8(229)
plot_8(1864)
plot_8(1811)

# 1d                                                      ####

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



