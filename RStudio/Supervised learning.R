# Name: Katlego Mohale


library(caret)
library(readr)
library(e1071)


# Data #
bank <- read_csv("bank_data.csv")
candy <- read_csv("candy-data.csv")

stars <- read_csv("stars.csv")
cluster <- read_csv("cluster.csv")


#Question 1

bank$y <- ifelse(bank$y == "yes", 1, 0)
#yes is a one(1), no is a zero(0)

set.seed(31)
split <- round(nrow(bank)*0.50)
train_ind <- sample(1:nrow(bank),
                    split,
                    replace = FALSE)
trainData <- bank[train_ind,]
testData <- bank[-train_ind,]

logistic_mod <- glm(y ~ ., data = trainData, family = "binomial")
summary(logistic_mod)

pred <- predict(logistic_mod, newdata = testData, type = "response")

y_pred_num <- ifelse(pred > 0.6, 1, 0)
y_pred <- factor(y_pred_num)
y_act <- factor(testData$y)

confusionMatrix(data = y_pred, reference = y_act, positive = "1")

q1_a <- 1925
q1_b <- 308
q1_c <- 715
q1_d <- 2341

q1_accuracy <- 0.8066
q1_precision <- 0.8621
q1_sensitivity <- 0.7292
q1_error <- 0.1934


#Question 2

candy$rankpercent <- ifelse(candy$rankpercent > 50, 1, 0)
#one(1) is popular,zero(0) is unpopular

set.seed(21)
cSplit <- round(nrow(candy)*0.65)
cTrain_ind <- sample(1:nrow(candy),
                    cSplit,
                    replace = FALSE)
cTrainData <- candy[cTrain_ind,]
cTestData <- candy[-cTrain_ind,]

cLogistic_mod <- glm(rankpercent ~ ., data = cTrainData, family = "binomial")
summary(cLogistic_mod)

cPred <- predict(cLogistic_mod, newdata = cTestData, type = "response")

y_cpred_num <- ifelse(cPred > 0.5, 1, 0)
y_cpred <- factor(y_cpred_num)
y_cact <- factor(cTestData$rankpercent)

confusionMatrix(data = y_cpred, reference = y_cact, positive = "1")

q2_a <- 6
q2_b <- 7
q2_c <- 7
q2_d <- 10

q2_accuracy <- 0.5333
q2_precision <- 0.4615
q2_sensitivity <- 0.4615
q2_error <- 0.4667


#Question 3

ggplot(stars,mapping=aes(x=`Luminosity(L/Lo)`, y =`Radius(R/Ro)` )) + geom_point()
q3 <- 3


#Question 4

set.seed(72)

q4a <- kmeans(cluster, centers = 2, nstart = 20)
q4b <- kmeans(cluster, centers = 3, nstart = 20)
q4c <- kmeans(cluster, centers = 4, nstart = 20)
q4d <- kmeans(cluster, centers = 5, nstart = 20)

q4clust <- 4
