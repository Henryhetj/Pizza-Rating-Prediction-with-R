#######load datasets and remove redundant columns and rows#########

library(readr)
pizza_barstool <- read_csv("C:/Users/Henry/Desktop/pizza/pizza_barstool.csv")
pizza1 <- subset(pizza_barstool, select = -c(2,5)) #(10,14:22)
pizza_datafiniti <- read_csv("C:/Users/Henry/Desktop/pizza/pizza_datafiniti.csv")
pizza2 <- unique(pizza_datafiniti[ ,1:10])
pizza2 <- subset(pizza2, select = -c(2,4) )
pizza_jared <- read_csv("C:/Users/Henry/Desktop/pizza/pizza_jared.csv")
pizza3 <- subset(pizza_jared, select = -c(1,4,5) )

############################pizza1#########################

#find and fill in missing values
sapply(pizza1, function(x) sum(is.na(x)))
which(is.na(pizza1$latitude))
pizza1[6,4]<-40.68060
pizza1[6,5]<--73.57818
pizza1[266,4] <- 40.72277
pizza1[266,5] <- -73.99620

#remove first two columns and lon/lat just keep zip code(correlative)
pizza1_no2 <- subset(pizza1, select = -c(1,2,4,5,19)) #19 is ? in correlation matrix

#split train and test here to avoid test leakage
per=0.55
train<-sample(nrow(pizza1_no2),per*nrow(pizza1_no2))
pizza1_no2.train<-pizza1_no2[train,]
pizza1_no2.test<-pizza1_no2[-train,]

#filter out X for pizza1 using correlation matrix
library("corrplot")
b<-cor(pizza1_no2.train)
corrplot(b,method="color",addCoef.col="grey")

#remove high correlative attributes
pizza1_no2.train <- subset(pizza1_no2.train, select = -c(6,8:10,12:15) )
pizza1_no2.train <- pizza1_no2.train[c(1:4,6,7,5)]

#remove more attributes with stepwise regression
library('MASS')
fit <- lm(review_stats_all_average_score~.,data=pizza1_no2.train) #y~all others x
stepAIC(fit,direction='backward')
pizza1_ftrain <- pizza1_no2.train[c(1:3,5,7)]


#########fit the model:

#option1 normal linear fitting
model1 <- lm(review_stats_all_average_score~.,data=pizza1_ftrain)

#option2: using k fold to increase the samples to train the model
library('caret')
model2 <- train(
  review_stats_all_average_score~.,
  data=pizza1_ftrain,
  method = "nls",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE
  )
)

#option3: nonlinear multivariant regression
model3 <- nls(review_stats_all_average_score~exp(a*price_level+b/provider_rating+c*review_stats_all_total_score+d*zip),
              data=pizza1_ftrain, start=c(a=0,b=0,c=0,d=0))

#evaluate the model
par(mfrow=c(2,2))
plot(model3)

#test the datasets
pizza1_ftest <- subset(pizza1_no2.test, select = c(1:3,7))
pizza1_no2.test$prediction <- predict(model3,pizza1_ftest)

#evaluate the test prediction result
library(dismo)
evaluate(pizza1_no2.test$review_stats_all_average_score, pizza1_no2.test$prediction)

#model3 is best with AUC 0.75
#seems too many samples(overfitting), 0.75-0.55, AUC increased to 0.75 , looking for better model

#####################pizza2###########################

#Studying the Hypothesis that there is a relationship between geography variables(latitude, longitude) and PriceRange(Max and Min)
#Null Hypothesis  H0:No relationship between geography variables and Price Range variables.
#Alternate Hypothesis  HA:There exists a relationship between geography variables and Price Range variables.

Geo_data <- subset(pizza2, select = c(7,8,4,5) )

#Relationship between every pair of variables
pairs(Geo_data, labels = colnames(Geo_data), main = "Pairs matrix", pch = 21,
      bg = c("red", "green3", "blue"), upper.panel = NULL)

#split train and test data
set.seed(1)
num_obs <- nrow(Geo_data)
train_index <-sample(num_obs, size = trunc(0.50 * num_obs))
train_data <- Geo_data[train_index, ]
test_data <-Geo_data[-train_index, ]

# Select all predictors with degree one for predicting Maxprice
RMSE <- function(pred, actual, narm = False){
  sqrt(mean((pred - actual) ^ 2, na.rm = narm))}
full_model <- lm(price_range_max ~ ., data = train_data)
summary(full_model)

# test RMSE
findRMSE <- function(model){
  cat("Train RMSE = ", RMSE(train_data$price_range_max, model$fitted.values, narm = TRUE), "\n")
  cat("Test RMSE = ", RMSE(test_data$price_range_max, predict(model, test_data),  narm = TRUE))}
findRMSE(full_model)

#########################pizza3########################

#rearrange the columns to show y at last
pizza3 <- pizza3[,c(3,4,5,2,6,1)]
colnames(pizza3)[colnames(pizza3)=="place"] <- "name"

#combine same rows using SQL
library('sqldf')
pizza3_sql <- sqldf('SELECT name,sum(total_votes) as t_v,sum(votes) as v,answer FROM pizza3 GROUP BY name,answer')

#optimize the dataset
pizza3_sql[226,3]<-8
pizza3_sql[230,2]<-25
pizza3_sql <- pizza3_sql[-c(228),] #has 'Fair'

# convert level to weighted scores
pizza3_sql$score <- c(rep(0,280))
pizza3_sql$score[which(pizza3_sql$answer=="Excellent")] <- '4'
pizza3_sql$score[which(pizza3_sql$answer=="Good")] <- '3'
pizza3_sql$score[which(pizza3_sql$answer=="Average")] <- '2'
pizza3_sql$score[which(pizza3_sql$answer=="Poor")] <- '1'
pizza3_sql$score[which(pizza3_sql$answer=="Never Again")] <- '0'
pizza3_sql$score <- as.numeric(pizza3_sql$score)
pizza3_sql$ws <- pizza3_sql$v*pizza3_sql$score

pizza3_sql$fin<-c(rep(0,280)) #fill all with 0
sequence <- seq(1, nrow(pizza3_sql), by=5)

for(i in sequence) {
  pizza3_sql$fin[i] <- (pizza3_sql$ws[i]+pizza3_sql$ws[i+1]+pizza3_sql$ws[i+2]+pizza3_sql$ws[i+3]+pizza3_sql$ws[i+4])/pizza3_sql$t_v[i]
}

# trim dataset
pizza3_fin <- pizza3_sql[,c(1,2,7)]
pizza3_fin <- pizza3_fin[sequence, ]

#check if there is any NA value and fill in the missing value
sapply(pizza3_fin, function(x) sum(is.na(x)))
which(is.na(pizza3_fin$fin))
pizza3_fin[9,3]<-0

#natural join
pizza32 <- merge(x = pizza3_fin, y = pizza2, by = "name") # just 9 obs, give it up
pizza31 <- merge(x = pizza3_fin, y = pizza1, by = "name")
pizza31_fin<-subset(pizza31,select=c(1,5,9,13,2,3,11,8))
pizza31_fin$price_level <- as.character(pizza31_fin$price_level)

#check the correlation
c<-cor(pizza31_fin[,-c(1)])
corrplot(c,method="color",addCoef.col="grey")

#split the train test datasets
library(caTools)
set.seed(123)
split = sample.split(pizza31_fin$price_level, SplitRatio = 0.75)
training_set = subset(pizza31_fin, split == TRUE)
test_set = subset(pizza31_fin, split == FALSE)

#scaling the data
training_set[,2:7] = scale(training_set[,2:7])
test_set[,2:7] = scale(test_set[,2:7])

#svm without k fold
library(e1071)
classifier = svm(formula =price_level ~ .,
                 data = training_set[,2:8],
                 type = 'C-classification',
                 kernel = 'radial')

y_pred = predict(classifier, newdata = test_set[,2:8])
cm = table(test_set[,8], y_pred)
cm

#Parallel Random Forest with repeated cv is best acc0.895
library(caret)
# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(price_level~., data=training_set, trControl=train_control, method="parRF")
# summarize results
print(model)
