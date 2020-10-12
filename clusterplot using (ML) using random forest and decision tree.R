#Prediction using decision tree algorithm 

#install.packages("rpart.plot")
#install.packages("dplyr")
#install.packages("partykit")

#reading and reviewing the data
data=read.table('E:/dload/Iris.csv', sep=",", header=TRUE)
head(data)
tail(data)

#shuffling the data
shuffle_index <- sample(1:nrow(data))
head(shuffle_index)
data= data[shuffle_index, ]
head(data)


#removing the ID column from the data and converting the species column to a factor
library(dplyr)
# Drop variables
clean_data <- data %>%
  select(-Id) %>% mutate(Species = factor(Species, levels = c('Iris-setosa','Iris-versicolor','Iris-virginica'), labels = c('Setosa','Versicolor','Virginica')))
  
glimpse(clean_data)

#splitting the data into training and testing set 
splitf <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- splitf(clean_data, 0.8, TRUE)
data_test <- splitf(clean_data, 0.8, FALSE)
dim(data_train)
dim(data_test)

#checking if randomization is correct
prop.table(table(data_train$Species))
prop.table(table(data_test$Species))


#viewing the training model
library(rpart.plot)
fit <- rpart(Species~., data=data_train, method='class')
rpart.plot(fit, extra=104, shadow.col="darkgray")

#prediction using training data
library(partykit)
myform= Species ~ SepalLengthCm + SepalWidthCm+ PetalLengthCm + PetalWidthCm
Iris_ctree <- ctree(myform,data=data_train)
table(predict(Iris_ctree),data_train$Species)
print(Iris_ctree)
plot(Iris_ctree, type="simple")


#prediction in reference to the test model

predict_unseen <-predict(Iris_ctree, data_test)
table(data_test$Species, predict_unseen)



#testing the accuracy of the trained model
 accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test




#THANK YOU
#CODE BY:
#CHITRANG KUREEL
