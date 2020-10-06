# Simple Linear Regression 
# Importing the dataset 
library(readxl)
Book1 <- read_excel("C:/Users/acer/Desktop/Book1.xlsx")
View(Book1)

# Splitting the dataset into the 
# Training set and Test set 

library(caTools) 
split = sample.split(dataset$Scores, SplitRatio = 0.8) 
trainingset = subset(dataset, split == TRUE) 
testset = subset(dataset, split == FALSE) 
trainingset #stores 20 random values from the dataset

# Fitting Simple Linear Regression to the Training set 
lm.r= lm(formula = Scores ~ Hours, 
         data = trainingset) 
coef(lm.r) 

# Predicting the Test set results based on training set linear model
ypred = predict(lm.r, newdata = testset) 

library(ggplot2) 

# Visualising the Training set results 
ggplot() + geom_point(aes(x = trainingset$Hours, 
                          y = trainingset$Scores), colour = 'red') +
  geom_line(aes(x = trainingset$Hours, 
                y = predict(lm.r, newdata = trainingset)), colour = 'blue') +
  
  ggtitle('Scores vs Hours Studied (Training set)') +
  xlab('Hours Studied') +
  ylab('Scores') 

# Visualising the Test set results 
ggplot() +
  geom_point(aes(x = testset$Hours, y = testset$Scores), 
             colour = 'red') +
  geom_line(aes(x = trainingset$Hours, 
                y = predict(lm.r, newdata = trainingset)), 
            colour = 'blue') +
  ggtitle('Scores vs Hours Studied (Test set)') +
  xlab('Hours Studied') +
  ylab('Scores') 

#comparing the actual results with the predicted results in testscore

data.frame("Actual"=testset$Scores, "Predicted"=ypred) 

#finding the value of the score obtained VIA ML ALGO
predict(lm.r, newdata = data.frame(Hours = 9.25))

#finding the actual value after studying 9.5 hours
lm.a= lm(formula = Scores ~ Hours, 
         data = dataset) 
predict(lm.a, newdata = data.frame(Hours = 9.25))
