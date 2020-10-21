glass <- read.csv(file ="C:\\Users\\Sony\\Downloads\\knn assignment -13\\glass.csv" )

View(glass)

str(glass)

table(glass$Type)

glass$Type <- as.factor(glass$Type)

str(glass)
#Create a function to normalize the data
normalize <- function(x) 
  {
  
  return ((x - min(x)) / (max(x) - min(x)))
}
#Apply the normalization function to the dataset
glass_n <- as.data.frame(lapply(glass[1:9], normalize))

glass_n$Type <-glass$Type 

nrow(glass_n)

set.seed(123)

sam <- sample(2,nrow(glass_n),replace = TRUE,prob = c(0.7,0.3))

glass_train_dummy<- glass_n[sam==1,]

glass_test_dummy <- glass_n[sam==2,]

glass_train <- glass_train_dummy[,-10]

glass_test <- glass_test_dummy[,-10]

glass_labels_train <- glass_train_dummy$Type

glass_labels_test <- glass_test_dummy$Type

library(class)
# Building the KNN model on training dataset 
glass_pred <- knn(train=glass_train,test = glass_test,cl = glass_labels_train,k = 11)

library(gmodels)

c <- CrossTable(x = glass_labels_test,y = glass_pred,prop.chisq = FALSE)


table(glass_test_dummy$Type)

t <- table(glass_labels_test,glass_pred)

t

 
