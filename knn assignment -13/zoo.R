zoo <- read.csv(file ="C:\\Users\\Sony\\Downloads\\knn assignment -13\\Zoo.csv" )

View(zoo)
 

table(zoo$type)

zoo$type <- as.factor(zoo$type)

str(zoo)
#Create a function to normalize the data
normalize <- function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}
#Apply the normalization function to the dataset
Zoo_n <- as.data.frame(lapply(zoo[2:17], scale))

Zoo_n$type <- zoo$type

set.seed(12)

sam <- sample(x = 2,size = nrow(Zoo_n),replace = TRUE,prob = c(0.7,0.3))

Zoo_train_dummy <- Zoo_n[sam==1,]

Zoo_test_dummy <- Zoo_n[sam==2,]

Zoo_train <- Zoo_train_dummy[,-17]

Zoo_test <- Zoo_test_dummy[,-17]

Zoo_labels_train <- Zoo_train_dummy$type

Zoo_labels_test <- Zoo_test_dummy$type

library(class)

zoo_pred <- knn(train = Zoo_train,test = Zoo_test,cl = Zoo_labels_train,k = 9 )

library(gmodels)

tree <- CrossTable(x = Zoo_labels_test,y = zoo_pred,prop.chisq = FALSE)


t <- table(Zoo_labels_test,zoo_pred)

t

 

