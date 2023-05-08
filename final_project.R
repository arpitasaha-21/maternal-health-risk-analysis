dataset<- read.csv("D:/FALL 2022-23/INTRODUCTION TO DATA SCIENCE/PROJECT/Maternal Health Risk Data Set.csv",
                   header = TRUE,sep = ",")
dataset


nrow(dataset)
ncol(dataset)
dim(dataset)
length(dataset)
names(dataset)
str(dataset)


normalize_data <- function(x) {
  ((x - min(x)) / (max(x) - min(x))) } 
norm_data1 <- as.data.frame(lapply(dataset[,1:6], normalize_data))
norm_data1


set.seed(123)

sample_set <- sample(c(TRUE, FALSE), nrow(norm_data1), replace=TRUE, prob=c(0.7,0.3))
train  <- norm_data1[sample_set, ]
test   <- norm_data1[!sample_set, ]

train_labels <- dataset[sample_set,7]


test_labels <-  dataset[!sample_set,7]


install.packages("class")
library(class)

length(train_labels)

model<-knn(train=train,test=test,cl=train_labels,k=27)
model

ACC <- 100 * sum(test_labels == model)/NROW(test_labels)
ACC

table(model,test_labels)

