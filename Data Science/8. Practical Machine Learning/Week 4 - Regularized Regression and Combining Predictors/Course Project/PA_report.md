# Practical Machine Learning: Peer Assessment
    
## Background Information 
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement and a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways: 

+ exactly according to the specification (Class A)
+ throwing the elbows to the front (Class B)
+ lifting the dumbbell only halfway (Class C)
+ lowering the dumbbell only halfway (Class D)
+ throwing the hips to the front (Class E)

The assignment is to construct a model out of the data that allows for the prediction of the exercise class.

The the link of **source code**: [click to check](https://github.com/theodoreguo/Coursera/blob/master/Data%20Science/8.%20Practical%20Machine%20Learning/Week%204%20-%20Regularized%20Regression%20and%20Combining%20Predictors/Course%20Project/PA_code.R)

## Data Processing
### Import Data
When importing the data, "NA", "#DIV/0!", and "" are recognized as missing values.

```
complete_training_set <- read.csv('pml-training.csv', na.strings = c("NA", "#DIV/0!", ""))
complete_testing_set <- read.csv('pml-testing.csv', na.strings = c("NA", "#DIV/0!", ""))
```
### Data Cleaning
Delete some variables which are irrelevant to our current project: user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window (columns 1 to 7).

```
complete_training_set <- complete_training_set[, -c(1:7)]
complete_testing_set <- complete_testing_set[, -c(1:7)]
```

### Data Partition
Create the partitions of the training data set into two data sets, 60% for training, 40% for testing.

```
index_training <- createDataPartition(y = complete_training_set$classe, p = 0.6, list = FALSE)
training <- complete_training_set[index_training, ]
testing <- complete_training_set[-index_training, ]
```

After that, delete columns with all missing values.

```
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]
```

## Prediction Models
In this project we apply two different methods to do the predition. They are classification tree and random forests. 

### Classification Tree

```
# Use ML algorithms for prediction: Dlassification Tree
model_fit_decission_trees <- rpart(classe ~ ., data = training, method = "class")
rpart.plot(model_fit_decission_trees, main = "Classification Tree", extra = 102, under = TRUE, faclen = 0)

# Prediction
predictions_decission_trees <- predict(model_fit_decission_trees, testing, type = "class")

# Use confusion matrix to test results:
confusionMatrix(predictions_decission_trees, testing$classe)   
```

The results are as follows:

```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1957  212   28   37   37
         B   83  828   73   98  133
         C   47  112  985  197  228
         D  110  237  194  874  171
         E   35  129   88   80  873

Overall Statistics
                                          
               Accuracy : 0.7032          
                 95% CI : (0.6929, 0.7133)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.625           
 Mcnemar's Test P-Value : < 2.2e-16       

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.8768   0.5455   0.7200   0.6796   0.6054
Specificity            0.9441   0.9388   0.9098   0.8915   0.9482
Pos Pred Value         0.8617   0.6815   0.6278   0.5511   0.7245
Neg Pred Value         0.9507   0.8959   0.9390   0.9342   0.9143
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2494   0.1055   0.1255   0.1114   0.1113
Detection Prevalence   0.2894   0.1549   0.2000   0.2021   0.1536
Balanced Accuracy      0.9104   0.7421   0.8149   0.7855   0.7768
```

From the confusion matrix, the accuracy is 0.7032 which is not good enough.

### Random Forests

```
# Use ML algorithms for prediction: Random Forests
model_fit_random_forest <- randomForest(classe ~ ., data = training)

# Prediction
predictions_random_forest <- predict(model_fit_random_forest, testing, type = "class")

# Use confusion matrix to test results
confusionMatrix(predictions_random_forest, testing$classe)
```

The results are as follows:

```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2229   12    0    0    0
         B    3 1504    6    0    0
         C    0    2 1362   22    1
         D    0    0    0 1263    0
         E    0    0    0    1 1441

Overall Statistics
                                         
               Accuracy : 0.994          
                 95% CI : (0.992, 0.9956)
    No Information Rate : 0.2845         
    P-Value [Acc > NIR] : < 2.2e-16      
                                         
                  Kappa : 0.9924         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9987   0.9908   0.9956   0.9821   0.9993
Specificity            0.9979   0.9986   0.9961   1.0000   0.9998
Pos Pred Value         0.9946   0.9941   0.9820   1.0000   0.9993
Neg Pred Value         0.9995   0.9978   0.9991   0.9965   0.9998
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2841   0.1917   0.1736   0.1610   0.1837
Detection Prevalence   0.2856   0.1928   0.1768   0.1610   0.1838
Balanced Accuracy      0.9983   0.9947   0.9959   0.9911   0.9996
```

The accuracy is 0.994.

## Final Prediction 
Among the above two algorithms applied, random forests has the decent accuracy. Therefore, we use random forest to do the prediction.

```
final_testing <- complete_testing_set[, colnames(testing[ , -c(53)])]
predictions_complete_testing <- predict(model_fit_random_forest, final_testing, type = "class")
predictions_complete_testing
```
```
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E
```
