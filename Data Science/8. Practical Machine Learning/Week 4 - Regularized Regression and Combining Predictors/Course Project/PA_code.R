# Load libraries
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)

# Set seed for research reproduceability
set.seed(2017)

# Download files if a local copy doesn't exist
path <- getwd()
destfile1 <- paste(path, "pml-training.csv", sep = "/")
destfile2 <- paste(path, "pml-testing.csv", sep = "/")

url_train_set <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test_set <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if (!file.exists(destfile1)) {
     download.file(url = url_train_set, destfile = destfile1)
}

if (!file.exists(destfile2)) {
     download.file(url = url_test_set, destfile = destfile2)
}

# Read data
complete_training_set <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
complete_testing_set <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))

# Delete some variables which are irrelevant to our current project:
# user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window (columns 1 to 7)
complete_training_set <- complete_training_set[, -c(1:7)]
complete_testing_set <- complete_testing_set[, -c(1:7)]

# Create the partitions of the training data set into two data sets, 60% for training, 40% for testing
index_training <- createDataPartition(y = complete_training_set$classe, p = 0.6, list = FALSE)
training <- complete_training_set[index_training, ]
testing <- complete_training_set[-index_training, ]

# Delete columns with all missing values
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

dim(training)
head(training)
dim(testing)
head(testing)

# Remove first ID variable so that it does not interfere with ML algorithms
training <- training[c(-1)]

# Use ML algorithms for prediction: Dlassification Tree
model_fit_decission_trees <- rpart(classe ~ ., data = training, method = "class")
rpart.plot(model_fit_decission_trees, main = "Classification Tree", extra = 102, under = TRUE, faclen = 0)

# Prediction
predictions_decission_trees <- predict(model_fit_decission_trees, testing, type = "class")

# Use confusion matrix to test results
confusionMatrix(predictions_decission_trees, testing$classe)      

# Use ML algorithms for prediction: Random Forests
model_fit_random_forest <- randomForest(classe ~ ., data = training)

# Prediction
predictions_random_forest <- predict(model_fit_random_forest, testing, type = "class")

# Use confusion matrix to test results
confusionMatrix(predictions_random_forest, testing$classe)

# 53 is the "classe" column
final_testing <- complete_testing_set[, colnames(testing[ , -c(53)])]
predictions_complete_testing <- predict(model_fit_random_forest, final_testing, type = "class")
predictions_complete_testing

# Function to generate files with predictions
pml_write_files = function(x) {
     n = length(x)
     for(i in 1 : n) {
          filename = paste0("prediction_for_case_", i, ".txt")
          write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
     }
}

pml_write_files(predictions_complete_testing)