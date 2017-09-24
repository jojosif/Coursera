# Load libraries
library(caret)
library(doMC)
library(e1071)

# Remove any variables from the environment
rm(list = ls())

path <- getwd()
destfile1 <- paste(path, "pml-training.csv", sep = "/")
destfile2 <- paste(path, "pml-testing.csv", sep = "/")

# Data URLs
train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Download files if a local copy doesn't exist
if (!file.exists(destfile1)) {
     download.file(url = train.url, destfile = destfile1)
}

if (!file.exists(destfile2)) {
     download.file(url = train.url, destfile = destfile2)
}

# Read CSV files
train.all <- read.csv('pml-training.csv')
test.all <- read.csv('pml-testing.csv')

rem.col.ids <- grep("X|user_name|cvtd_timestamp|raw_timestamp_part_1|raw_timestamp_part_2", names(train.all))
train.clean <- train.all[, -rem.col.ids]

nzv <- nearZeroVar(train.clean[, -155])
train.clean <- train.clean[, -nzv]

# Some columns have mostly NAs and a little more than 400 rows with values
# Remove those columns where the number is less than the threshold of 623 rows
train.clean <- train.clean[, colSums(is.na(train.clean)) < 19000]

# Set number of available CPU cores to 8
registerDoMC(cores = 8)

# Train the model
model <- train(classe ~ ., data = train.clean, method = "rf", trControl = trainControl(method = "oob"))
save(model, file = "model.RData")

results <- model$results
save(results, file = "results.RData")

# Remove the columns not used in the training data from the test data
test.clean <- test.all[, which(names(test.all) %in% names(train.clean))]

# Predict the answers for the test data
answers <- predict(model, newdata = test.clean)
save(answers, file = "answers.RData")
answers

# Define the function provided to create the answer files and write the answers
pml_write_files = function(x) {
     n = length(x)
     for(i in 1 : n) {
          filename = paste0("prediction_for_case_", i, ".txt")
          write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
     }
}

pml_write_files(answers)