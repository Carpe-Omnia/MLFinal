# Loading needed libraries
library(caret)
library(randomForest)

# Loading training data
if(!file.exists("pml-training.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                destfile = "pml-training.csv")
}
training_data <- read.csv("pml-training.csv", na.strings = c("NA", ""))

# Loading testing data
if(!file.exists("pml-testing.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                destfile = "pml-testing.csv")
}
validation_data <- read.csv("pml-testing.csv")

# Setting seed for reproducibility
set.seed(1234)

# Splitting training data into training and testing sets
inTrain <- createDataPartition(y = training_data$classe, p = 0.7, list = FALSE)
training_set <- training_data[inTrain, ]
testing_set <- training_data[-inTrain, ]

# Convert 'classe' to factor
training_set$classe <- as.factor(training_set$classe) ; testing_set$classe  <- as.factor(testing_set$classe)

# Identify columns with missing values in training set
na_cols <- sapply(training_set, function(x) {sum(is.na(x))})
cols_with_na <- names(na_cols[na_cols > 0])

# Remove columns with missing values from all sets
training_set <- training_set[, !names(training_set) %in% cols_with_na]
testing_set <- testing_set[, !names(testing_set) %in% cols_with_na]
validation_data <- validation_data[, !names(validation_data) %in% cols_with_na]

# Remove unnecessary columns from all sets
unnecessary_cols <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
training_set <- training_set[, !names(training_set) %in% unnecessary_cols]
testing_set <- testing_set[, !names(testing_set) %in% unnecessary_cols]
validation_data <- validation_data[, !names(validation_data) %in% unnecessary_cols]

# Check structure of target variable 'classe'
str(training_set$classe)

# Train Random Forest model
model <- randomForest(classe ~ ., data = training_set, ntree = 100)

# Making predictions on testing set
predictions_testing <- predict(model, testing_set)

# Evaluate model on testing set
confusionMatrix_testing <- confusionMatrix(predictions_testing, testing_set$classe)
modelAccuracy <- confusionMatrix_testing$overall[[1]]
cat("Model Accuracy on Testing Set:", modelAccuracy, "\n")

# Making predictions on validation set
predictions_validation <- predict(model, validation_data)

# Write predictions to CSV file
write.csv(predictions_validation, file = "quiz.csv", row.names = FALSE)

# Print predictions for validation set
cat("Predictions for Validation Set:\n")
print(predictions_validation)
