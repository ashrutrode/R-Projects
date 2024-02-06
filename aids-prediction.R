# Load the required libraries
library(caret)
library(dplyr)

# Load the MASS package
library(MASS)

# Load the Aids2 dataset
data(Aids2, package = "MASS")

# Get rid of the following columns
Aids <- subset(Aids2, select = -c(state, sex, diag, death))

# Send the first column, status, to the end
Aids <- Aids %>%
  select(-status, everything())

# View the new dataset
View(Aids)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(Aids$status, p = 0.7, list = FALSE)
train_data <- Aids[trainIndex, ]
test_data <- Aids[-trainIndex, ]

# Define the training control
ctrl <- trainControl(method = "cv", number = 5)

# Train a classification model using caret
model <- train(status ~ ., data = train_data, method = "rf", trControl = ctrl)

# Print the model results
print(model)

# Make predictions on the testing set
predictions <- predict(model, newdata = test_data)

# Evaluate the model performance
confusionMatrix(predictions, test_data$status)
