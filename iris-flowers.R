# Load the required library
library(caret)

# Load the iris dataset
data(iris)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[trainIndex, ]
test_data <- iris[-trainIndex, ]

# Define the training control
ctrl <- trainControl(method = "cv", number = 5)

# Train a classification model using caret
model <- train(Species ~ ., data = train_data, method = "rf", trControl = ctrl)

# Print the model results
print(model)

# Make predictions on the testing set
predictions <- predict(model, newdata = test_data)

# Evaluate the model performance
confusionMatrix(predictions, test_data$Species)
