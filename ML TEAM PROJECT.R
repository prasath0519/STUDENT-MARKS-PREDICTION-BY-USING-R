install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(caTools)

# Simulate some data
set.seed(123)
data <- data.frame(
  Hours = runif(100, min = 1, max = 10),
  Scores = runif(100, min = 50, max = 100)
)

# Scatter plot of the data
ggplot(data, aes(x = Hours, y = Scores)) +
  geom_point() +
  labs(title = "Scatter plot of Hours Studied vs Scores", x = "Hours Studied", y = "Scores")

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(data$Scores, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Fit a linear regression model
model <- lm(Scores ~ Hours, data = train_data)
summary(model)

# Predict scores on the test set
predictions <- predict(model, test_data)

# Combine actual and predicted scores
results <- data.frame(
  Actual = test_data$Scores,
  Predicted = predictions
)

# Plot actual vs predicted scores
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Scores", x = "Actual Scores", y = "Predicted Scores")

# Calculate the Mean Absolute Error (MAE)
mae <- mean(abs(results$Actual - results$Predicted))
cat("Mean Absolute Error:", mae, "\n")

# Save the model
saveRDS(model, file = "student_score_model.rds")