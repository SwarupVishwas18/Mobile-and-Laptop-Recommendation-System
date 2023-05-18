# Load the dataset into R
laptops <- read.csv("laptops.csv")

# Preprocess the dataset
laptops$processor <- as.factor(laptops$processor)
laptops$os <- as.factor(laptops$os)

laptops <- laptops[!is.na(laptops$rating), ]
laptops <- laptops[complete.cases(laptops), ]
laptops$ram <- as.integer(gsub("([A-Za-z]+).*", "\\", lap$ram))
empty_cols <- which(colSums(is.na(data)) == nrow(data))

# Remove empty columns
if(length(empty_cols) > 0) {
  data <- data[, -empty_cols]
}
# Split the dataset into training and testing sets
library(caret)
set.seed(123)
train_index <- createDataPartition(laptops$rating, p = 0.8, list = FALSE)
train_data <- laptops[train_index, ]
train_data <-  train_data[complete.cases(train_data), ]
test_data <- laptops[-train_index, ]



# Train a linear regression model
model <- lm(rating ~ price + ram , data = train_data)

# Generate predictions for the testing data
predictions <- predict(model, newdata = test_data)

# Evaluate the accuracy of the model
library(Metrics)
rmse <- rmse(predictions, test_data$rating)
mae <- mae(predictions, test_data$rating)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
