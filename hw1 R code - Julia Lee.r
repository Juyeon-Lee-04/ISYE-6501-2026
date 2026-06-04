
## KSVM
data <- file.choose(new = FALSE)

my_data <- read.table(data,header = TRUE)
my_data
install.packages("kernlab")
library(kernlab)

print(my_data)
x_data <- as.matrix(my_data[,1:10])
y_data <- my_data[,11]
y_vector = as.factor(y_data)

model <- ksvm(x=x_data, y=y_vector, kernel="vanilladot", C=1)
model
model2 <- ksvm(x=x_data, y=y_vector, kernel="vanilladot", C=100)
model3 <- ksvm(x=x_data, y=y_vector, kernel="vanilladot", C= 1000)
model4 <- ksvm(x=x_data, y=y_vector, kernel="vanilladot", C= 500)
model3 <- ksvm(x=x_data, y=y_vector, kernel="vanilladot", C= 0.001) 
model <-  ksvm(x=x_data, y=y_vector, kernel="vanilladot", C= 0.01) 
predictions <- predict(model, x_data)
print(predictions)

confusion_table <- table(Actual = y_vector, Predicted = predictions)
print(confusion_table)

weights <- colSums(alpha(model)[[1]] * xmatrix(model)[[1]])
equation_coefficients <- data.frame(
       Feature = colnames(x_data),
       Weight = weights
   )
equation_coefficients <- data.frame(
       Feature = colnames(x_data),
       Weight = weights
   )

equation_coefficients
b_intercept <- - b(model)
b_intercept
kernels_to_test <- c("vanilladot", "rbfdot", "polydot")
 
for (k in kernels_to_test) {
  
  # Train SVM model
  temp_model <- ksvm(
    x = x_data,
    y = y_vector,
    kernel = k,
    C = 1
  )
  
  # Generate predictions
  preds <- predict(temp_model, x_data)
  
  # Compute training error
  err <- mean(preds != y_vector)
  
  # Print results
  cat(
    "Kernel:",
    sprintf("%-12s", k),
    "| Training Error:",
    round(err, 4),
    "\n"
  )
}


##KKNN

install.packages("kknn")
library(kknn)

x_scaled <- scale(as.matrix(my_data[, 1:10]))
y_response <- as.numeric(as.character(my_data[, 11]))
scaled_df <- data.frame(x_scaled, Target = y_response)


df_my_data <- as.data.frame(my_data)
library(kknn)

x_cols <- 1:10
y_col <- 11
n <- nrow(df_my_data)
k_values <- 1:20
accuracy <- numeric(length(k_values))

accuracy

for (k_idx in seq_along(k_values)) {
  
  k <- k_values[k_idx]
  correct <- 0
  
  for (i in 1:n) {
    
    train_data <- df_my_data[-i, ]
    test_data  <- df_my_data[i, , drop = FALSE]
    
    model <- kknn(
      formula = df_my_data[,y_col] ~ .,
      train = train_data,
      test = test_data,
      k = k,
      scale = TRUE
    )
    
    prob <- fitted(model)
    pred <- ifelse(prob > 0.5, 1, 0)
    
    actual <- df_my_data[i, y_col]
    
    if (pred == actual) {
      correct <- correct + 1
    }
  }
  
  accuracy[k_idx] <- correct / n
}
colnames(df_my_data)[11] <- "target"
df_my_data$target <- as.numeric(as.character(df_my_data$target))
library(kknn)

n <- nrow(df_my_data)
k_values <- 1:20
accuracy <- numeric(length(k_values))


for (k_idx in seq_along(k_values)) {
  
  k <- k_values[k_idx]
  correct <- 0
  
  for (i in 1:n) {
    
    train_data <- df_my_data[-i, ]
    test_data  <- df_my_data[i, , drop = FALSE]
    
    model <- kknn(target ~ ., train_data, test_data, k = k, scale = TRUE)
    
    prob <- fitted(model)
    pred <- ifelse(prob > 0.5, 1, 0)
    
    actual <- df_my_data[i, "target"]
    
    if (pred == actual) {
      correct <- correct+1
    }
  }
  
  accuracy[k_idx] <- correct / n
}
k <- 15
n <- nrow(df_my_data)
preds <- numeric(n)

for (i in 1:n) {
  train_data <- df_my_data[-i, ]
  test_data  <- df_my_data[i, , drop = FALSE]
  
  model <- kknn(target ~ ., train_data, test_data, k = k, scale = TRUE)
  
  prob <- fitted(model)
  preds[i] <- ifelse(prob > 0.5, 1, 0)
}
table(Predicted = preds, Actual = df_my_data$target)


## 3.3 Cross Validation

# Code partially assisted by ChatGPT (OpenAI, 2023).
# OpenAI. (2023). ChatGPT (Mar 14 version) [Large language model]. https://chat.openai.com/

library(caret)
library(kknn)

# Convert target to factor
my_data$V11 <- as.factor(my_data$V11)

# Scale predictors
preProcValues <- preProcess(
  my_data[, 1:10],
  method = c("center", "scale")
)

scaled_x <- predict(
  preProcValues,
  my_data[, 1:10]
)

# Final dataframe
data_scaled <- data.frame(
  scaled_x,
  target = my_data$V11
)

# Cross-validation setup
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

train_control
# Train KNN model with k = 1 to 20
set.seed(1)

knn_model <- train(
  target ~ .,
  data = data_scaled,
  method = "kknn",
  trControl = train_control,
  
  tuneGrid = data.frame(
    kmax = 1:20,
    distance = rep(2, 20),
    kernel = rep("optimal", 20)
  )
)

# Results
print(knn_model)
# Full table
knn_model$results
# Best parameters
knn_model$bestTune
# Best accuracy
max(knn_model$results$Accuracy)

## CV with DATA Split
set.seed(1)

# 70% training
train_index <- createDataPartition(
    my_data$V11,
    p = 0.70,
    list = FALSE
)

train_data <- my_data[train_index, ]
remaining_data <- my_data[-train_index, ]

# Split remaining 30% into 15% validation and 15% test
valid_index <- createDataPartition(
    remaining_data$V11,
    p = 0.50,
    list = FALSE
)

validation_data <- remaining_data[valid_index, ]
test_data <- remaining_data[-valid_index, ]

preProcValues <- preProcess(
  train_data[, 1:10],
  method = c("center", "scale")
)

train_x <- predict(preProcValues, train_data[, 1:10])
valid_x <- predict(preProcValues, validation_data[, 1:10])
test_x  <- predict(preProcValues, test_data[, 1:10])

train_scaled <- data.frame(train_x, V11 = train_data$V11)
valid_scaled <- data.frame(valid_x, V11 = validation_data$V11)
test_scaled  <- data.frame(test_x,  V11 = test_data$V11)

k_values <- 1:20
validation_accuracy <- numeric(length(k_values))


for(k_idx in seq_along(k_values)) {
  
  k <- k_values[k_idx]
  
  model <- kknn(
    V11 ~ .,
    train = train_scaled,
    test = valid_scaled,
    k = k
  )
  
  preds <- fitted(model)
  
  validation_accuracy[k_idx] <-
    mean(preds == valid_scaled$V11)
}
best_k <- k_values[which.max(validation_accuracy)]
best_k

validation_accuracy

final_train <- rbind(train_scaled, valid_scaled)

final_model <- kknn(
  V11 ~ .,
  train = final_train,
  test = test_scaled,
  k = best_k
)

final_model

test_preds <- fitted(final_model)
test_accuracy <- mean(test_preds == test_scaled$V11)

test_accuracy

table(
  Predicted = test_preds,
  Actual = test_scaled$V11
)
