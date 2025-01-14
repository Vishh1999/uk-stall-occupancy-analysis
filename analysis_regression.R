#### Packages Installation and Importing ----
install.packages(c("caret", "pROC", "mlr", "readxl", "dplyr",
                   "class", "e1071", "rpart", "randomForest",
                   "Boruta", "xgboost", "rpart.plot", "ggplot2"))

library(caret)
library(pROC)
library(mlr)
library(readxl)
library(dplyr)
library(class)
library(e1071)
library(rpart)
library(randomForest)
library(Boruta)
library(xgboost)
library(rpart.plot)
library(ggplot2)

#### Data preprocessing ----
# Read the data from the Excel file
data <- read_excel("nmftmarkets.xlsx")

# Overview of the dataset
summary(data)
str(data)

# define condition columns and weekly numeric columns
condition_cols <- c('openmonday_mkv', 'opentuesday_mkv',
                    'openwednesday_mkv', 'openthursday_mkv',
                    'openfriday_mkv', 'opensaturday_mkv', 'opensunday_mkv')

weekly_numeric_cols <- list(
  c('monnostallsavail_mkv', 'monnostallsocc_mkv', 'monnostalltrad_mkv'),
  c('tuenostallsavail_mkv', 'tuenostallsocc_mkv', 'tuenostalltrad_mkv'),
  c('wednostallsavail_mkv', 'wednostallsocc_mkv', 'wednostalltrad_mkv'),
  c('thunostallsavail_mkv', 'thunostallsocc_mkv', 'thunostalltrad_mkv'),
  c('frinostallsavail_mkv', 'frinostallsocc_mkv', 'frinostalltrad_mkv'),
  c('satnostalltrad_mkv'),
  c('sunnostallsavail_mkv', 'sunnostallsocc_mkv', 'sunnostalltrad_mkv')
)

# the weekly numeric columns contain BLANK values when the data is not known even if the market is open
# Perform value counts for all columns in weekly_numeric_cols before cleaning up the BLANK values
# Displaying only 2 groups to showcase but all these columns have BLANK values
for (group in weekly_numeric_cols[1]) {
  lapply(group, function(col) {
    print(col)
    print("/n")
    print(table(data[[col]], useNA = "ifany"))
  })
}

# function to clean and impute blank names
clean_blank_names <- function(data, col, condition_col) {
  # replace values containing 'BLANK' with NA
  data[[col]] <- ifelse(grepl("BLANK", data[[col]], fixed = TRUE), NA, data[[col]])
  mean_value <- mean(as.numeric(data[[col]]), na.rm = TRUE) # Ensure numeric calculations
  # replace NA with mean value only if condition_col equals 1
  data[[col]] <- ifelse(is.na(data[[col]]) & data[[condition_col]] == 1,
                        mean_value,
                        data[[col]])
  return(data)
}

# apply the cleaning function to relevant columns
for (i in seq_along(condition_cols)) {
  condition_col <- condition_cols[i]
  day_numeric_cols <- weekly_numeric_cols[[i]]

  for (col in day_numeric_cols) {
    data <- clean_blank_names(data, col, condition_col)
  }
}

# Perform value counts for all columns in weekly_numeric_cols after cleaning up the BLANK values
for (group in weekly_numeric_cols[1]) {
  lapply(group, function(col) {
    print(col)
    print("\n")
    print(table(data[[col]], useNA = "ifany"))
    print("\n")
  })
}

# calculate average stalls available, occupied and the weekly operational days
data <- data %>%
  mutate(across(c(monnostallsavail_mkv, tuenostallsavail_mkv, wednostallsavail_mkv,
                  thunostallsavail_mkv, frinostallsavail_mkv, sunnostallsavail_mkv,
                  satnostalltrad_mkv,
                  monnostallsocc_mkv, tuenostallsocc_mkv, wednostallsocc_mkv,
                  thunostallsocc_mkv, frinostallsocc_mkv, sunnostallsocc_mkv,
                  satnostalltrad_mkv), ~ as.numeric(.))) %>%
  mutate(
    avg_stalls_available = rowMeans(select(., monnostallsavail_mkv, tuenostallsavail_mkv, wednostallsavail_mkv,
                                           thunostallsavail_mkv, frinostallsavail_mkv, sunnostallsavail_mkv,
                                           satnostalltrad_mkv), na.rm = TRUE),
    avg_stalls_occupied = rowMeans(select(., monnostallsocc_mkv, tuenostallsocc_mkv, wednostallsocc_mkv,
                                          thunostallsocc_mkv, frinostallsocc_mkv, sunnostallsocc_mkv,
                                          satnostalltrad_mkv), na.rm = TRUE),
    weekly_operational_days = rowSums(select(., all_of(condition_cols)), na.rm = TRUE)
  )

# encoding the name_loc variable
data$name_loc_encoded <- as.integer(factor(data$name_loc))

#### EDA ----
# Target Variable: Stall Occupancy Rate
data <- data %>%
  mutate(stall_occupancy_rate = avg_stalls_occupied / avg_stalls_available)

# Occupancy rate in percentage visualised
ggplot(data, aes(x = stall_occupancy_rate * 100)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(title = "Density Plot of Stall Occupancy Rate",
       x = "Stall Occupancy Rate (%)",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Target Variable Distribution shown in box plot + Jitter plot combination
ggplot(data, aes(x = "", y = stall_occupancy_rate * 100)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, color = "blue", alpha = 0.5) +
  labs(title = "Box Plot of Stall Occupancy Rate with Jitter",
       x = "",
       y = "Stall Occupancy Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Prepare features (X) and target (y)
X <- data[, c("avg_stalls_available", "weekly_operational_days", "latitude", "longitude",
              "servicecharge_mkv", "name_loc_encoded")]
y <- data$stall_occupancy_rate

#### Feature Selection ----
boruta_output <- Boruta(X, y, doTrace = 2, maxRuns = 100)
print(boruta_output)
plot(boruta_output, main = "Boruta Feature Importance")
# Finalize decision on tentative attributes
boruta_final <- TentativeRoughFix(boruta_output)
# Confirmed attributes
important_features <- getSelectedAttributes(boruta_final, withTentative = FALSE)
cat("Confirmed Features:\n")
print(important_features)

# Split into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Scale the features
scaler <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(scaler, X_train)
X_test_scaled <- predict(scaler, X_test)

feature_importance_list <- list()

#### Model Training and Execution ----
# Multiple Linear Regression (MLR)
model_lm <- lm(y_train ~ ., data = cbind(X_train_scaled, y_train))
y_pred_lm <- predict(model_lm, newdata = X_test_scaled)
mse_lm <- mean((y_test - y_pred_lm)^2)
cat("Mean Squared Error (Linear Regression):", mse_lm, "\n")
plot(y_test, y_pred_lm, main = "True vs Predicted (Linear Regression)",
     xlab = "True Stall Occupancy Rate", ylab = "Predicted Stall Occupancy Rate", col = "blue")
abline(0, 1, col = "red", lwd = 2)
importance_lm <- summary(model_lm)$coefficients
importance_lm <- data.frame(
  Feature = rownames(importance_lm),
  Importance = importance_lm[, "Estimate"],
  Model = "Linear Regression"
)
importance_lm <- importance_lm[importance_lm$Feature != "(Intercept)", ]
feature_importance_list[[1]] <- importance_lm

# Support Vector Machine (SVM) -- Linear
model_svm_linear <- svm(y_train ~ ., data = cbind(X_train_scaled, y_train), kernel = "linear")
y_pred_svm_linear <- predict(model_svm_linear, newdata = X_test_scaled)
mse_svm_linear <- mean((y_test - y_pred_svm_linear)^2)
cat("Mean Squared Error (SVM - Linear Kernel):", mse_svm_linear, "\n")
plot(y_test, y_pred_svm_linear, main = "True vs Predicted (SVM - Linear Kernel)",
     xlab = "True Stall Occupancy Rate", ylab = "Predicted Stall Occupancy Rate", col = "blue")
abline(0, 1, col = "red", lwd = 2)
importance_svm_linear <- as.numeric(t(model_svm_linear$coefs) %*% model_svm_linear$SV)
importance_svm_linear <- data.frame(
  Feature = colnames(X_train_scaled),
  Importance = importance_svm_linear,
  Model = "SVM Linear"
)
feature_importance_list[[2]] <- importance_svm_linear

# Support Vector Machine (SVM) -- Radial
model_svm_radial <- svm(y_train ~ ., data = cbind(X_train_scaled, y_train), kernel = "radial")
y_pred_svm_radial <- predict(model_svm_radial, newdata = X_test_scaled)
mse_svm_radial <- mean((y_test - y_pred_svm_radial)^2)
cat("Mean Squared Error (SVM - Radial Kernel):", mse_svm_radial, "\n")
plot(y_test, y_pred_svm_radial, main = "True vs Predicted (SVM - Radial Kernel)",
     xlab = "True Stall Occupancy Rate", ylab = "Predicted Stall Occupancy Rate", col = "blue")
abline(0, 1, col = "red", lwd = 2)
# It is not straightforward to extract the feature importance for Radial Kernel

# Decision Tree
model_dt <- rpart(y_train ~ ., data = cbind(X_train_scaled, y_train), method = "anova")
y_pred_dt <- predict(model_dt, newdata = X_test_scaled)
mse_dt <- mean((y_test - y_pred_dt)^2)
cat("Mean Squared Error (Decision Tree):", mse_dt, "\n")
cat("\nFeature Importance (Decision Tree):\n")
importance_dt <- varImp(model_dt)
print(importance_dt)
rpart.plot(model_dt, main = "Decision Tree")
plot(y_test, y_pred_dt, main = "True vs Predicted (Decision Tree)",
     xlab = "True Stall Occupancy Rate", ylab = "Predicted Stall Occupancy Rate", col = "blue")
abline(0, 1, col = "red", lwd = 2)
importance_dt <- as.data.frame(varImp(model_dt))
importance_dt$Feature <- rownames(importance_dt)
rownames(importance_dt) <- NULL
importance_dt$Model <- "Decision Tree"
colnames(importance_dt)[colnames(importance_dt)=="Overall"] <- "Importance"
feature_importance_list[[3]] <- importance_dt

# Random Forest
model_rf <- randomForest(y_train ~ ., data = cbind(X_train, y_train), ntree = 100)
y_pred_rf <- predict(model_rf, newdata = X_test)
mse_rf <- mean((y_test - y_pred_rf)^2)
cat("Mean Squared Error (Random Forest):", mse_rf, "\n")
cat("\nFeature Importance (Random Forest):\n")
print(importance(model_rf))
varImpPlot(model_rf)
plot(y_test, y_pred_rf, main = "True vs Predicted (Random Forest)",
     xlab = "True Stall Occupancy Rate", ylab = "Predicted Stall Occupancy Rate", col = "blue")
abline(0, 1, col = "red", lwd = 2)
importance_rf <- as.data.frame(importance(model_rf))
importance_rf$Feature <- rownames(importance_rf)
rownames(importance_rf) <- NULL
importance_rf <- importance_rf[, c("Feature", "IncNodePurity")]
colnames(importance_rf)[2] <- "Importance"
importance_rf$Model <- "Random Forest"
feature_importance_list[[4]] <- importance_rf

# XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
param <- list(
  objective = "reg:squarederror", # Regression objective
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.3
)
model_xgb <- xgb.train(params = param, data = dtrain, nrounds = 100)
y_pred_xgb <- predict(model_xgb, newdata = dtest)
mse_xgb <- mean((y_test - y_pred_xgb)^2)
cat("Mean Squared Error (XGBoost):", mse_xgb, "\n")
plot(y_test, y_pred_xgb, main = "True vs Predicted (XGBoost)",
     xlab = "True Stall Occupancy Rate", ylab = "Predicted Stall Occupancy Rate", col = "blue")
abline(0, 1, col = "red", lwd = 2)
importance_xgb <- xgb.importance(feature_names = colnames(X_train), model = model_xgb)
importance_xgb <- importance_xgb[, c("Feature", "Gain")]
colnames(importance_xgb)[2] <- "Importance"
importance_xgb$Model <- "XGBoost"
feature_importance_list[[5]] <- importance_xgb

#### Model Performance Summary ----
model_performance <- data.frame(
  Model = c("Linear Regression", "SVM Linear", "SVM Radial", "Decision Tree", "Random Forest", "XGBoost"),
  MSE = c(mse_lm, mse_svm_linear, mse_svm_radial, mse_dt, mse_rf, mse_xgb)
)
# sorting the model performance based on MSE
print(model_performance[order(model_performance$MSE, decreasing = TRUE), ])

ggplot(model_performance, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "Model Performance Comparison", x = "Model", y = "Mean Squared Error (MSE)") +
  theme(legend.position = "none")

# combine all feature importance data
feature_importance <- do.call(rbind, feature_importance_list)

# compare Feature Importance for all the different Models
ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance Comparison",
       x = "Features",
       y = "Importance") +
  theme(legend.position = "top")
