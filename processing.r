# List of required packages
required_packages <- c("tidyr", "ggplot2", "gridExtra", "dplyr", "caret", "smotefamily", "polycor", "readr")

# Function to check and install packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Loop through the required packages
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Load required libraries
library(tidyr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)
library(smotefamily)  # Contains ADASYN
library(polycor)
library(readr)

# Set seed for reproducibility
set.seed(42)

# Load Data
train_data <- read.csv("Dataset/archive/train.csv")

# Preprocessing Function
preprocess_data <- function(data) {
  # Convert Yes/No columns to binary
  boolean_columns <- c('is_esc', 'is_adjustable_steering', 'is_tpms', 'is_parking_sensors', 
                       'is_parking_camera', 'is_front_fog_lights', 'is_rear_window_wiper', 
                       'is_rear_window_washer', 'is_rear_window_defogger', 'is_brake_assist', 
                       'is_power_door_locks', 'is_central_locking', 'is_power_steering', 
                       'is_driver_seat_height_adjustable', 'is_day_night_rear_view_mirror', 
                       'is_ecw', 'is_speed_alert')
  
  for (col in boolean_columns) {
    if (col %in% names(data)) {
      data[[col]] <- ifelse(data[[col]] == "Yes", 1, 0)
    }
  }
  
  # Extract numeric values from 'max_torque' and 'max_power'
  if ("max_torque" %in% names(data)) {
    data$max_torque <- as.numeric(gsub("[^0-9.]", "", data$max_torque))
  }
  if ("max_power" %in% names(data)) {
    data$max_power <- as.numeric(gsub("[^0-9.]", "", data$max_power))
  }
  
  # Drop irrelevant columns if they exist
  if ("policy_id" %in% names(data)) {
    data <- data %>% select(-policy_id)
  }
  
  # Remove non-numeric columns
  data <- data %>% select(where(is.numeric))
  
  return(data)
}

# Preprocess the data
processed_data <- preprocess_data(train_data)

# Remove any rows with NA values
processed_data <- na.omit(processed_data)

# Identify most predictive features
correlations <- cor(processed_data, use = "complete.obs")
cor_with_claim <- correlations["is_claim", ]
important_features <- names(sort(abs(cor_with_claim), decreasing = TRUE))[2:11]  # Top 10 predictors excluding target

# Reduce data to key features
selected_features <- c(important_features, "is_claim")
processed_data <- processed_data %>% select(all_of(selected_features))
######################################################################################
# Set a seed for reproducibility
set.seed(42)

# Split the data into 60% train, 20% validation, 20% test
train_index <- sample(1:nrow(processed_data), 0.6 * nrow(processed_data))
remaining_data <- processed_data[-train_index, ]
validate_index <- sample(1:nrow(remaining_data), 0.5 * nrow(remaining_data))
validate_set <- remaining_data[validate_index, ]
test_set <- remaining_data[-validate_index, ]

# Initial train set
train_set <- processed_data[train_index, ]

# Print the sizes of the datasets
cat("Initial Dataset Sizes:\n")
cat("Train Set:", nrow(train_set), "\n")
cat("Validation Set:", nrow(validate_set), "\n")
cat("Test Set:", nrow(test_set), "\n\n")






# Assuming 'train_set' is your original dataset
# Split the dataset into majority and minority classes
majority <- train_set[train_set$is_claim == 0, ]
minority <- train_set[train_set$is_claim == 1, ]

# Step 1: Oversampling - Increase minority class size 
# Sample the minority class to match the size of the majority class
oversampled_minority <- minority[sample(1:nrow(minority), nrow(majority), replace = TRUE), ]

# Print the sizes after oversampling
cat("Size of oversampled minority class:", nrow(oversampled_minority), "\n")
cat("Size of majority class:", nrow(majority), "\n")

# Step 2: Undersampling - Reduce majority class size 
# Sample the majority class to match the size of the oversampled minority class
undersampled_majority <- majority[sample(1:nrow(majority), nrow(oversampled_minority)), ]

# Print the sizes after undersampling
cat("Size of undersampled majority class:", nrow(undersampled_majority), "\n")

# Combine the undersampled majority and oversampled minority classes
balanced_train_set <- rbind(undersampled_majority, oversampled_minority)

# Step 3: Shuffle the combined dataset to randomize class order
balanced_train_set <- balanced_train_set[sample(1:nrow(balanced_train_set)), ]

# Verify the class distribution
cat("\nClass Distribution After Balancing:\n")
print(table(balanced_train_set$is_claim))

# Optional: Percentage distribution
cat("\nPercentage Distribution After Balancing:\n")
print(prop.table(table(balanced_train_set$is_claim)) * 100)








# Shuffle the balanced training set
balanced_train_set <- balanced_train_set[sample(1:nrow(balanced_train_set)), ]

# Print the size of the balanced training set
# Print the sizes of the datasets
cat("Balanced Train Set Size:", nrow(balanced_train_set), "\n")
cat("Test Set Size:", nrow(test_set), "\n")
cat("Validation Set Size:", nrow(validate_set), "\n\n")

# Count entries for each is_claim class
cat("Class Counts in the Balanced Train Set:\n")
print(table(balanced_train_set$is_claim))

cat("\nClass Counts in the Test Set:\n")
print(table(test_set$is_claim))

cat("\nClass Counts in the Validation Set:\n")
print(table(validate_set$is_claim))







# Standardization on the balanced training set
preprocess_params <- preProcess(balanced_train_set %>% select(-is_claim), method = c("center", "scale"))
standardized_train_data <- predict(preprocess_params, balanced_train_set %>% select(-is_claim))
# Add the target variable back to the standardized training data and convert to integer
standardized_train_data$is_claim <- as.integer(balanced_train_set$is_claim)

# Apply the same preprocessing (standardization) to the validation set
standardized_validate_data <- predict(preprocess_params, validate_set %>% select(-is_claim))
# Add the target variable back to the standardized validation data and convert to integer
standardized_validate_data$is_claim <- as.integer(validate_set$is_claim)

# Apply the same preprocessing (standardization) to the test set
standardized_test_data <- predict(preprocess_params, test_set %>% select(-is_claim))
# Add the target variable back to the standardized test data and convert to integer (if available)
standardized_test_data$is_claim <- as.integer(test_set$is_claim)

# Shuffle the standardized training set
standardized_train_data <- standardized_train_data[sample(1:nrow(standardized_train_data)), ]

# Print the first few rows of the standardized training data
cat("\nStandardized Training Data (first few rows):\n")
print(head(standardized_train_data))

# Print the first few rows of the standardized validation data
cat("\nStandardized Validation Data (first few rows):\n")
print(head(standardized_validate_data))

# Print the first few rows of the standardized test data
cat("\nStandardized Test Data (first few rows):\n")
print(head(standardized_test_data))

# Save the standardized datasets to CSV files
# change HOME for windows
downloads_path <- file.path(Sys.getenv("HOME"), "Downloads")
write.csv(standardized_train_data, file.path(downloads_path, "standardized_train_set.csv"), row.names = FALSE)
write.csv(standardized_test_data, file.path(downloads_path, "standardized_test_set.csv"), row.names = FALSE)
write.csv(standardized_validate_data, file.path(downloads_path, "standardized_validate_set.csv"), row.names = FALSE)

# Visualizations for Report and Feature Selection Justification

# Bar Plot for Feature Importance
cor_with_claim <- cor(processed_data, use = "complete.obs")["is_claim", ]
important_features <- names(sort(abs(cor_with_claim), decreasing = TRUE))[2:11]  # Top 10 predictors excluding target

# Create a data frame for plotting
correlation_df <- data.frame(
  Feature = names(cor_with_claim),
  Correlation = cor_with_claim
) %>% 
  filter(Feature %in% important_features) %>% 
  arrange(desc(abs(Correlation)))

# Plot feature importance
importance_plot <- ggplot(correlation_df, aes(x = reorder(Feature, -abs(Correlation)), y = Correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Feature Importance (Correlation with 'is_claim')",
    x = "Feature",
    y = "Correlation"
  )

# Distribution of Selected Features
distribution_plots <- lapply(important_features, function(feature) {
  ggplot(processed_data, aes_string(x = feature, fill = "factor(is_claim)")) +
    geom_density(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = paste("Distribution of", feature),
      x = feature,
      fill = "Claim Status"
    )
})

# Combine distribution plots into a grid
distribution_grid <- do.call(grid.arrange, c(distribution_plots, ncol = 2))

# Class Balance Visualization
balance_before <- train_set %>% 
  count(is_claim) %>% 
  mutate(Dataset = "Original Training Set")

balance_after <- balanced_train_set %>% 
  count(is_claim) %>% 
  mutate(Dataset = "Balanced Training Set")

class_balance_df <- bind_rows(balance_before, balance_after)

# Plot class balance
balance_plot <- ggplot(class_balance_df, aes(x = Dataset, y = n, fill = factor(is_claim))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Class Balance Before and After Balancing",
    x = "Dataset",
    y = "Count",
    fill = "Claim Status"
  )

# Display the visualizations
print(importance_plot)
print(distribution_grid)
print(balance_plot)

