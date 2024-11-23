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
library(smotefamily)
library(polycor)
library(readr)    # Added for read_csv

# Set seed for reproducibility
set.seed(42)

# Load Data
train_data <- read.csv("~/Documents/R/com3018CW/CarInsuranceDatasetProcessing/Dataset/archive/train.csv")

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
    if (col %in% names(data)) {  # Check if column exists
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

# Split the data into training and testing sets (70% train, 30% test)
set.seed(42) # For reproducibility
train_index <- sample(1:nrow(processed_data), 0.7 * nrow(processed_data))
train_set <- processed_data[train_index, ]
test_set <- processed_data[-train_index, ]

# Check the dimensions of the datasets
print(dim(train_set))
print(dim(test_set))

# SMOTE Oversampling on the training set only
x <- as.data.frame(train_set %>% select(-is_claim))  # Features
y <- train_set$is_claim  # Target variable

# Apply SMOTE
smote_output <- SMOTE(X = x, target = y, K = 5, dup_size = 1)

# Extract the balanced dataset
smote_train_df <- smote_output$data
names(smote_train_df)[ncol(smote_train_df)] <- "is_claim"

# Ensure is_claim is numeric after SMOTE
smote_train_df$is_claim <- as.numeric(smote_train_df$is_claim)

# Standardization on the training set
preprocess_params <- preProcess(smote_train_df %>% select(-is_claim), method = c("center", "scale"))
standardized_train_data <- predict(preprocess_params, smote_train_df)

# Apply the same preprocessing (standardization) to the test set
standardized_test_data <- predict(preprocess_params, test_set)

# Ensure is_claim remains numeric in the test set
standardized_test_data$is_claim <- as.numeric(standardized_test_data$is_claim)

# Check the structure of the standardized datasets
str(standardized_train_data)
str(standardized_test_data)

# Save the standardized datasets to CSV files
write.csv(standardized_train_data, "~/Documents/R/com3018CW/CarInsuranceDatasetProcessing/standardized_train_data.csv", row.names = FALSE)
write.csv(standardized_test_data, "~/Documents/R/com3018CW/CarInsuranceDatasetProcessing/standardized_test_data.csv", row.names = FALSE)

# Print messages to confirm saving
cat("Standardized training data saved to 'standardized_train_data.csv'\n")
cat("Standardized testing data saved to 'standardized_test_data.csv'\n")

# Visualization 1: Box Plots for Continuous Variables
continuous_vars <- c("policy_tenure", "age_of_car", "age_of_policyholder ", "max_torque", "max_power", "displacement")

# Check which continuous variables actually exist in the data
available_vars <- continuous_vars[continuous_vars %in% names(standardized_train_data)]

long_data <- standardized_train_data %>%
  select(all_of(available_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

gg_boxplot <- ggplot(long_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Box Plots for Continuous Variables",
       x = "Variables",
       y = "Values") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

# Visualization 2: Correlation Bar Plot
numeric_data <- standardized_train_data %>% select(where(is.numeric))
correlations <- cor(numeric_data, use = "complete.obs")
cor_with_claim <- correlations["is_claim", ]
cor_with_claim <- cor_with_claim[!names(cor_with_claim) %in% "is_claim"]
cor_with_claim_sorted <- sort(abs(cor_with_claim), decreasing = TRUE)

cor_barplot <- ggplot(data.frame(Variable = names(cor_with_claim_sorted), 
                                 Correlation = cor_with_claim_sorted), 
                      aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlation with Claim Status",
       x = "Variables",
       y = "Absolute Correlation")

# Visualization 3: Pairwise Scatter Plots
plots <- list()
counter <- 1

# Only use available continuous variables
available_vars <- continuous_vars[continuous_vars %in% names(standardized_train_data)]

for (i in 1:(length(available_vars) - 1)) {
  for (j in (i + 1):length(available_vars)) {
    p <- ggplot(standardized_train_data, aes_string(x = available_vars[i], y = available_vars[j])) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = paste(available_vars[i], "vs", available_vars[j]))
    plots[[counter]] <- p
    counter <- counter + 1
  }
}

# Only create scatter plot matrix if there are plots
if (length(plots) > 0) {
  scatter_plot_matrix <- do.call(grid.arrange, c(plots, ncol = 2))
}

# Display plots
print(gg_boxplot)
print(cor_barplot)
if (exists("scatter_plot_matrix")) {
  print(scatter_plot_matrix)
}
