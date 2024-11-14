install.packages(c("ggplot2", "gridExtra", "plotly", "dplyr", "caret"))

data <- read.csv("Dataset/archive/train.csv") 
processed_data <- data

head(processed_data)         # Displays the first few rows
summary(processed_data)      # Provides a summary of each column
str(processed_data)          # Shows the structure of the data (data types, column names, etc.)

colSums(is.na(processed_data))  # Count of missing values in each column

# Convert appropriate columns to numeric types
# processed_data$displacement <- as.numeric(processed_data$displacement)
# str(processed_data)

# Verify extraction by viewing the first few rows
# head(processed_data$max_torque)
# head(processed_data$max_power)

# List of boolean columns to convert
# boolean_columns <- c('is_esc', 'is_adjustable_steering', 'is_tpms', 'is_parking_sensors', 
#                     'is_parking_camera', 'is_front_fog_lights', 'is_rear_window_wiper', 
#                     'is_rear_window_washer', 'is_rear_window_defogger', 'is_brake_assist', 
#                     'is_power_door_locks', 'is_central_locking', 'is_power_steering', 
#                     'is_driver_seat_height_adjustable', 'is_day_night_rear_view_mirror', 
#                     'is_ecw', 'is_speed_alert')

# Convert each boolean column to binary (1/0)
# for (col in boolean_columns) {
#   processed_data[[col]] <- ifelse(processed_data[[col]] == "Yes", 1, 0)
# }

# Verify the conversion
# head(data[, boolean_columns])
# head(processed_data[, boolean_columns])

# way 1 to visualise

library(ggplot2)
library(gridExtra)

# Create box plots for each variable
p1 <- ggplot(processed_data, aes(y = policy_tenure)) +
  geom_boxplot(fill = "blue") +
  ggtitle("Policy Tenure") +
  theme_minimal()

p2 <- ggplot(processed_data, aes(y = age_of_car)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Age of Car") +
  theme_minimal()

p3 <- ggplot(processed_data, aes(y = age_of_policyholder)) +
  geom_boxplot(fill = "green") +
  ggtitle("Age of Policyholder") +
  theme_minimal()

p4 <- ggplot(processed_data, aes(y = max_torque)) +
  geom_boxplot(fill = "red") +
  ggtitle("Max Torque") +
  theme_minimal()

p5 <- ggplot(processed_data, aes(y = max_power)) +
  geom_boxplot(fill = "purple") +
  ggtitle("Max Power") +
  theme_minimal()

p6 <- ggplot(processed_data, aes(y = displacement)) +
  geom_boxplot(fill = "brown") +
  ggtitle("Displacement") +
  theme_minimal()

# Arrange the plots in a 3x2 grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)

# way 2 to visualise

library(plotly)

# Create an interactive box plot
fig <- plot_ly(processed_data, y = ~policy_tenure, type = "box", name = "policy_tenure", boxpoints = "all", marker = list(color = 'blue')) %>%
  add_trace(y = ~age_of_car, name = "age_of_car", boxpoints = "all", marker = list(color = 'orange')) %>%
  add_trace(y = ~age_of_policyholder, name = "age_of_policyholder", boxpoints = "all", marker = list(color = 'green')) %>%
  add_trace(y = ~max_torque, name = "max_torque", boxpoints = "all", marker = list(color = 'red')) %>%
  add_trace(y = ~max_power, name = "max_power", boxpoints = "all", marker = list(color = 'purple')) %>%
  add_trace(y = ~displacement, name = "displacement", boxpoints = "all", marker = list(color = 'brown')) %>%
  layout(title = "Outliers Detection",
         boxmode = "group")  # Group boxes together

fig

library(dplyr)
library(caret)
library(stringr)

# Remove irrelevant variables
processed_data <- processed_data %>% select(-policy_id)

# Extract numeric values from 'max_torque' and 'max_power' columns
processed_data$max_torque <- as.numeric(str_extract(processed_data$max_torque, "\\d+\\.?\\d*"))
processed_data$max_power <- as.numeric(str_extract(processed_data$max_power, "\\d+\\.?\\d*"))

# Remove near-zero variance columns
# nzv <- nearZeroVar(data, saveMetrics = TRUE)
# data <- data[, !nzv$nzv]

# Identify numeric columns before one-hot encoding
numeric_columns <- sapply(processed_data, is.numeric)

# Ensure the target variable 'is_claim' is excluded from the scaling process
numeric_columns <- setdiff(names(processed_data)[numeric_columns], "is_claim")

# Convert categorical columns to factors (for one-hot encoding)
processed_data <- processed_data %>%
  mutate_if(is.character, as.factor)

# Apply one-hot encoding to remaining categorical variables
processed_data_encoded <- dummyVars(" ~ .", data = processed_data, fullRank = TRUE)
processed_data <- predict(processed_data_encoded, newdata = processed_data)
processed_data <- as.data.frame(processed_data)

# Scale the numeric columns
processed_data[numeric_columns] <- scale(processed_data[numeric_columns])

# Calculate feature correlations with target variable `is_claim`
correlations <- cor(processed_data, use = "complete.obs")
cor_with_claim <- correlations["is_claim", ]
cor_with_claim <- cor_with_claim[!names(cor_with_claim) %in% "is_claim"]  # Exclude self-correlation

# Rank variables by absolute correlation
cor_with_claim_sorted <- sort(abs(cor_with_claim), decreasing = TRUE)

# Select the top factors
top_50_factors <- head(cor_with_claim_sorted, 50)

# Convert the top factors to a data frame
top_50_df <- data.frame(
  Factor = names(top_50_factors),
  Correlation = as.numeric(top_50_factors)
)

# Create a bar plot to show the top factors
ggplot(top_50_df, aes(x = reorder(Factor, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for easier reading
  labs(title = "Top 50 Factors Influencing Insurance Claim Likelihood",
       x = "Factors",
       y = "Correlation with Claim Likelihood") +
  theme_minimal()

# Download the modified data
downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
write.csv(processed_data, file.path(downloads_path, "processed_data.csv"), row.names = FALSE)
