

# Load necessary libraries
library(ggplot2)

df <- PrescribingDataForRimport

# Assuming the data is already in a dataframe `df`
# Plotting the distribution of BNF4Spend using a histogram
# Load the ggplot2 library

ggplot(df, aes(x = reorder(MDMRank, -PercentSpendOnBNF4), y = PercentSpendOnBNF4)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Percent Spend on BNF4 by MDM Rank",
       x = "MDM Rank",
       y = "Percent Spend on BNF4")



library(ggplot2)

# Assuming your data is in a dataframe called df
# Create a new factor variable 'MDMRank_bins' with 'cut' to bin the MDMRank values
df$MDMRank_bins <- cut(df$MDMRank, breaks=seq(from=min(df$MDMRank), to=max(df$MDMRank), by=50), include.lowest=TRUE, right=FALSE)

# Plot with the new binned MDMRank variable
ggplot(df, aes(x = MDMRank_bins, y = PercentSpendOnBNF4)) +
  geom_jitter(alpha = 1, width = 0.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=7),
        axis.title = element_text(size=10)) +
  labs(title = "Distribution of Percent Spend on BNF4 by Binned MDM Rank",
       x = "Binned MDM Rank",
       y = "Percent Spend on BNF4")






library(ggplot2)

# Assuming df$MDMRank is a factor and can be reasonably converted to numeric for plotting
df$MDMRank_numeric <- as.numeric(as.character(df$MDMRank))

ggplot(df, aes(x = MDMRank_numeric, y = PercentSpendOnBNF4)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  scale_x_continuous(breaks = seq(from = min(df$MDMRank_numeric, na.rm = TRUE), 
                                  to = max(df$MDMRank_numeric, na.rm = TRUE), by = 50)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Percent Spend on BNF4 by MDM Rank",
       x = "MDM Rank",
       y = "Percent Spend on BNF4")


library(ggplot2)

# Assuming df$MDMRank is a factor and converting it to numeric for plotting purposes
df$MDMRank_numeric <- as.numeric(as.character(df$MDMRank))

ggplot(df, aes(x = MDMRank_numeric, y = PercentSpendOnBNF4)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + # Add a linear regression line with standard error
  scale_x_continuous(breaks = seq(from = min(df$MDMRank_numeric, na.rm = TRUE), 
                                  to = max(df$MDMRank_numeric, na.rm = TRUE), by = 50)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Percent Spend on BNF4 by MDM Rank",
       x = "MDM Rank",
       y = "Percent Spend on BNF4")


# Assuming your data frame is named 'df'

# Calculate BNF4Spend per patient
df$BNF4SpendPerPatient <- df$BNF4Spend / df$NumberPatients

# Now you can view the data frame to confirm the new column has been added
head(df)

# chart for BNF4 spend per patient 

ggplot(df, aes(x = MDMRank_numeric, y = BNF4SpendPerPatient)) +
  geom_jitter(alpha = 0.5, width = 0.2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) + # Add a linear regression line with standard error
  scale_x_continuous(breaks = seq(from = min(df$MDMRank_numeric, na.rm = TRUE), 
                                  to = max(df$MDMRank_numeric, na.rm = TRUE), by = 50)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of BNF4 Spend Per Patientby MDM Rank",
       x = "MDM Rank",
       y = "BNF4 Spend Per Patient")


ggplot(df, aes(x=factor(MDMRank_numeric), y=BNF4SpendPerPatient)) +
  geom_violin() +
  labs(x="MDM Rank", y="BNF4 Spend Per Patient")


ggplot(df, aes(x=MDMRank_numeric, y=BNF4SpendPerPatient)) +
  geom_hex() +
  labs(x="MDM Rank", y="BNF4 Spend Per Patient")


qqnorm(df$BNF4SpendPerPatient)
qqline(df$BNF4SpendPerPatient)

# Applying log transformation to the BNF4SpendPerPatient variable, adding 1 to shift all values away from zero
df$log_BNF4SpendPerPatient <- log(df$BNF4SpendPerPatient + 1)

# Plotting the histogram of the log-transformed BNF4SpendPerPatient
hist(df$log_BNF4SpendPerPatient, main="Histogram of Log-Transformed BNF4 Spend Per Patient", xlab="Log(BNF4 Spend Per Patient + 1)")

# Q-Q plot for the log-transformed BNF4SpendPerPatient
qqnorm(df$log_BNF4SpendPerPatient)
qqline(df$log_BNF4SpendPerPatient, col = "steelblue", lwd = 2)




yggplot(df, aes(x=factor(MDMRank_numeric), y=BNF4SpendPerPatient)) +
  geom_boxplot() +
  labs(x="MDM Rank", y="BNF4 Spend Per Patient")







# Fit a linear regression model with multiple predictors
model <- lm(PercentSpendOnBNF4 ~ NumberPatients + MDMRank, data = df)

# Summary of the model
summary(model)

# Assuming you've already fitted a model named 'model'
library(ggplot2)

# Use the 'predict()' function to get the predicted values from the model
df$Predicted <- predict(model)

# Plot the actual vs predicted values
ggplot(df, aes(x = Predicted, y = PercentSpendOnBNF4)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Predicted Percent Spend on BNF4", 
       y = "Actual Percent Spend on BNF4", 
       title = "Actual vs. Predicted Percent Spend on BNF4")

# Calculate residuals
df$Residuals <- residuals(model)

# Plot residuals
ggplot(df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predicted Percent Spend on BNF4", 
       y = "Residuals", 
       title = "Residuals vs. Predicted Percent Spend on BNF4")

# Use 'augment()' from the broom package to get a dataframe with both observed and predicted values
library(broom)
df_augmented <- augment(model)

# Plot the data with the regression line
ggplot(df_augmented, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted values", 
       y = "Residuals", 
       title = "Residuals vs Fitted Plot")


#-------------------

install.packages("randomForest")
library(randomForest)

# Check for missing values and remove or impute them
df <- na.omit(df)  # This removes rows with missing values; consider imputation instead

# Convert MDMRank_numeric to a numeric variable if it's not already
df$MDMRank_numeric <- as.numeric(as.character(df$MDMRank_numeric))

set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(df), size = floor(0.8 * nrow(df)))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]


set.seed(123)  # for reproducibility
rf_model1 <- randomForest(PercentSpendOnBNF4 ~ MDMRank_numeric, data=train_data, ntree=800, mtry=1, importance=TRUE)

predictions1 <- predict(rf_model1, test_data)

mse <- mean((predictions1 - test_data$PercentSpendOnBNF4)^2)
rmse <- sqrt(mse)
print(paste("RMSE:", rmse))

tuned_rf <- tuneRF(train_data[, -which(names(train_data) == "PercentSpendOnBNF4")], 
                   train_data$PercentSpendOnBNF4,
                   stepFactor=1.05,
                   improve=0.01,
                   ntreeTry=500,
                   trace=TRUE,
                   plot=TRUE)




# Load the randomForest package
install.packages("randomForest")
library(randomForest)

# Check for missing values and remove or impute them
df <- na.omit(df)  # This removes rows with missing values; consider imputation instead

# Convert MDMRank and NumberPatients to numeric if they are not already
df$MDMRank <- as.numeric(as.character(df$MDMRank))
df$NumberPatients <- as.numeric(as.character(df$NumberPatients))

# Split the data into training and test sets
set.seed(123)  # Setting seed for reproducibility
train_indices <- sample(1:nrow(df), size = floor(0.8 * nrow(df)))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the Random Forest model with two predictors: MDMRank and NumberPatients
set.seed(124)  # Setting seed for reproducibility
rf_model2 <- randomForest(BNF4Spend ~ MDMRank + NumberPatients, data=train_data, ntree=800, mtry=2, importance=TRUE)

# Make predictions on the test set
predictions2 <- predict(rf_model2, test_data)

# Calculate and print RMSE
mse <- mean((predictions - test_data$BNF4Spend)^2)
rmse <- sqrt(mse)
print(paste("RMSE:", rmse))


# For the first model (single predictor)
predictions1 <- predict(rf_model1, test_data)
mape_1 <- mean(abs((predictions1 - test_data$BNF4Spend) / test_data$BNF4Spend))
print(paste("MAPE for Model 1:", mape_1))

# For the second model (two predictors)
predictions2 <- predict(rf_model2, test_data)
mape_2 <- mean(abs((predictions2 - test_data$BNF4Spend) / test_data$BNF4Spend))
print(paste("MAPE for Model 2:", mape_2))

# Assuming you have a randomForest model named rf_model
importance <- importance(rf_model)
varImpPlot(rf_model, main="Variable Importance")

#---------------------

# For a single variable
partialPlot(rf_model, train_data, "MDMRank", main="Partial Dependence Plot for MDMRank")

# Assuming you have a vector of predictions named predictions
actual <- test_data$BNF4Spend
predicted <- predict(rf_model, test_data)

# Create a dataframe for plotting
plot_data <- data.frame(Actual=actual, Predicted=predicted)

# Use ggplot2 for plotting
library(ggplot2)
ggplot(plot_data, aes(x=Actual, y=Predicted)) + 
  geom_point(alpha=0.4) +
  geom_smooth(method="lm", color="red") +
  labs(title="Actual vs Predicted Plot", x="Actual Values", y="Predicted Values") +
  theme_minimal()

# Extract the structure of the first tree in the forest
tree <- getTree(rf_model, k = 1, labelVar = TRUE)

install.packages("rpart.plot")

library(rpart.plot)
rpart.plot(tree)

library(partykit)
# Convert to party object
randomForest_to_party <- as.party(rf_model)

# Plot
plot(randomForest_to_party)



