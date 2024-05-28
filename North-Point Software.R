################# PROJECT NORTH-POINT  PRODUCTION COMPANY #######################
# Load the required libraries
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)
library(caret)
library(lattice)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(e1071)
library(class)
library(gains)
library(gridExtra)

##loading the dataset in R using read csv function and head function to see 5 observations
npspc_data <- read.csv(file = "North-Point List.csv", header = TRUE, sep = ",")

head(npspc_data)
dim(npspc_data)
str(npspc_data)

### Data clean up 
# Checking for missing values
missing_values <- sum(is.na(npspc_data))
missing_values

# Data exploration
summary(npspc_data)

# Creating a logical condition to check for zeros in  variables
zero_condition <- npspc_data$Spending == 0

# Using subset() to extract rows with zeros in  variables
rows_with_zeros <- subset(npspc_data, zero_condition)
dim(rows_with_zeros)

# View the resulting subset
print(rows_with_zeros)

# Save the subset to a CSV file
write.csv(rows_with_zeros, file = "rows_with_zeros.csv", row.names = FALSE)

pur_condition <- npspc_data$Purchase == 1 & npspc_data$Spending == 0
rows_pur_condition <- subset(npspc_data, pur_condition)
dim(rows_pur_condition)
print(rows_pur_condition)

zero_pur_condition <- npspc_data$Purchase == 0 & npspc_data$Spending != 0
rows_zero_pur_condition <- subset(npspc_data, zero_pur_condition)
dim(rows_zero_pur_condition)
print(rows_zero_pur_condition)

# Find the row where sequence_number is 711
row_to_change <- which(npspc_data$sequence_number == 711)

# Change the value of "Purchase" to 1 for the identified row
npspc_data[row_to_change, "Purchase"] <- 1

zero_Freq <- npspc_data$Freq == 0   #& npspc_data$Spending != 0
rows_zero_Freq <- subset(npspc_data, zero_Freq)
dim(rows_zero_Freq)
print(rows_zero_Freq)
summary(rows_zero_Freq)


zero_res <- npspc_data$Address_is_res == 1   & npspc_data$Spending == 0
rows_zero_res <- subset(npspc_data, zero_res)
dim(rows_zero_res)
print(rows_zero_res)
summary(rows_zero_Freq)


########################### VIF ####################################
library(car)  # for the vif() function

vif_values <- car::vif(lm(Spending ~ . -sequence_number -Purchase, data = npspc_data))
vif_values

########################### Data Exploration #####################
# theme settings
my_theme <- theme(
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black")
)

# Create a list of numerical columns to include in the box plots
numerical_columns <- c('Freq', 'last_update_days_ago', 'X1st_update_days_ago', 'Spending')

## BOx plots
# Create box plots for each numerical column
par(mfrow=c(2, 2))  # Set the layout to 2x2 for four plots on one page
for (col in numerical_columns) {
  boxplot(npspc_data[[col]], 
          main = paste("Boxplot of", col), 
          xlab = col,
          horizontal = TRUE,  # Display horizontal box plots
          col = "lightblue",  # Box plot color
          border = "black"    # Border color
  )
}

## Histogram
# Generating histograms for key variables
hist_vars <- c('Spending', 'last_update_days_ago', 'X1st_update_days_ago', 'Freq')
par(mfrow=c(2, 2))
for (var in hist_vars) {
  hist(npspc_data[[var]], main = paste("Distribution of", var), xlab = var, breaks = 20, col = "lightblue", border = "black")
  box(lwd = 2)  # Adds a thicker border around each histogram
}

#################### Barplots ####################################
# Reshaping the data for ggplot2
# Convert binary to categorical "Yes"/"No"
long_data <- reshape2::melt(npspc_data, id.vars = "sequence_number", 
                            measure.vars = c("US", "Web.order", "Gender.male")) %>%
  mutate(value = ifelse(value == 1, "Yes", "No"))

# Plotting without grid lines and with categorical labels
ggplot(long_data, aes(x = value, fill = value)) +  # Added fill aesthetic for better color control
  geom_bar(stat = "count", show.legend = FALSE) +  # Default stat for geom_bar is "count" which is appropriate here
  facet_wrap(~variable, scales = "free_y") +       # 'scales = "free_y"' allows each facet to independently scale the y-axis
  theme_minimal() +
  my_theme
  # theme(panel.grid.major = element_blank(),        # Removes major grid lines
  #       panel.grid.minor = element_blank(),        # Removes minor grid lines
  #       panel.border = element_rect(color = "black", fill = NA), # Adds border around each plot
  #       axis.text.x = element_text(hjust = 1))  
long_data_2 <- reshape2::melt(npspc_data, id.vars = "sequence_number", 
                              measure.vars = c("Address_is_res", "Purchase"))%>%
  mutate(value = ifelse(value == 1, "Yes", "No"))

ggplot(long_data_2, aes(x = value, fill = value)) +  # Added fill aesthetic for better color control
  geom_bar(stat = "count", show.legend = FALSE) +  # Default stat for geom_bar is "count" which is appropriate here
  facet_wrap(~variable, scales = "free_y") +       # 'scales = "free_y"' allows each facet to independently scale the y-axis
  theme_minimal() +
  my_theme

###################### Scatter plot #######################################
# Create a scatter plot of Spending vs. Frequency with grid lines removed
scatterplot_freq <- ggplot(npspc_data, aes(x = Freq, y = Spending)) +
  geom_point() +
  labs(title = "Scatter Plot of Spending vs. Frequency",
       x = "Frequency",
       y = "Spending") +
  theme_minimal() +
  my_theme
 
# Scatter plot of Spending vs. last_update_days_ago with grid lines removed
scatterplot_last_update <- ggplot(npspc_data, aes(x = last_update_days_ago, y = Spending)) +
  geom_point() +
  labs(title = "Scatter Plot of Spending vs. Last Update Days Ago",
       x = "Last Update Days Ago",
       y = "Spending") +
  theme_minimal() +
  my_theme
  
# Scatter plot of Spending vs. 1st_update_days_ago with grid lines removed
scatterplot_1st_update <- ggplot(npspc_data, aes(x = X1st_update_days_ago, y = Spending)) +
  geom_point() +
  labs(title = "Scatter Plot of Spending vs. 1st Update Days Ago",
       x = "1st Update Days Ago",
       y = "Spending") +
  theme_minimal() +
  my_theme

# Display the scatter plots
print(scatterplot_freq)
print(scatterplot_last_update)
print(scatterplot_1st_update)


#### plot to see total spending by each source
# List of source columns
source_columns <- c('source_a', 'source_c', 'source_b', 'source_d', 'source_e', 'source_m', 'source_o', 'source_h', 'source_r', 'source_s', 'source_t', 'source_u', 'source_p', 'source_x', 'source_w')

# Initialize an empty data frame to store the results
source_spending_df <- data.frame(Source = character(0), TotalSpending = numeric(0))

# Loop through each source column and calculate total spending
for (source_col in source_columns) {
  total_spending <- sum(npspc_data$Spending[npspc_data[[source_col]] == 1])
  source_spending_df <- rbind(source_spending_df, data.frame(Source = source_col, TotalSpending = total_spending))
}

# Sort the data frame by Total Spending in descending order
source_spending_df <- source_spending_df[order(-source_spending_df$TotalSpending), ]

ggplot(source_spending_df, aes(x = TotalSpending, y = reorder(Source, TotalSpending))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = 'Total Spending by Source',
       x = 'Total Spending',
       y = 'Source') +
  scale_x_continuous(labels = comma) +  # Format x-axis labels with commas
  theme_minimal()+
  theme(panel.grid.major = element_blank(),    # Remove major grid lines
        panel.grid.minor = element_blank(),    # Remove minor grid lines
        panel.border = element_rect(color = "black", fill = NA))  # Keep the border


############ chi square tests for categorical variables##########
# Chi square test for categorical variables with outcome variable Purchase
chisq_US <- chisq.test(npspc_data$US, npspc_data$Purchase)
chisq_weborder <- chisq.test(npspc_data$Web.order, npspc_data$Purchase)
chisq_gender <- chisq.test(npspc_data$Purchase, npspc_data$Gender.male)
chisq_address <- chisq.test(npspc_data$Address_is_res, npspc_data$Purchase)
#chisq_recent_update <- chisq.test(npspc_data$recently_updated, npspc_data$Purchase)

# Subsetting source columns
source_columns <- c('source_a', 'source_c', 'source_b', 'source_d', 'source_e', 
                    'source_m', 'source_o', 'source_h', 'source_r', 'source_s', 
                    'source_t', 'source_u', 'source_p', 'source_x', 'source_w')

# Initialize an empty list to store the results
chisq_results <- list()

# Perform chi-square test for each source column
for (source_col in source_columns) {
  chisq_test_result <- chisq.test(npspc_data[[source_col]], npspc_data$Purchase)
  chisq_results[[source_col]] <- chisq_test_result
}

# View the results
chisq_US
chisq_weborder
chisq_gender
chisq_address

chisq_results

######  Correlation analysis  ######
# Computed the correlation matrix for the specified numerical columns
correlation_matrix <- cor(npspc_data[numerical_columns], use = "complete.obs")  # Handles missing data by using available data
correlation_matrix

#####################  Data partitioning ##################################
# Set seed
set.seed(1234) # for reproducibility

# Partitioning data indices
training_rows <- createDataPartition(npspc_data$Purchase, p = 0.4, list = FALSE)
training_data <- npspc_data[training_rows, ]

validation_holdout_rows <- setdiff(1:nrow(npspc_data), training_rows)
validation_rows <- sample(validation_holdout_rows, 700)
validation_data <- npspc_data[validation_rows, ]

holdout_rows <- setdiff(validation_holdout_rows, validation_rows)
holdout_data <- npspc_data[holdout_rows, ]

# Check the proportions
cat("Training data proportion:", nrow(training_data), "\n")
cat("Validation data proportion:", nrow(validation_data), "\n")
cat("Holdout data proportion:", nrow(holdout_data), "\n")

#################### Predictior Relevance Analysis ##########################
# subsetting dataset relevant to models
step <- select(training_data, -sequence_number, -Spending, -X1st_update_days_ago)
str(step)

# Linear regression model 
model_purchase <- glm(Purchase ~ ., data = step, family = binomial(), control = glm.control(maxit = 100))
summary(model_purchase)

#### Step wise feature selection For Purchasers######
## Forward stepwise emilination
model_purchase_forward <- step(model_purchase, direction = "forward")
summary(model_purchase_forward)


## BAckward stepwise elimination
model_purchase_backward <- step(model_purchase, direction = "backward")
summary(model_purchase_backward)


## stepwise variable selection using BIC
model_purchase_bic <- step(model_purchase, k = log(nrow(training_data)))
summary(model_purchase_bic)

#### Step wise feature selection For Spending ########
## Linear regression model 
model_spending <- lm(Spending ~. -sequence_number -Purchase -X1st_update_days_ago, data = training_data)
summary(model_spending)

## Forward stepwise emilination
spending_forward <- step(model_spending, direction = "forward")
summary(spending_forward)

##BAckward stepwise elimination
spending_backward <- step(model_spending, direction = "backward")
summary(spending_backward)

## stepwise variable selection using BIC
spending_bic <- step(model_spending, k = log(nrow(npspc_data)))
summary(spending_bic)

########################### Purchase - ModelSelection fitting with selected Variables #############################
# List of predictor variables based on your backward model
variables_to_include <- c("source_a", "source_e", "source_o", "source_h", "source_r", 
                          "source_t", "source_u", "source_p", "source_x", "source_w", 
                          "Freq", "Web.order", "Address_is_res", "Purchase")

# Creating the subset from the training data
training_subset <- training_data[variables_to_include]
names(training_subset)

purchase_glm <- glm(Purchase ~ ., data = training_subset, family = binomial(), control = glm.control(maxit = 50))
summary(purchase_glm)

# Model Evaluation
predicted_probabilities <- predict(purchase_glm, newdata = validation_data, type = "response")

# Convert probabilities to binary class predictions based on a threshold (e.g., 0.5)
threshold <- 0.5
predicted_classes <- ifelse(predicted_probabilities > threshold, 1, 0)

conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c("1", "0")), factor(validation_data$Purchase, levels = c("1", "0")))
conf_matrix

##########  Decision tree for purchase. ##
purchase_tree <- rpart(Purchase ~ ., data = training_subset, method = "class")
summary(purchase_tree)
rpart.plot(purchase_tree, extra = 1,  fallen.leaves = TRUE)

# predictio on validation data
predicted_treeclasses <- predict(purchase_tree, newdata = validation_data, type = "class")

# Confusion Matrix
conf_matrix_rtree <- confusionMatrix(factor(predicted_treeclasses, levels = c("1", "0")), factor(validation_data$Purchase, levels = c("1", "0")))
conf_matrix_rtree

####### naive bayes #######
purchase_nb <- naiveBayes(Purchase ~ ., data = training_subset)

# Display model output
print(purchase_nb)

# Identifying a-priori probability for each outcome variable class
#purchase_nb_priori_probs <- purchase_nb$apriori
# cat("A-priori probabilities:\n")
# print(purchase_nb_priori_probs)


# classifying the new data
#purchase_pred.prob <- predict(purchase_nb, newdata = validation_data, type = "raw")
purchase_pred.class <- predict(purchase_nb, newdata = validation_data)

# Confusion Matrix
conf_matrix_nb <- confusionMatrix(factor(purchase_pred.class, levels = c("1", "0")),
                                  factor(validation_data$Purchase, levels = c("1", "0")))
conf_matrix_nb
######## KNN for purchase ######
# Convert 'Purchase' to a factor with explicit levels
training_subset$Purchase <- factor(training_subset$Purchase, levels = c("1", "0"))

# Define the control using a repeated k-fold cross-validation
#trainControl <- trainControl(method="cv", number=10)
set.seed(20)
# Train the model to find the best k
knnFit <- train(Purchase ~ ., data=training_subset,
                method="knn",
                tuneLength=20, # Tries 20 different values of k
                trControl=trainControl(method="cv", number=10),
                preProcess = c("center", "scale")) # Preprocessing steps

# Print the best k value
print(knnFit$bestTune)
print(knnFit)
plot(knnFit)

# train k-NN model with k=5
model_knn <- train(Purchase ~ ., data=training_subset,
               method="knn",  # specify the model
               preProcess=c("center", "scale"),  # normalize data
               tuneGrid=expand.grid(k=5),
               trControl=trainControl(method="none"))

# predict new data
knn_pred <- predict(model_knn, validation_data)

conf_matrix_knn <- confusionMatrix(factor(knn_pred, levels = c("1", "0")), factor(validation_data$Purchase, levels = c("1", "0")))
conf_matrix_knn
## Predicting on holdout data 
# Evaluating model perfomance on holdout data and adding a new column
PurchaseProbability_pred <- predict(purchase_glm, newdata = holdout_data, type = "response")

holdout_data$PredictedPurchase_Prob <- PurchaseProbability_pred

# Convert predicted probabilities to binary outcomes with a cutoff threshold
holdout_data$PredictedPurchase <- ifelse(holdout_data$PredictedPurchase_Prob > 0.5, 1, 0)
summary(holdout_data$PredictedPurchase)

# predicted classes
predicted_purchases <- holdout_data$PredictedPurchase
# Actual classes
actual_classes_hd <- holdout_data$Purchase

conf_matrix_purchase <- confusionMatrix(factor(predicted_purchases, levels = c("1", "0")), factor(actual_classes_hd, levels = c("1", "0")))
conf_matrix_purchase

############# SPending - Multiple Linear Regression model on data  ###########
# For purchase model with significant attributes chosen by backward Step wise elimination method
# List of variables to include in the subset
variables_to_include <- c("source_a", "source_c", "source_o", "source_h", "source_r", 
                          "source_u", "source_w", "Freq", "last_update_days_ago", "Gender.male", "Address_is_res", "Spending")

# Creating the subset from the training data
training_subset_lm <- training_data[variables_to_include]

# Check the structure of the new subset to ensure it's correctly created
str(training_subset_lm)

## Linear Regression
## Training model with selected variables
spending_lm <- lm(Spending ~., data = training_subset_lm)
summary(spending_lm )

Spending_predict <- predict(spending_lm, newdata = validation_data)
summary(Spending_predict)

# accuracy_results <- forecast::accuracy(Spending_predict, validation_data$Spending)
# accuracy_results

# Check for zero values
zero_values_sp <- sum(validation_data$Spending == 0)
cat("Number of zero values in actual spending:", zero_values_sp, "\n")

# Check for negative values
negative_values <- sum(validation_data$Spending < 0)
cat("Number of negative values in actual spending:", negative_values, "\n")

ss_mae <- mean(abs(Spending_predict - validation_data$Spending))
ss_rmse <- sqrt(mean((Spending_predict - validation_data$Spending)^2))

ss_total <- sum((validation_data$Spending - mean(validation_data$Spending))^2)
ss_res <- sum((validation_data$Spending - Spending_predict)^2)
r_squared <- 1 - (ss_res / ss_total)

correlation <- cor(validation_data$Spending, Spending_predict)

cat("Mean Absolute Error:", ss_mae)
cat("Root Mean Squared Error:", ss_rmse, "\n")
cat("R-sqaured value:", r_squared)
cat("Correlation between Actual and Predicted:", correlation)

# actual vs predicted
plot(validation_data$Spending, Spending_predict, xlab = "Actual Spending", ylab = "Predicted Spending")
abline(0, 1, col = "red") # Adds a y=x line for reference

# actual vs residuals
residuals <- validation_data$Spending - Spending_predict
plot(validation_data$Spending, residuals, xlab = "Actual Spending", ylab = "Residuals")
abline(h = 0, col = "red") # Adds a horizontal line at 0 for reference

######################### R tree for SPending #####################
spending_tree <- rpart(Spending ~ ., data = training_subset_lm)
spending_tree

summary(spending_tree)
rpart.plot(spending_tree, extra = 1,  fallen.leaves = TRUE)
#yesno = 2, type = 3,

# predictio on validation data
predicted_spending_tree <- predict(spending_tree, newdata = validation_data)

tree_mae <- mean(abs(predicted_spending_tree - validation_data$Spending))

tree_rmse <- sqrt(mean((predicted_spending_tree - validation_data$Spending)^2))

tree_ss_total <- sum((validation_data$Spending - mean(validation_data$Spending))^2)
tree_ss_res <- sum((validation_data$Spending - predicted_spending_tree)^2)
tree_r_squared <- 1 - (tree_ss_res / tree_ss_total)

tree_correlation <- cor(validation_data$Spending, predicted_spending_tree)


cat("Mean Absolute Error:", tree_mae)
cat("Root Mean Squared Error:", tree_rmse, "\n")
cat("R-sqaured value:", tree_r_squared)
cat("Correlation between Actual and Predicted:", tree_correlation)

# Actual vs Predicted
plot(validation_data$Spending, predicted_spending_tree, xlab = "Actual Spending", ylab = "Predicted Spending")
abline(0, 1, col = "red") # Adds a y=x line for reference

# Actual vs Residuals
tree_residuals <- validation_data$Spending - predicted_spending_tree
plot(validation_data$Spending, tree_residuals, xlab = "Actual Spending", ylab = "Residuals")
abline(h = 0, col = "red") # Adds a horizontal line at 0 for reference

###############################client  REquirements ##################################

# North-Point has supplied its customer list of 200,000 names to the pool, which totals over 5,000,000 names, 
# so it is now entitled to pick 200,000 names for a mailing.
# North-Point would like to select the name that have the best chance of performing well, 
# so it conduct a test – it draws 20,000 names from the pool and does a test mailing of the new list.
# This mailing yielded 1065 purchasers, a response rate of 0.053 or 5.3%. 
# To optimize the performance of the machine learning models, it was decided to work with a 
# stratified sample that contained equal number of purchasers and non-purchasers. For ease of 
# presentation, the data set in this project includes just 1000 purchasers and 1000 non-purchasers, 
# an apparent response rate of 0.5 or 50%. Therefore, after using the dataset to predict who will be
# a purchaser, we must adjust the purchase rate back down by multiplying each “case’s probability of
# purchase” by 5.3/50 or 0.106.
# 


# 1.Each list booklet costs approximately $2 to mail, which includes printing, postage, and mailing
# other mailing costs). Estimate the gross profit that the firm could expect from remaining 180,000 
# observations (customers) if it selects randomly from the pool.

Total_Spending <- sum(npspc_data$Spending)
Avg_spending <- Total_Spending/1000 
Avg_spending 

Predicted_Purchasers <- 180000*0.053
Predicted_Purchasers

#######################################################################

########################## Using Holdout data #########################
# a.Add a column to the data frame with the predicted probability of purchase from your selected
# machine learning model in step 2.
# # Evaluating model perfomance on holdout data and adding a new column
# PurchaseProbability_pred <- predict(purchase_glm, newdata = holdout_data, type = "response")
# holdout_data$PredictedPurchase_Prob <- PurchaseProbability_pred
# str(holdout_data)
# # Convert predicted probabilities to binary outcomes with a cutoff threshold
# holdout_data$PredictedPurchase <- ifelse(holdout_data$PredictedPurchase_Prob > 0.5, 1, 0)
# summary(holdout_data$PredictedPurchase)
# # predicted classes
# predicted_purchases <- holdout_data$PredictedPurchase
# # Actual classes
# actual_classes_hd <- holdout_data$Purchase
# conf_matrix_purchase <- confusionMatrix(factor(predicted_purchases, levels = c("1", "0")), factor(actual_classes_hd, levels = c("1", "0")))
# conf_matrix_purchase
# head(holdout_data)

# b.Add another column with predicted spending value from the work in step 3.
spending_predicted <- predict(spending_lm, newdata = holdout_data)
summary(spending_predicted)

holdout_data$predictedSpending <- spending_predicted

hd_mae <- mean(abs(spending_predicted - holdout_data$Spending))

hd_rmse <- sqrt(mean((spending_predicted - holdout_data$Spending)^2))

hd_total <- sum((holdout_data$Spending - mean(holdout_data$Spending))^2)
hd_res <- sum((holdout_data$Spending - spending_predicted)^2)
hd_r_squared <- 1 - (hd_res / hd_total)

correlation_hd <- cor(holdout_data$Spending, spending_predicted)


cat("Mean Absolute Error:", hd_mae)
cat("Root Mean Squared Error:", hd_rmse, "\n")
cat("R-sqaured value:", hd_r_squared)
cat("Correlation between Actual and Predicted:", correlation_hd)


plot(holdout_data$Spending, spending_predicted, xlab = "Actual Spending", ylab = "Predicted Spending")
abline(0, 1, col = "red") # Adds a y=x line for reference


residuals_hd <- holdout_data$Spending - spending_predicted
plot(holdout_data$Spending, residuals_hd, xlab = "Actual Spending", ylab = "Residuals")
abline(h = 0, col = "red") # Adds a horizontal line at 0 for reference

# holdout_data$predicted_spending <- Spending_predict
summary(holdout_data)
head(holdout_data)

# c.Add a column for “adjusted probability of purchase” to adjust for oversampling the purchaser 
# (see #1). This can be done by multiplying “predicted probability of purchase” by 
#   original purchase rate (1065/20000/0.50=0.1065). This is to adjust for over-sampling the purchases. 
originalPurchaseRate <- 1065 / 20000 / 0.50
adjustedPurchaseProb <- holdout_data$PredictedPurchase_Prob * originalPurchaseRate

holdout_data$adjustedPurchaseProb <- adjustedPurchaseProb
head(holdout_data)

#  d.Add another column for expected spending. This should be the adjusted spending – adjusting predicted spending of 4b by adjusted probability of 4c.
expectedSpending <- holdout_data$predictedSpending * holdout_data$adjustedPurchaseProb

holdout_data$expectedSpending <- expectedSpending

head(holdout_data,3)
summary(expectedSpending)
#  e.Plot the cumulative gain chart for the expected spending (cumulative expected spending as a function of records targeted).

# Calculate gains
gain <- gains(holdout_data$expectedSpending, holdout_data$expectedSpending)

# Cumulative gains chart
cumulative_df <- data.frame(
  cases = c(0, gain$cume.obs),
  cumulative_spending = c(0, gain$cume.pct.of.total * sum(holdout_data$expectedSpending))
)

cumulative_plot <- ggplot(cumulative_df, aes(x = cases, y = cumulative_spending)) +
  geom_line() +
  geom_line(data = data.frame(cases = c(0, nrow(holdout_data)), cumulative_spending = c(0, sum(holdout_data$expectedSpending))),
            color = "gray", linetype = 2) + # adds baseline
  labs(x = "# Cases", y = "Cumulative expected spending", title = "Cumulative gains chart") +
  scale_y_continuous(labels = scales::comma)

# Decile-wise lift chart
lift_df <- data.frame(
  percentile = gain$depth,
  mean_response = gain$mean.resp / mean(holdout_data$expectedSpending)
)

lift_plot <- ggplot(lift_df, aes(x = percentile, y = mean_response)) +
  geom_bar(stat = "identity") +
  labs(x = "Percentile", y = "Decile mean / global mean", title = "Decile-wise lift chart")

# Arrange plots
grid.arrange(cumulative_plot, lift_plot, ncol = 2)
