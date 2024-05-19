attach(data)
summary(data)

# Check missing values
missing_counts <- colSums(is.na(data))

# Output missing value counts
print(missing_counts)

# Remove specified columns using subset()
columns_to_remove <- c("PassengerId", "Ticket", "Cabin", "WikiId", 
                       "Name_wiki", "Age_wiki", "Hometown", 
                       "Boarded", "Destination", "Lifeboat", 
                       "Body", "Class")

data_cleaned <- subset(data, select = -which(names(data) %in% columns_to_remove))

# Print the cleaned dataset
head(data_cleaned)

# Check missing values again
missing_counts_again<- colSums(is.na(data_cleaned))

# Output missing value counts
print(missing_counts_again)

library(mice)

# Perform multiple imputation
imputed_data <- mice(data_cleaned, method = "pmm", m = 5)  
titanic <- complete(imputed_data)  

# Check missing values again
missingvalues<- colSums(is.na(titanic))

# Output missing value counts
print(missingvalues)

#Remove embarked missing values
titanic <- na.omit(titanic)

# Check missing values again
missingvalues_again<- colSums(is.na(titanic))

# Output missing value counts
print(missingvalues_again)

# Age classification
titanic$Age_Category[titanic$Age < 20] <- 'Below 20'
titanic$Age_Category[titanic$Age >= 20 & titanic$Age < 40] <- 'Between 20-40'
titanic$Age_Category[titanic$Age >= 40 & titanic$Age < 60] <- 'Between 40-60'
titanic$Age_Category[titanic$Age >= 60] <- 'Over 60'

# Create variable for family size
titanic$Family_Size <- titanic$SibSp + titanic$Parch + 1

titanic$Family_Class[titanic$Family_Size == 1] <- 'Single'
titanic$Family_Class[titanic$Family_Size <= 2 & titanic$Family_Size > 1] <- 'Small'
titanic$Family_Class[titanic$Family_Size < 4 & titanic$Family_Size > 2] <- 'Medium'
titanic$Family_Class[titanic$Family_Size >= 4] <- 'Large'

# Extract titles from Name using regular expressions
titanic$Title <- gsub('(.*, )|(\\..*)', '', titanic$Name)
# Replace less frequent titles with 'Other' directly in the 'Title' column
less_frequent_titles <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", 
                          "Mlle", "Mme", "Ms", "Rev", "Sir", "the Countess")

# Replace less frequent titles with 'Other' in 'Title'
titanic$Title[titanic$Title %in% less_frequent_titles] <- "Other"

# Display the table of counts for the updated 'Title' column
table(titanic$Title)

# Changes on categorical variables
titanic$Pclass <- mapvalues(titanic$Pclass, 
                            from = c(1, 2, 3), 
                            to = c("First", "Second", 
                                   "Third"))

titanic$Family_Size <- mapvalues(titanic$Family_Size, 
                                 from = c(0, 1, 2, 3, 4, 5, 6, 7, 8,9,10,11), 
                                 to = c("Zero", "One", "Two", "Three", 
                                        "Four", "Five", "Six", "Seven", 
                                        "Eight","Nine","Ten","Eleven"))
titanic <- mutate(titanic, Survived = if_else (Survived== 0, "Non-Survival", "Survival"))

titanic <- subset(titanic, select = -c(Name,Age,SibSp,Parch,Family_Size))

titanic$Survived=as.factor(titanic$Survived)
titanic$Pclass=as.factor(titanic$Pclass)
titanic$Sex=as.factor(titanic$Sex)
titanic$Embarked=as.factor(titanic$Embarked)
titanic$Age_Category=as.factor(titanic$Age_Category)
titanic$Family_Class=as.factor(titanic$Family_Class)
titanic$Title=as.factor(titanic$Title)

summary(titanic)
head(titanic)

library(clustMixType)

# Define categorical columns
categorical_cols <- c("Pclass", "Sex", "Embarked", "Age_Category", "Family_Class", "Title")

# Convert categorical columns to factors
titanic[categorical_cols] <- lapply(titanic[categorical_cols], as.factor)

# Handle NA values if present
titanic <- na.omit(titanic)  

# Prepare data for clustering 
data_for_clustering <- titanic[, c("Pclass", "Sex", "Fare", "Embarked", "Age_Category", "Family_Class", "Title")]

# Specify the range of clusters to evaluate
k_range <- 2:6

# Perform silhouette analysis to determine the optimal number of clusters
silhouette_values <- sapply(k_range, function(k) {
  set.seed(123) 
  kmproto <- kproto(data_for_clustering, k, max.iter = 100, num.cat.vars = length(categorical_cols))
  
  # Check if clustering was successful
  if (!is.null(kmproto$cluster)) {
    # Calculate Gower distance matrix
    dist_matrix <- as.matrix(daisy(data_for_clustering, metric = "gower"))
    
    # Calculate silhouette width
    silhouette_avg <- mean(silhouette(kmproto$cluster, dist_matrix))
    return(silhouette_avg)
  } else {
    return(NA) 
  }
})

# Plot silhouette values for different numbers of clusters
plot(k_range, silhouette_values, type = "b", xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# Identify the optimal number of clusters based on the highest silhouette value
optimal_clusters <- which.max(silhouette_values) + 1
cat("Optimal number of clusters:", optimal_clusters, "\n")

# Perform clustering with the optimal number of clusters
set.seed(123)  
final_clusters <- kproto(data_for_clustering, optimal_clusters, max.iter = 100, num.cat.vars = length(categorical_cols))

# Add cluster labels back to the original dataset
titanic$Cluster <- final_clusters$cluster

# Explore the cluster assignments
table(titanic$Cluster)

# Initialize an empty list to store cluster summaries
cluster_summary <- list()

# Iterate over each cluster ID
for (cluster_id in unique(titanic$Cluster)) {
  # Subset data for the current cluster
  cluster_data <- titanic[titanic$Cluster == cluster_id, ]
  
  # Calculate variable frequencies within the cluster
  variable_frequencies <- lapply(cluster_data[, categorical_cols], table)
  
  # Prepare a list to store summary data for the current cluster
  cluster_summary_data <- list()
  
  # Iterate over each categorical variable
  for (variable_name in names(variable_frequencies)) {
    # Extract frequencies for the current variable
    frequencies <- variable_frequencies[[variable_name]]
    
    # Create a data frame for the current variable within the cluster
    variable_summary <- data.frame(
      Cluster = rep(cluster_id, length(frequencies)),  
      Variable = rep(variable_name, length(frequencies)), 
      Category = names(frequencies), 
      Frequency = as.vector(frequencies) 
    )
    
    # Append variable summary to the list
    cluster_summary_data[[variable_name]] <- variable_summary
  }
  
  # Combine variable summaries into a single data frame for the cluster
  cluster_summary_df <- do.call(rbind, cluster_summary_data)
  
  # Append the cluster summary to the list
  cluster_summary[[as.character(cluster_id)]] <- cluster_summary_df
}

# Combine all cluster summaries into a single data frame
final_cluster_summary_df <- do.call(rbind, cluster_summary)

# Print the summarized data
print(final_cluster_summary_df)

library(ggplot2)

# Calculate counts of survivors and non-survivors within each cluster
survival_counts <- table(titanic$Cluster, titanic$Survived)

# Convert the survival counts to a data frame
survival_counts_df <- as.data.frame.matrix(survival_counts)
survival_counts_df$Cluster <- rownames(survival_counts_df)

# Reshape the data for plotting
survival_counts_long <- tidyr::pivot_longer(survival_counts_df, 
                                            cols = c("Non-Survival", "Survival"),
                                            names_to = "Survived",
                                            values_to = "Count")

# Plotting the survival counts by cluster
ggplot(survival_counts_long, aes(x = Cluster, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Count), vjust = -0.5, color = "white", size = 3.5) +  
  labs(title = "Survival Status Within Clusters",
       x = "Cluster", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate counts of survivors and non-survivors within each cluster
survival_counts <- table(titanic$Cluster, titanic$Survived)

# Display the table
print(survival_counts)

library(dplyr)

# Define the cluster-purity calculation function
calculate_cluster_purity <- function(cluster_data) {
  # Create a contingency table of Survived vs Non-Survived within each cluster
  cluster_table <- table(cluster_data$Cluster, cluster_data$Survived)
  
  # Calculate the total number of passengers in each cluster
  cluster_totals <- rowSums(cluster_table)
  
  # Calculate purity for each cluster
  cluster_purity <- sapply(1:nrow(cluster_table), function(i) {
    max(cluster_table[i, ]) / cluster_totals[i]
  })
  
  # Return the cluster purity values
  return(cluster_purity)
}

# Calculate cluster purity
cluster_purity <- calculate_cluster_purity(titanic)

# Print cluster purity values
cat("Cluster Purity:\n")
for (i in 1:length(cluster_purity)) {
  cat("Cluster", i, ": ", round(cluster_purity[i], 3), "\n")
}

# Load required libraries
library(caret)
library(dplyr)
library(leaps)

# Step 1: Group Clusters
# Define group assignments based on cluster purity values
group1 <- c(5, 6)  # Clusters 3 and 6
group2 <- c(3)  # Clusters 1, 2, and 4
group3 <- c(1,2,4)  # Cluster 5

# Step 2: Split Data into Training and Testing Sets
# Function to split data into training and testing sets for a given group
split_data <- function(data, group) {
  group_data <- filter(data, Cluster %in% group)
  set.seed(123)  
  train_indices <- createDataPartition(group_data$Survived, p = 0.7, list = FALSE)
  train_data <- group_data[train_indices, ]
  test_data <- group_data[-train_indices, ]
  list(train_data = train_data, test_data = test_data)
}

# Split data for Group 1
group1_data <- split_data(titanic, group1)
train_data_group1 <- group1_data$train_data
test_data_group1 <- group1_data$test_data

# Split data for Group 2
group2_data <- split_data(titanic, group2)
train_data_group2 <- group2_data$train_data
test_data_group2 <- group2_data$test_data

# Split data for Group 3
group3_data <- split_data(titanic, group3)
train_data_group3 <- group3_data$train_data
test_data_group3 <- group3_data$test_data

# Step 3: Model Selection and Cross-Validation (Forward Selection)
# Function to perform forward selection and cross-validation
model_selection <- function(train_data) {
  control <- trainControl(method = "cv", number = 5)
  fit <- train(Survived ~ ., data = train_data, method = "glm", family = "binomial",
               trControl = control, tuneLength = 10, preProcess = c("center", "scale", "zv"))
  fit
}

# Perform model selection and cross-validation for Group 1
model_group1 <- model_selection(train_data_group1)

# Perform model selection and cross-validation for Group 2
model_group2 <- model_selection(train_data_group2)

# Perform model selection and cross-validation for Group 3
model_group3 <- model_selection(train_data_group3)

# Function to fit logistic regression model
fit_logistic_regression <- function(train_data, selected_vars) {
  # Check if selected_vars exist in train_data
  selected_vars <- intersect(selected_vars, colnames(train_data))
  
  if (length(selected_vars) == 0) {
    # Handle case where no selected variables are available
    cat("No valid selected variables for logistic regression.\n")
    return(NULL)
  }
  
  formula <- as.formula(paste("Survived ~", paste(selected_vars, collapse = " + ")))
  glm_model <- glm(formula, data = train_data, family = "binomial")
  glm_model
}

# Get selected variables from forward selection results
selected_vars_group1 <- names(which(model_group1$finalModel$coefficients != 0)[-1])
selected_vars_group2 <- names(which(model_group2$finalModel$coefficients != 0)[-1])
selected_vars_group3 <- names(which(model_group3$finalModel$coefficients != 0)[-1])

# Fit logistic regression model for Group 1
logistic_model_group1 <- fit_logistic_regression(train_data_group1, selected_vars_group1)

# Fit logistic regression model for Group 2
logistic_model_group2 <- fit_logistic_regression(train_data_group2, selected_vars_group2)

# Fit logistic regression model for Group 3
logistic_model_group3 <- fit_logistic_regression(train_data_group3, selected_vars_group3)

# Step 5: Evaluate Model Accuracy
# Convert 'Survived' column to binary (0 or 1)
test_data_group1$Survived <- ifelse(test_data_group1$Survived == "Survival", 1, 0)
test_data_group2$Survived <- ifelse(test_data_group2$Survived == "Survival", 1, 0)
test_data_group3$Survived <- ifelse(test_data_group3$Survived == "Survival", 1, 0)

# Function to evaluate model accuracy
evaluate_accuracy <- function(model, test_data) {
  predicted <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predicted > 0.5, 1, 0)
  accuracy <- mean(predicted_class == test_data$Survived)
  accuracy
}

# Evaluate accuracy for logistic regression models
accuracy_group1 <- evaluate_accuracy(logistic_model_group1, test_data_group1)
accuracy_group2 <- evaluate_accuracy(logistic_model_group2, test_data_group2)
accuracy_group3 <- evaluate_accuracy(logistic_model_group3, test_data_group2)

# Display accuracy results
cat("Accuracy for Group 1:", round(accuracy_group1, 3), "\n")
cat("Accuracy for Group 2:", round(accuracy_group2, 3), "\n")
cat("Accuracy for Group 3:", round(accuracy_group3, 3), "\n")

# Compare accuracy of models across groups
accuracy_results <- c(accuracy_group1, accuracy_group2, accuracy_group3)
group_names <- c("Group 1", "Group 2", "Group 3")

# Create a bar plot to visualize accuracy comparison
barplot(accuracy_results, names.arg = group_names, 
        main = "Accuracy Comparison of Logistic Regression Models",
        ylab = "Accuracy",
        col = "skyblue",
        ylim = c(0, 1))

# Function to extract coefficients and variable names
extract_coefficients <- function(model) {
  coef_df <- data.frame(variable = names(model$coefficients), coefficient = model$coefficients)
  coef_df
}

# Extract coefficients for each group's logistic regression model
coef_group1 <- extract_coefficients(logistic_model_group1)
coef_group2 <- extract_coefficients(logistic_model_group2)
coef_group3 <- extract_coefficients(logistic_model_group3)

# Print coefficients for each group
print("Coefficients for Group 1:")
print(coef_group1)
print("Coefficients for Group 2:")
print(coef_group2)
print("Coefficients for Group 3:")
print(coef_group3)

# Function to calculate confusion matrix metrics
calculate_confusion_matrix <- function(model, test_data) {
  predicted <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predicted > 0.5, 1, 0)
  confusion_matrix <- confusionMatrix(factor(predicted_class), factor(test_data$Survived))
  confusion_matrix
}

# Calculate confusion matrix for each group's logistic regression model
confusion_matrix_group1 <- calculate_confusion_matrix(logistic_model_group1, test_data_group1)
confusion_matrix_group2 <- calculate_confusion_matrix(logistic_model_group2, test_data_group2)
confusion_matrix_group3 <- calculate_confusion_matrix(logistic_model_group3, test_data_group3)

# Print confusion matrix metrics
print("Confusion Matrix for Group 1:")
print(confusion_matrix_group1$table)
print("Confusion Matrix for Group 2:")
print(confusion_matrix_group2$table)
print("Confusion Matrix for Group 3:")
print(confusion_matrix_group3$table)

# Function to calculate precision, recall, and F1 score
calculate_metrics <- function(confusion_matrix) {
  TP <- confusion_matrix[2, 2]  # True Positives
  FP <- confusion_matrix[1, 2]  # False Positives
  FN <- confusion_matrix[2, 1]  # False Negatives
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(list(precision = precision, recall = recall, f1_score = f1_score))
}

# Calculate metrics for Group 1
metrics_group1 <- calculate_metrics(confusion_matrix_group1$table)
metrics_group2 <- calculate_metrics(confusion_matrix_group2$table)
metrics_group3 <- calculate_metrics(confusion_matrix_group3$table)

print("Metrics for Group 1:")
print(metrics_group1)

print("Metrics for Group 2:")
print(metrics_group2)

print("Metrics for Group 3:")
print(metrics_group3)

# Define a function to calculate Mean Squared Error (MSE)
calculate_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Calculate MSE for Group 1
mse_group1 <- mean((test_data_group1$Survived - predicted_class_group1)^2)

# Calculate MSE for Group 2
mse_group2 <- mean((test_data_group2$Survived - predicted_class_group2)^2)

# Calculate MSE for Group 3
mse_group3 <- mean((test_data_group3$Survived - predicted_class_group3)^2)

# Display the MSE values
mse_group1
mse_group2
mse_group3


# Plot ROC curve and calculate AUC
library(pROC)

# Generate predicted probabilities or classes for each group
predicted_class_group1 <- predict(logistic_model_group1, newdata = test_data_group1, type = "response")
predicted_class_group2 <- predict(logistic_model_group2, newdata = test_data_group2, type = "response")
predicted_class_group3 <- predict(logistic_model_group3, newdata = test_data_group3, type = "response")

# Calculate ROC curves for each group
roc_group1 <- roc(test_data_group1$Survived, predicted_class_group1)
roc_group2 <- roc(test_data_group2$Survived, predicted_class_group2)
roc_group3 <- roc(test_data_group3$Survived, predicted_class_group3)

# Plot ROC curves
plot(roc_group1, main = "ROC Curve - Group 1", col = "blue")
plot(roc_group2, main = "ROC Curve - Group 2", col = "green", add = TRUE)
plot(roc_group3, main = "ROC Curve - Group 3", col = "red", add = TRUE)

# Calculate AUC for each group
auc_group1 <- auc(roc_group1)
auc_group2 <- auc(roc_group2)
auc_group3 <- auc(roc_group3)

# Print AUC values
print("Area Under the Curve (AUC) for Group 1:")
print(auc_group1)

print("Area Under the Curve (AUC) for Group 2:")
print(auc_group2)

print("Area Under the Curve (AUC) for Group 3:")
print(auc_group3)

