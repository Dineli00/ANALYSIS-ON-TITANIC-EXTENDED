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
imputed_data <- mice(data_cleaned, method = "pmm", m = 5)  # Example using predictive mean matching (pmm)
titanic <- complete(imputed_data)  # Obtain completed dataset

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

library(randomForest)
library(rpart)

# Ensure the 'Survived' column is a factor
titanic$Survived <- factor(titanic$Survived, levels = c("Non-Survival", "Survival"))

# Split data into training (70%) and testing (30%) sets
set.seed(123) 
train_indices <- sample(1:nrow(titanic), 0.7 * nrow(titanic))
train_data <- titanic[train_indices, ]
test_data <- titanic[-train_indices, ]

# Check summary of training data
summary(train_data)

# Check summary of testing data
summary(test_data)

# Decision Tree Model
# Create a decision tree model using training data
model_tree <- rpart(Survived ~ Pclass + Sex + Age_Category + Fare + Embarked,
                    data = train_data, method = "class")

# Plot the decision tree
plot(model_tree)
text(model_tree)

# Random Forest Model
# Create a random forest model using training data
model_rf <- randomForest(Survived ~ Pclass + Sex + Age_Category + Fare + Embarked,
                         data = train_data, importance = TRUE)

# View model summary
print(model_rf)

# Extract confusion matrix from model
conf_matrix <- model_rf$confusion

# Print confusion matrix
print(conf_matrix)

# Calculate class error rates
class_error_non_survival <- conf_matrix[1, 3] / sum(conf_matrix[1, 1:2])
class_error_survival <- conf_matrix[2, 3] / sum(conf_matrix[2, 1:2])

# Print class error rates
cat("Class Error (Non-Survival):", class_error_non_survival, "\n")
cat("Class Error (Survival):", class_error_survival, "\n")

# Plot variable importance
varImpPlot(model_rf)

# Model Evaluation
# Make predictions using the random forest model on testing data
predictions <- predict(model_rf, test_data, type = "response")

# Confusion matrix
conf_matrix <- table(test_data$Survived, predictions)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy)

# Tune random forest parameters
model_rf_tuned <- randomForest(Survived ~ Pclass + Sex + Age_Category + Fare + Embarked,
                               data = train_data, 
                               ntree = 1000,      
                               mtry = 3,          
                               nodesize = 10,    
                               importance = TRUE)

# Evaluate the tuned model
predictions_tuned <- predict(model_rf_tuned, test_data, type = "response")
conf_matrix_tuned <- table(test_data$Survived, predictions_tuned)
accuracy_tuned <- sum(diag(conf_matrix_tuned)) / sum(conf_matrix_tuned)
cat("Tuned Model Accuracy:", accuracy_tuned, "\n")

# Calculate confusion matrix
conf_matrix <- table(test_data$Survived, predictions)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate error rates
error_non_survival <- conf_matrix[1, 2] / sum(conf_matrix[1, ])
error_survival <- conf_matrix[2, 1] / sum(conf_matrix[2, ])
cat("Error Rate (Non-Survival):", error_non_survival, "\n")
cat("Error Rate (Survival):", error_survival, "\n")

# Calculate precision, recall, and F1-score
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")

# Plot ROC curve and calculate AUC
library(pROC)
roc_curve <- roc(test_data$Survived, as.numeric(predictions == "Survival"))
auc <- auc(roc_curve)
plot(roc_curve)
cat("AUC:", auc, "\n")

# Plot variable importance for random forest model
varImpPlot(model_rf_tuned)

class(model_rf_tuned)

# Extract variable importance for random forest model
importance <- varImp(model_rf_tuned)
print(importance)

library(caret)

# Define cross-validation settings
ctrl <- trainControl(method = "cv", number = 10)

# Tune random forest model using cross-validation
tuned_model <- train(Survived ~ Pclass + Sex + Age_Category + Fare + Embarked,
                     data = train_data,
                     method = "rf",
                     trControl = ctrl,
                     tuneGrid = expand.grid(mtry = c(2, 3, 4)),
                     importance = TRUE)

# Print the tuned model
print(tuned_model)

# Extract and print variable importance of the tuned model
print(varImp(tuned_model))

