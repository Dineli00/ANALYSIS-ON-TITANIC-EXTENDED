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

library(plyr)
library(dplyr)

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


# Load necessary libraries
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
train_index <- createDataPartition(titanic$Survived, p = 0.7, list = FALSE)
train_data <- titanic[train_index, ]
test_data <- titanic[-train_index, ]

# Check the dimensions of the training and testing sets
dim(train_data)  
dim(test_data)   

library(ggplot2)

# Calculate survival counts
survival_counts <- table(train_data$Survived)

# Calculate survival percentages
survival_percentages <- prop.table(survival_counts) * 100

# Create a dataframe for plotting
survival_df <- data.frame(Survived = names(survival_percentages),
                          Count = as.numeric(survival_counts),
                          Percentage = as.numeric(survival_percentages))

# Plot
ggplot(survival_df, aes(x = "", y = Percentage, fill = factor(Survived))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(x = NULL, y = NULL, fill = "Survived") +
  scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Survival Distribution")

# Calculate survival count by gender
survival_gender <- table(train_data$Survived, train_data$Sex)
colnames(survival_gender) <- c("Female", "Male")

# Plotting survival count by gender
ggplot(data = as.data.frame.table(survival_gender), aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "Survived") +
  geom_text(aes(label = paste0(Freq, " (", round(Freq/sum(Freq)*100), "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Survival Count by Gender") +
  theme_minimal()

# Calculate survival count by passenger class
survival_class <- table(train_data$Survived, train_data$Pclass)
colnames(survival_class) <- c("1st Class", "2nd Class", "3rd Class")

# Plotting survival count by passenger class
ggplot(data = as.data.frame.table(survival_class), aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Passenger Class", y = "Count", fill = "Survived") +
  geom_text(aes(label = paste0(Freq, " (", round(Freq/sum(Freq)*100), "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Survival Count by Passenger Class") +
  theme_minimal()

# Plotting distribution of fare by survival
ggplot(train_data, aes(x = Survived, y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(x = "Survived", y = "Fare", fill = "Survived") +
  ggtitle("Distribution of Fare by Survival") +
  theme_minimal()

variables_of_interest <- c(train_data$Fare)
# Function to identify outliers using IQR method
identify_outliers <- function(x) {
  x <- na.omit(x)  
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  return(range(outliers))
}

# Loop through each variable for analysis
variables_of_interest <- c("Fare")  # Add other variables if needed
for (variable in variables_of_interest) {
  # Identify outliers
  outliers_range <- identify_outliers(train_data[[variable]])
  
  # Print range of outliers for the current variable
  cat("Outliers range for", variable, ":", toString(outliers_range), "\n")
}

# Fare
tapply(train_data$Fare,train_data$Survived,median)
tapply(train_data$Fare,train_data$Survived,IQR)

# Calculate survival count by embarkation port
survival_embark <- table(train_data$Survived, train_data$Embarked)

# Plotting survival count by embarkation port
ggplot(data = as.data.frame.table(survival_embark), aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Embarked Port", y = "Count", fill = "Survived") +
  geom_text(aes(label = paste0(Freq, " (", round(Freq/sum(Freq)*100), "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Survival Count by Embarked Port") +
  theme_minimal()

# Calculate survival count by age category
survival_age <- table(train_data$Survived, train_data$Age_Category)

# Plotting survival count by age category
ggplot(data = as.data.frame.table(survival_age), aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age Category", y = "Count", fill = "Survived") +
  geom_text(aes(label = paste0(Freq, " (", round(Freq/sum(Freq)*100), "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Survival Count by Age Category") +
  theme_minimal()

# Calculate survival count by family category
survival_family <- table(train_data$Survived, train_data$Family_Class)

# Plotting survival count by family category
ggplot(data = as.data.frame.table(survival_family), aes(x = Var2, y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Family Category", y = "Count", fill = "Survived") +
  geom_text(aes(label = paste0(Freq, " (", round(Freq/sum(Freq)*100), "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Survival Count by Family Category") +
  theme_minimal()

# Calculate survival count by title
survival_title <- table(train_data$Survived, train_data$Title)

# Convert the table to a data frame for plotting
survival_title_df <- as.data.frame.table(survival_title)
colnames(survival_title_df) <- c("Survived", "Title", "Count")

# Plotting survival counts by title
ggplot(data = survival_title_df, aes(x = Title, y = Count, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Title", y = "Count", fill = "Survived") +
  geom_text(aes(label = paste0(Count, " (", round(Count/sum(Count)*100, 1), "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Survival Counts by Title") +
  theme_minimal()

library(caret)
library(MASS)  

# Define a function for forward selection
forward_select <- function(train_data, response_col, predictors) {
  formula <- as.formula(paste(response_col, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, data = train_data, family = binomial)
  full_model <- stepAIC(model, direction = "forward", trace = FALSE)
  return(full_model)
}

# Set seed for reproducibility
set.seed(123)

# Specify the response column
response_col <- "Survived"

# Specify the predictor columns
predictor_cols <- c("Pclass", "Sex", "Fare", "Embarked", "Age_Category", "Family_Class", "Title")

# Initialize an empty vector to store selected predictors
selected_predictors <- c()

# Perform forward selection
for (i in 1:length(predictor_cols)) {
  remaining_predictors <- setdiff(predictor_cols, selected_predictors)
  candidate_models <- lapply(remaining_predictors, function(predictor) {
    forward_select(train_data, response_col, c(selected_predictors, predictor))
  })
  candidate_aics <- sapply(candidate_models, AIC)
  best_candidate <- which.min(candidate_aics)
  selected_predictors <- c(selected_predictors, remaining_predictors[best_candidate])
  cat("Selected predictors:", selected_predictors, "\n")
}

# Build the final model with the selected predictors
final_formula <- as.formula(paste(response_col, "~", paste(selected_predictors, collapse = " + ")))
final_model <- glm(final_formula, data = train_data, family = binomial)

# Obtain the coefficients of the final model
coefficients <- coef(final_model)
print(coefficients)

# Coefficients from the final model
coefficients <- c(
  `(Intercept)` = 17.726961234,
  `Sexmale` = -16.777181845,
  `PclassSecond` = -0.712612337,
  `PclassThird` = -1.864454500,
  `Age_CategoryBetween 20-40` = -0.675723341,
  `Age_CategoryBetween 40-60` = -0.948015512,
  `Age_CategoryOver 60` = -1.500760118,
  `TitleMiss` = -16.313436072,
  `TitleMr` = -1.274534458,
  `TitleMrs` = -16.159394870,
  `TitleOther` = -1.858639820,
  `Family_ClassMedium` = 1.018304310,
  `Family_ClassSingle` = 1.047079062,
  `Family_ClassSmall` = 0.954571760,
  `Fare` = 0.006111721,
  `EmbarkedQ` = -0.356689218,
  `EmbarkedS` = -0.417487293
)

# Create the logistic regression equation as a string
equation <- paste(
  "logit(p) = ",
  paste(
    sapply(names(coefficients), function(name) {
      coeff <- coefficients[name]
      if (name == "(Intercept)") {
        sprintf("%.6f", coeff)
      } else {
        sign <- ifelse(coeff >= 0, "+", "-")
        sprintf("%s %.6f * %s", sign, abs(coeff), name)
      }
    }),
    collapse = " "
  )
)

# Print the equation
cat(equation, "\n")

# Cross-validate the model
set.seed(123)
cv <- train(final_formula, data = train_data, method = "glm", trControl = trainControl(method = "cv", number = 5))
print(cv)

# Predict using the final model on test data
test_predictions <- predict(final_model, newdata = test_data, type = "response")

# Convert probabilities to class labels (0 or 1)
test_predicted_classes <- ifelse(test_predictions > 0.5, "Survival", "Non-Survival")

# Calculate accuracy
accuracy <- mean(test_predicted_classes == test_data$Survived)
cat("Accuracy:", accuracy, "\n")

# Confusion matrix
conf_matrix <- table(Actual = test_data$Survived, Predicted = test_predicted_classes)
print(conf_matrix)

# Convert confusion matrix to a data frame for plotting
conf_matrix_df <- as.data.frame.matrix(conf_matrix)
conf_matrix_df <- cbind(Actual = rownames(conf_matrix_df), conf_matrix_df)
rownames(conf_matrix_df) <- NULL

# Melt the data frame for plotting
conf_matrix_df <- reshape2::melt(conf_matrix_df, id.vars = "Actual")

# Plot confusion matrix 
ggplot(data = conf_matrix_df, aes(x = Actual, y = variable, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), vjust = 1) +
  scale_fill_gradient(low = "pink", high = "red") +  # Changing color palette
  labs(title = "Confusion Matrix",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()

# Calculate error measures
TP <- conf_matrix["Survival", "Survival"]
TN <- conf_matrix["Non-Survival", "Non-Survival"]
FP <- conf_matrix["Non-Survival", "Survival"]
FN <- conf_matrix["Survival", "Non-Survival"]

# Sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")

# Specificity (True Negative Rate)
specificity <- TN / (TN + FP)
cat("Specificity (True Negative Rate):", specificity, "\n")

# Precision
precision <- TP / (TP + FP)
cat("Precision:", precision, "\n")

# F1 Score
f1_score <- 2 * precision * sensitivity / (precision + sensitivity)
cat("F1 Score:", f1_score, "\n")

# Balanced accuracy
balanced_accuracy <- (sensitivity + specificity) / 2
cat("Balanced Accuracy:", balanced_accuracy, "\n")

# Misclassification rate (Error rate)
misclassification_rate <- 1 - accuracy
cat("Misclassification Rate:", misclassification_rate, "\n")

library(pROC)  
library(PRROC) 

# ROC curve
roc_curve <- roc(test_data$Survived, test_predictions)
plot(roc_curve, main = "ROC Curve", col = "orange")
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "orange", lwd = 2)

# Compute AUC for the ROC curve
roc_auc <- auc(roc_curve)
cat("ROC AUC:", roc_auc, "\n")

