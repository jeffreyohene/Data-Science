# Load libraries
library(pacman)
p_load(tidyverse, caTools, caret, pROC, 
       tidyr, reshape2, gridExtra, randomForest)

dir()
# Options
options(scipen = 999)

# Load data
df <- read_csv('Bank Customer Churn Prediction.csv')


# Data Preprocessing
str(df)
summary(df)

# Checking for NA values
any_na <- any(is.na(df))

# Correct data conversion
df$country <- factor(df$country)
df$gender <- factor(df$gender)
df$tenure <- factor(df$tenure)
df$products_number <- factor(df$products_number)
df$credit_card <- factor(df$credit_card)
df$active_member <- factor(df$active_member)
df$churn <- factor(df$churn)


summary(df)

# EXPLORATORY DATA ANALYSIS

# 1. Histograms.
# Variables: Age, Credit Score, Balance, Account Balance, Estimated Salary & 
# Number of Products

# Age
ggplot(df,
       aes(
         x = age
       )) +
  geom_histogram(fill = 'darkgreen', color= 'white')+
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
  

# Credit Score
ggplot(df,
       aes(
         x = credit_score
       )) +
  geom_histogram(fill = 'darkgreen', color= 'white')+
  labs(title = "Distribution of Credit Score", 
       x = "Credit Score",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Balance
ggplot(df,
       aes(
         x = balance
       )) +
  geom_histogram(fill = 'darkgreen', color= 'white')+
  labs(title = "Distribution of Account Balance",
       x = "Account Balance",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Visualize accounts with balance greater than zero
df |>
  filter(balance > 0) |>
  ggplot(
    aes(
      x = balance
    )
  ) +
  geom_histogram(fill = 'darkgreen', color= 'white')+
  labs(title = "Distribution of Account Balance",
       x = "Account Balance",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Estimated Salary
ggplot(df,
       aes(
         x = estimated_salary
       )) +
  geom_histogram(bins = 20, fill = 'darkgreen', color= 'white')+
  labs(title = "Distribution of Salary Estimates",
       x = "Estimated Salary",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# 2. Barplots
# Variables: Country, Credit Card, Gender & Active Member

# Country
ggplot(df,
       aes(
         x = country
       )) +
  geom_bar(fill='salmon',color= 'black') +
  labs(title = "Distribution of Customers by Country",
       x = "Country",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Number of Bank Products
ggplot(df,
       aes(
         x = products_number
       )) +
  geom_bar(fill = 'darkgreen', color= 'white')+
  labs(title = "Distribution of Products",
       x = "Number of Products",
       y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Credit Card
df$credit_card <- factor(df$credit_card)

ggplot(df,
       aes(
         x = credit_card
         )) +
  geom_bar(fill = 'salmon', color = 'black')+
  labs(
    title = "Distribution of Customers by Credit Card Ownership",
     x = "Credit Card",
     y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# Active Member

ggplot(df,
       aes(
         x = active_member
       )) +
  geom_bar(fill = 'salmon', color = 'black')+
  labs(
    title = "Distribution of Customers by Account Status",
    x = "Status",
    y = "Frequency") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# 3. Boxplots
# Variables:
# a. Churn by Country
# b. Churn by Age & Gender

ggplot(df, aes(x = country, fill = factor(churn))) +
  geom_bar(position = position_dodge(), color='black') +
  labs(title = "Churn Distribution by Country",
       x = "Country",
       y = "Count") +
  scale_fill_manual(values = c("0" = 'darkgreen', '1' = 'salmon'),
                    name = "Churn",
                    labels = c("Did Not Churn", "Churned")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# By Gender
ggplot(df, aes(x = gender, fill = factor(churn))) +
  geom_bar(position = position_dodge(), color='black') +
  labs(title = 'Churn Distribution by Gender',
       x = 'Gender',
       y = 'Count') +
  scale_fill_manual(values = c("0" = 'darkgreen', "1" = 'salmon'),
                    name = "Churn",
                    labels = c("Did Not Churn", "Churned")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Dashboard
# Churn by Age
p1 <- ggplot(df,
       aes(
         x = churn,
         y = age
       )) +
  geom_boxplot(fill = c("salmon", "lightgreen"), color= 'black')+
  labs(x = "Churn",
       y = "Age") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# Churn by Credit Score
p2 <- ggplot(df,
       aes(
         x = churn,
         y = credit_score
       )) +
  geom_boxplot(fill = c("salmon", "lightgreen"), color= 'black')+
  labs(x = "Churn",
       y = "Credit Score") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Churn by Balance
p3 <- ggplot(df,
       aes(
         x = churn,
         y = balance
       )) +
  geom_boxplot(fill = c("salmon", "lightgreen"), color= 'black')+
  labs(x = "Churn",
       y = "Account Balance") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Churn by Estimated Salary
p4 <- ggplot(df,
       aes(
         x = churn,
         y = estimated_salary
       )) +
  geom_boxplot(fill = c("salmon", "lightgreen"), color= 'black')+
  labs(x = "Churn",
       y = "Estimated Salary") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Combine plots using grid.arrange
grid.arrange(p1, p2, p3,p4, ncol = 2)


################################################################################
summary(df)

#Balance
ggplot(df,
       aes(churn,balance))+
  geom_boxplot()

# Calculate the median of the balance column
median_balance <- median(df$balance)

# Replace zeros with the median value
df$balance[df$balance == 0] <- median_balance

# Verify the changes
summary(df)

# visual check
ggplot(df,
       aes(churn,balance))+
  geom_boxplot()



# Set a seed for reproducibility
set.seed(321)

# Split the data into 70% training and 30% testing
split <- sample.split(df$churn, SplitRatio = 0.7)
train <- subset(df, split == T)
test <- subset(df, split == F)

# Define predictor variables and the target variable
predictors <- c("credit_score", "age", "tenure", "balance", "products_number", 
                "credit_card", "active_member", "estimated_salary")

target <- "churn"

# Perform logistic regression
m1 <- glm(formula = paste(target, "~", paste(predictors, 
                                                       collapse = "+")), 
                    family = "binomial", data = train)

# Print summary of the model
summary(m1)

# Make predictions on the test data
test_predictions <- predict(m1, newdata = test, type = "response")

# Convert probabilities to binary predictions (0 or 1)
test_predictions_binary <- ifelse(test_predictions > 0.5, 1, 0)

# Evaluate the model's performance
conf_matrix <- confusionMatrix(table(test_predictions_binary, test$churn))
print(conf_matrix)

# Calculate accuracy, precision, recall, and F1-score
accuracy <- conf_matrix$overall["Accuracy"] * 100
precision <- conf_matrix$byClass["Pos Pred Value"] * 100
recall <- conf_matrix$byClass["Sensitivity"] * 100
f1_score <- conf_matrix$byClass["F1"] * 100

# Print evaluation metrics
print(paste("Accuracy:", accuracy, "%"))
print(paste("Precision:", precision, "%"))
print(paste("Recall:", recall,"%"))
print(paste("F1-Score:", f1_score, "%"))


# Train the Random Forest model
rf_model <- randomForest(churn ~ ., data = train[, c(predictors, "churn")])

# Make predictions on the test set
rf_predictions <- predict(rf_model, test[, c(predictors, "churn")])

# Evaluate the model's performance
conf_matrix_rf <- confusionMatrix(table(rf_predictions, test$churn))
accuracy_rf <- conf_matrix_rf$overall["Accuracy"] * 100
precision_rf <- conf_matrix_rf$byClass["Pos Pred Value"] * 100
recall_rf <- conf_matrix_rf$byClass["Sensitivity"] * 100
f1_score_rf <- conf_matrix_rf$byClass["F1"] * 100

# model tree node distribution
hist(treesize(rf_model), col="dodgerblue",
     main = "Number of Tree Nodes")

# Get variable importance from the Random Forest model
variable_importance_rf <- importance(rf_model)

# Print the variable importance
print("Random Forest Variable Importance:")
print(variable_importance_rf)
varImpPlot(rf_model)

# Print evaluation metrics
print(paste("Random Forest Model Metrics:"))
print(paste("Accuracy:", accuracy_rf, "%"))
print(paste("Precision:", precision_rf, "%"))
print(paste("Recall:", recall_rf, "%"))
print(paste("F1-Score:", f1_score_rf, "%"))




