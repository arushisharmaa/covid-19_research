#01/17/2024: 
#have one column for the fold name, one column for the accuracy value and one column for what type of data it is 
#(ex: multinominal, logistic, regression)
library(randomForest)  # For random forest modeling
library(tidyverse)     # For data manipulation and visualization
library(nnet)          # For neural network modeling
library(caret)         # For the plotting models

# Function to perform Multinomial Logistic Regression and Random Forest with cross-validation
perform_cross_validation <- function(dataset, num_folds) {
  
  # Step 1: Data Preparation
  sub_df <- dataset %>%
    select(THCIC_ID, RACE, ZCTA_SVI, drive_time, SPEC_UNIT_1, PAT_AGE_ORDINAL, ETHNICITY) %>%
    mutate(THCIC_ID = as.factor(THCIC_ID), RACE = as.factor(RACE))
  
  # Step 2: Define the number of folds for cross-validation
  set.seed(123)  # For reproducibility
  cv_indices <- createFolds(sub_df$THCIC_ID, k = num_folds)
  
  # Create empty data frame to store evaluation metrics
  evaluation_metrics_df <- list(
    Fold = integer(),
    Train_Data = list(),
    Test_Data = list(),
    Multinom_Model = list(),
    Multinom_ConfusionMatrix = list(),
    RF_Model = list(),
    RF_ConfusionMatrix = list(),
    Variable_Importance = list(),
    stringsAsFactors = FALSE
  )
  
  # Loop through folds
  for (fold in seq_along(cv_indices)) {
    train_indices <- unlist(cv_indices[-fold])
    test_indices <- unlist(cv_indices[fold])
    
    train_data <- sub_df[train_indices, ]
    test_data <- sub_df[test_indices, ]
    
    cat(paste("Fold:", fold, ", Training Data Size:", nrow(train_data), ", Test Data Size:", nrow(test_data)), "\n")
    
    # Multinomial Logistic Regression
    multinom_model <- multinom(THCIC_ID ~ RACE + ZCTA_SVI + drive_time + SPEC_UNIT_1 + ETHNICITY + PAT_AGE_ORDINAL, data = train_data, maxit = 1000)
    
    # Predictions and evaluation
    predicted_data <- predict(multinom_model, newdata = test_data)
    cm_multinom <- confusionMatrix(table(test_data$THCIC_ID, predicted_data))
    
    # Build a Random Forest Model & Check the Accuracy 
    rf_model <- randomForest(THCIC_ID ~ RACE + ZCTA_SVI + drive_time + SPEC_UNIT_1 + ETHNICITY + PAT_AGE_ORDINAL, data = train_data)
    predicted_data_rf <- predict(rf_model, newdata = test_data)
    confusionMatrix(as.factor(predicted_data_rf), as.factor(test_data$THCIC_ID))
    
    # Calculate variable importance for Random Forest
    var_importance <- importance(rf_model)
    
    # Store metrics in the data frame
    evaluation_metrics_df <- bind_rows(
      evaluation_metrics_df,
      tibble(
        Fold = fold,
        Train_Data = list(train_data),
        Test_Data = list(test_data),
        Multinom_Model = list(multinom_model),
        Multinom_ConfusionMatrix = list(cm_multinom),
        RF_Model = list(rf_model),
        RF_ConfusionMatrix = list(confusionMatrix(as.factor(predicted_data_rf), as.factor(test_data$THCIC_ID))),
        Variable_Importance = list(var_importance), 
      )
    )
  }
  
  # Compare results across folds
  multinom_accuracies <- sapply(evaluation_metrics_df$Multinom_ConfusionMatrix, function(cm) cm$overall["Accuracy"])
  
  # Print or visualize the results
  cat("Multinomial Logistic Regression Accuracies:\n")
  print(multinom_accuracies)
  
  View(evaluation_metrics_df)
  return(evaluation_metrics_df)
  
}

# Function to print out the results from the metrics data frame 
print_fold_results <- function(result, fold_value) {
  cat("Fold value:", fold_value, "\n")
  
  # Display Train Data
  cat("\nTrain Data:\n")
  print(summary(result$Train_Data[[fold_value]]))
  
  # Display Test Data
  cat("\nTest Data:\n")
  print(summary(result$Test_Data[[fold_value]]))
  
  # Display Multinom Model
  cat("\nMultinom Model:\n")
  print(result$Multinom_Model[[fold_value]])
  
  # Display Multinom Confusion Matrix
  cat("\nMultinom Confusion Matrix:\n")
  print(result$Multinom_ConfusionMatrix[[fold_value]])
  
  # Display Random Forest Model
  cat("\nRandom Forest Model:\n")
  print(result$RF_Model[[fold_value]])
  
  # Display Random Forest Confusion Matrix
  cat("\nRandom Forest Confusion Matrix:\n")
  print(result$RF_ConfusionMatrix[[fold_value]])
  
  # Display Variable Importance
  cat("\nVariable Importance:\n")
  print(result$Variable_Importance[[fold_value]])
  
}

print_accuracies <- function(result, fold_value, matrix_type) {
  # Create a data frame to store the values
  accuracies <- data.frame(
    Fold = integer(),
    Accuracies = numeric()
  )
  
  # Store Overall values for the specified Confusion Matrix type
  overall_value <- result[[paste0(matrix_type)]][[5]]$overall[c(1, 3, 4)]
  accuracies <- rbind(accuracies, Fold = fold_value, data.frame(Accuracies = overall_value))

  # Return the data frame if needed
  return(accuracies)
}


# Set working directory
setwd("/Users/arushishaarma/Documents/GitHub/covid-19_research/")
getwd()

# Read the CSV file into a data frame
num_folds = 10
INPUT_DATA = read_csv("INPUT_DATA/austin_only_onehot_pudf_2018Q1.csv")
result <- perform_cross_validation(INPUT_DATA, num_folds)

fold_value = 8
print_fold_results(result, fold_value)

fold_value = 8
#matrix_type <- "RF_ConfusionMatrix"
matrix_type <- "Multinom_ConfusionMatrix"
accur <-print_accuracies(result, fold_value, matrix_type)


print(result$RF_ConfusionMatrix[[5]]$overall[3])

histogram(INPUT_DATA$LENGTH_OF_STAY) 
plot(INPUT_DATA$LENGTH_OF_STAY, INPUT_DATA$drive_time)








