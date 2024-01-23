########################################################################
# Code to run functions for comparing model performance
# Models tested are Random Forest and Multinomial Logistic Regression
# Currently run only for Austin, TX hospitals for ILI patients
# Code updated by Emily Javan - 2024-01-22 - ATX
########################################################################

#install.packages("ghibli") # only install once, these are just studio ghibli movie inspired color palletes

# source functions
source("CODE/0117_algorithm_hospfind.R") # this loads tidyverse etc.

# Check working directory
getwd()

# this is your default directory if you open the .Rproj file when you begin working
# you don't need to set it, if getwd returns something unexpected you probably opened the wrong proj
#setwd("/Users/arushishaarma/Documents/GitHub/covid-19_research/")

# Step 0: Read the CSV file into a data frame
# we'll double check how this data file was made from originals
INPUT_DATA = read_csv("INPUT_DATA/austin_only_onehot_pudf_2018Q1.csv")

# Step 1: Data Preparation
# Your function should take in the clean data to apply algorithm to any set of features
sub_df <- INPUT_DATA %>%
  select(THCIC_ID, RACE, ZCTA_SVI, drive_time, SPEC_UNIT_1, PAT_AGE_ORDINAL, ETHNICITY) %>%
  mutate(THCIC_ID = as.factor(THCIC_ID), RACE = as.factor(RACE))

# Step 2: Run models Random Forest and Multi-nomial regression for hospitals with at least 10% of data
# num_folds not define optimal number will be chosen for 80/20 split of data
# if seed not set then will be random
result <- perform_cross_validation(dataset=sub_df, seed=123) # num_folds=10

# Step 3: Plot results
# adding the location and labels for our null model performance
model_compare_acc = read_csv("OUTPUT_DATA/model_compare_accuries_2024-01-22.csv") %>%
  mutate(line_label = ifelse(fold==max(fold), paste0(model, " null model"), NA),
         fold_nudge = ifelse(fold==max(fold), (fold-0.2), NA),
         y_location = ifelse(fold==max(fold), AccuracyNull, NA) )

acc_comp_plot = ggplot(model_compare_acc, aes(x=fold, group=model, color=model))+ # notice how group/color are in the aes
  # plot accuracy per fold and error bars
  geom_point(aes(y=Accuracy))+
  geom_errorbar(aes(ymin=AccuracyLower,ymax=AccuracyUpper), width=0.2, alpha=0.6)+
  # plot dashed line of how well the null/random model would do
  geom_line(aes(y=AccuracyNull), linetype="dashed")+
  # rename the x-axis to be more informative
  labs(x="Cross Validation Folds")+
  # add labels to end of line for our null accuracy
  geom_label(aes(x=fold_nudge, y=y_location, label=line_label), show.legend=F)+
  # change color scheme, this onw is okay but the default is easier to see
  # comment this out or try some other scales
  ghibli::scale_colour_ghibli_d("MarnieMedium2", direction = -1)+
  # make background white
  theme_bw()
# save your plot with a high resolution (1200), but let computer decide on the size
ggsave("FIGURES/model_accuracy_compare.png", acc_comp_plot, dpi=1200)


# I'll leave it to you to make a plot for the classes, usually F1 score is helpful
# color scales will make more sense when you have more classes to visualize


##########################################################################################
# Arushi's code, didn't edit these functions
fold_value = 8
print_fold_results(result, fold_value)

fold_value = 8
#matrix_type <- "RF_ConfusionMatrix"
matrix_type <- "Multinom_ConfusionMatrix"
accur <-print_accuracies(result, fold_value, matrix_type)


print(result$RF_ConfusionMatrix[[5]]$overall[3])

histogram(INPUT_DATA$LENGTH_OF_STAY) 
plot(INPUT_DATA$LENGTH_OF_STAY, INPUT_DATA$drive_time)