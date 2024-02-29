#02/07/2024 
#boosted regression tree -> similar to a random forest as it iterates through it 
#take one hospital for a logistic regression (worst preformance one and the best preformance one)
#outcome: get a 1 if you went or 0 if you didn't 
#summary of the logisitic regression 

# a table for each hospital (name and id) for the size, total # of patients, ICU beds, train & test data, 
#percent of each age, percent of each race, mean and standard dev of the drive time 
#get the group by the hospital ID and then find the percent of 'X' value of each hospital and store

########################################################################
# Code to run functions for comparing model performance
# Models tested are Random Forest and Multinomial Logistic Regression
# Currently run only for Austin, TX hospitals for ILI patients
# Code updated by Emily Javan - 2024-01-22 - ATX
########################################################################

#install.packages("ghibli") # only install once, these are just studio ghibli movie inspired color palletes
library(beeswarm)
library(ggbeeswarm)

# Data generation
set.seed(1995)
x <- rnorm(300)

# Bee swarm plot
#beeswarm(x) 

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
# Function to plot F1 scores for each class
model_class_compare = read_csv("OUTPUT_DATA/model_class_compare_2024-01-22.csv")  %>% 
  mutate(THCIC_ID = as.factor(THCIC_ID))

#Get the mean of the train and test data for the class size 
class_size <- data.frame()
for(i in 1:5){
  temp_train <- result[[2]][[i]]
  temp_test <- result[[3]][[i]]
  df <- data.frame(fold = i, table(temp_train$THCIC_ID), table(temp_test$THCIC_ID)) %>% 
    select(-Var1.1)
  names(df) <- c("fold", "class", "train", "test") 
  class_size <- rbind(class_size, df)
}

class_size_mean <- class_size %>% 
  group_by(class) %>%
  summarize(train_size_mean = mean(train), test_size_mean = mean(test)) %>% 
  ungroup() %>%
  rename(THCIC_ID = class)

full_df = model_class_compare %>% 
  left_join(class_size_mean, by="THCIC_ID")

#Create the boxplot for the classes based on F1 score 
# model_f1_compare <- ggplot(full_df, 
#                            aes(x=THCIC_ID, y=F1, group=interaction(model, THCIC_ID), shape=model,
#                                color=as.factor(fold) )) + 
#   #geom_boxplot(alpha=0.2) + 
#   #geom_point(position = position_dodge(width = 0.75)) + 
#   geom_beeswarm(dodge.width = 0.75, alpha=0.7) + 
#   #geom_jitter(position = position_dodge(width = 0.75)) + #width = 0.1, 
#   #geom_label(aes(label = F1), position = position_dodge(width = 1.0), vjust = -0.5, size=2) +
#   #ggrepel::geom_label_repel()
#   theme_bw(base_size=20) 

model_f1_compare <- ggplot(full_df, 
                                  aes(x=F1, y=fold, group=model, color=model)) + 
  #geom_boxplot() + 
  #geom_beeswarm(dodge.width = 0.75, alpha=0.3) + 
  geom_line(alpha=0.5)+
  geom_point()+
  #geom_label(aes(label = F1), position = position_dodge(width = 1.0), vjust = -0.5, size = 2) +
  facet_wrap(~THCIC_ID, ncol = 1)+
  theme_bw() 


ggsave("FIGURES/model_f1_compare_facetwrap.png",model_f1_compare, dpi =1200)



#Create the boxplot for the classes based on Precision
model_percision_compare <- ggplot(full_df, 
                           aes(x=THCIC_ID, y=Precision, group=interaction(model, THCIC_ID), color=model)) + 
  geom_boxplot() + 
  theme_bw() 

# Create a summary dataset with mean precision for each model and class
summary_data <- full_df %>%
  group_by(model, THCIC_ID) %>%
  summarise(mean_precision = mean(Precision), .groups = 'drop')

# Create the detailed ggplot
model_precision_compare <- ggplot(full_df, 
                                  aes(x = factor(THCIC_ID), y = Precision, fill = model)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_point(data = summary_data, aes(x = factor(THCIC_ID), y = mean_precision), 
             color = "black", size = 3, shape = 17) +
  stat_summary(data = summary_data, fun = mean, geom = "point", 
               aes(x = factor(THCIC_ID), y = mean_precision, group = model), 
               size = 3, color = "black", shape = 18) +
  theme_minimal() +
  labs(title = "Comparison of Precision by Class and Model",
       x = "THCIC_ID",
       y = "Precision",
       fill = "Model") +
  theme(legend.position = "bottom")

#facet wrapped percision boxplot 
model_percision_compare <- ggplot(full_df, 
        aes(x=THCIC_ID, y=Precision, group=interaction(model, THCIC_ID), color=model)) + 
        geom_line(alpha=0.5)+
        geom_point()+
        #geom_label(aes(label = F1), position = position_dodge(width = 1.0), vjust = -0.5, size = 2) +
        facet_wrap(~THCIC_ID, ncol = 1)+
        theme_bw() 


ggsave("FIGURES/model_percision_compare.png",model_percision_compare, dpi =1200)

# Create a summary dataset with mean balanced accuracy for each model and class
summary_data_accuracy <- full_df %>%
  group_by(model, THCIC_ID) %>%
  summarise(mean_accuracy = mean(Balanced.Accuracy), .groups = 'drop')

# Create the detailed ggplot for accuracy comparison
model_accuracy_compare <- ggplot(full_df, 
                                 aes(x = factor(THCIC_ID), y = Balanced.Accuracy, fill = model)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_point(data = summary_data_accuracy, aes(x = factor(THCIC_ID), y = mean_accuracy), 
             color = "black", size = 3, shape = 17) +
  annotate("text", x = '829900' , y = 0.7, color = "black", label = "triangle = mean accuracy" ) + 
  stat_summary(data = summary_data_accuracy, fun = mean, geom = "point", 
               aes(x = factor(THCIC_ID), y = mean_accuracy, group = model), 
               size = 3, color = "black", shape = 18) +
  theme_minimal() +
  labs(title = "Comparison of Balanced Accuracy by Class and Model",
       x = "THCIC_ID",
       y = "Balanced Accuracy",
       fill = "Model") +
  theme(legend.position = "bottom")

#Create the boxplot for the classes based on Accuracy
model_accuracy_compare <- ggplot(full_df, 
                                  aes(x=THCIC_ID, y=Balanced.Accuracy, group=interaction(model, THCIC_ID), color=model)) + 
  geom_boxplot() + 
  geom_label(aes(label = F1), position = position_dodge(width = 1.0), vjust = -0.5, size = 2) + 
  theme_bw() 

#facet wrapped percision boxplot 
model_percision_compare <- ggplot(full_df, 
                                  aes(x=THCIC_ID, y=Precision, group=interaction(model, THCIC_ID), color=model)) + 
  geom_line(alpha=0.5)+
  geom_point()+
  #geom_label(aes(label = F1), position = position_dodge(width = 1.0), vjust = -0.5, size = 2) +
  facet_wrap(~THCIC_ID, ncol = 1)+
  theme_bw() 
ggsave("FIGURES/model_accuracy_compare.png",model_percision_compare, dpi =1200)

#HOSPITAL #1 TESTED: 497000
#low SVI = better at prediciting the index (less vulunrable = more healthy white and educated)

#HOSPITAL #2 TESTED: 797600
#low SVI = better at prediciting the index (less vulunrable = more healthy white and educated)

#HOSPITAL #3 TESTED: 829900
#high SVI = better at prediciting the index (less vulunrable = more healthy white and educated)


#look at all of the SVIs 
ggplot(sub_df, aes(x = ZCTA_SVI, group=THCIC_ID, fill= THCIC_ID)) +
  geom_boxplot(width = 0.2) +
  labs(x = "ZCTA_SVI") +
  theme_minimal()


X = '829900'

#compare each hospital to each other 
hospital_data_compare <- sub_df %>%
  mutate(is_hospital_x = ifelse(THCIC_ID == X, 1, 0),
         is_hospital_x  = as.factor(is_hospital_x))
table(hospital_data_compare$is_hospital_x)

ggplot(hospital_data_compare, aes(x = ZCTA_SVI, fill = is_hospital_x)) +
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") +
  labs(x = "ZCTA_SVI", y = "Frequency") +
  theme_minimal()

# Box plot
ggplot(hospital_data_compare, aes(x = is_hospital_x, y = ZCTA_SVI, fill =is_hospital_x)) +
  geom_boxplot() +
  labs(x = "THCIC_ID", y = "ZCTA_SVI") +
  theme_minimal()

# Violin plot
ggplot(hospital_data_compare, aes(x = is_hospital_x, y = ZCTA_SVI, fill = is_hospital_x)) +
  geom_violin() +
  labs(x = "THCIC_ID", y = "ZCTA_SVI") +
  theme_minimal()

# Density plot
ggplot(hospital_data_compare, aes(x = ZCTA_SVI, fill = is_hospital_x)) +
  geom_density(alpha = 0.5) +
  labs(x = "ZCTA_SVI", y = "Density") +
  theme_minimal()

# Scatter plot with jitter
ggplot(hospital_data_compare, aes(x = is_hospital_x, y = ZCTA_SVI, color = is_hospital_x)) +
  geom_jitter(width = 0.2) +
  labs(x = "THCIC_ID", y = "ZCTA_SVI") +
  theme_minimal()

logistic_model <- glm(is_hospital_x ~ ZCTA_SVI,
                      data = hospital_data_compare, 
                      family = binomial)
summary(logistic_model)

table(sub_df$THCIC_ID)

#The coefficient estimate for `ZCTA_SVI` is -0.6981 (estimated) with a standard error of 0.4554. 
#This means that for every one-unit increase in `ZCTA_SVI`, the log odds (check the difference between log odds and odds) of being in hospital A compared to other Austin area hospitals decrease by approximately 0.6981. 
#Patients who attended hospital A lived in ZIP codes with approximately 1.6526 units lower social vulnerability index (SVI) than those who attended other Austin area hospitals.
# The significance of the coefficient is determined by the z-value, which is -3.242, and the associated p-value, which is 0.00119. 
#This indicates that the effect of `ZCTA_SVI` on the likelihood of being in hospital A versus other hospitals is statistically significant.
# In summary, according to this logistic regression model, higher social vulnerability index (SVI) is associated with a decreased likelihood of being in hospital A compared to other Austin area hospitals.

#p value is area under the curve 


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