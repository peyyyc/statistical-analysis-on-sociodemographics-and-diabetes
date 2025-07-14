#Clear environment
rm(list = ls())

#Install packages
#install.packages(c("tidyverse", "ggplot2", "dplyr", patchwork")
library(patchwork) #combining plots into one visualiztion chart
library(tidyverse) #data manipulation and visualization
library(ggplot2) #visualization
library(dplyr) #data wrangling

#Load and inspect data
data <- read.csv("final_dataset(in).csv", stringsAsFactors = FALSE)
sum(is.na(data)) #269938
colnames(data)
colSums(is.na(data))/nrow(data)
glimpse(data)
#household_fs_cat, SNAP_current, poverty_index, age, hispanic_origin, non_hispanic_origin

#Selecting initial predictors and response
selected_data <- data[, c("diabetes_diag", "household_fs_cat", "SNAP_current", "poverty_index", "age", "hispanic_origin", "non_hispanic_origin")]

#Exploratory Analysis
#install.packages("naniar") #visualizing missing data points
library(naniar)
library(viridis)
missing_summary <- data %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))
missing_var_plot <- 10 #selecting top 10 out of 39 variables with missing values 
if(nrow(missing_summary) > 0) {
  print(paste("Plotting missing percentage for top", min(missing_var_plot, nrow(missing_summary)), "variables"))
  missing_plot <- missing_summary %>%
    slice_head(n = missing_var_plot) %>%
    ggplot(aes(x = reorder(variable, pct_miss), y = pct_miss)) +
    geom_col(fill = "orange") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(title = paste("Top", min(missing_var_plot, nrow(missing_summary)), "Variables by Percent Missing"),
         x = "Variable",
         y = "Missing") +
    theme_minimal()
  print(missing_plot)
} #none of the selected predictors are in the top 10 missing values table

#assessing variables individually
dm_diag_plot <- ggplot(selected_data, aes(x = factor(diabetes_diag))) +
  geom_bar(aes(y = ..prop.., group = 1), fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y = ..prop..), stat = "count", vjust = -0.5) +
  labs(title = "Distribution of Diabetes Status",
       x = "Diabetes Status (1-Yes, 2- No, 3,7,9- other)",
       y = "Percentage") +
  theme_minimal()
household_fs_plot <- ggplot(selected_data, aes(x = factor(household_fs_cat))) + 
  geom_bar(fill = "green") + 
  labs(title = "Distribution of Household Food Security", 
       x = "Food Security Category Code", y="Count") + 
  theme_minimal()
SNAP_plot <- ggplot(selected_data, aes(x = factor(SNAP_current))) + 
  geom_bar(fill = "orange") + 
  labs(title = "Distribution of SNAP Participation", 
       x = "SNAP Status Code", 
       y="Count") + 
  theme_minimal()
hisp_plot <- ggplot(selected_data, aes(x = factor(hispanic_origin))) + 
  geom_bar(fill = "purple") + 
  labs(title = "Distribution of Hispanic Origin", 
       x = "Hispanic Origin", 
       y="Count") + 
  theme_minimal()
non_hisp_plot <- ggplot(selected_data, aes(x = factor(non_hispanic_origin))) + 
  geom_bar(fill = "pink") + 
  labs(title = "Distribution of Non-Hispanic Origin", 
       x = "Non-Hispanic Origin", 
       y="Count") + 
  theme_minimal()
combined_plot_1 <- (dm_diag_plot) / (household_fs_plot + SNAP_plot) / (hisp_plot + non_hisp_plot)
print(combined_plot_1)

age_plot <- ggplot(selected_data, aes(x = age)) + 
  geom_histogram(bins=30, fill="lightblue") + 
  labs(title="Age Distribution",
       x = "Age",
       y = "Count") + 
  theme_minimal()
pov_index_plot <- ggplot(selected_data, aes(x = poverty_index)) + 
  geom_histogram(bins=30, fill="orchid") + 
  labs(title="Poverty Income Ratio Distribution",
       x = "Poverty Index",
       y = "Count") + 
  theme_minimal()
print(age_plot + pov_index_plot)

#Creating binary variable for diabetes_diag with diabetes_binary column
selected_data <- selected_data %>%
  mutate(
    diabetes_binary = case_when(
      diabetes_diag == 1 ~ 1,
      diabetes_diag == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
str(selected_data$diabetes_diag)

db_house <- ggplot(selected_data, aes(x = household_fs_cat, fill = factor(diabetes_binary))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion",
       x = "Household Food Security Category") #majority of counts fall in category 0 (No diabetes)
db_SNAP <- ggplot(selected_data, aes(x = SNAP_current, fill = factor(diabetes_binary))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion",
       x = "SNAP Participation (1- yes, 2- No, 7,9- other)") #majority of counts fall in category 0 (No diabetes)
db_pov_index <- ggplot(selected_data, aes(x = factor(diabetes_binary), y = poverty_index, fill = factor(diabetes_binary))) +
  geom_boxplot() +
  scale_fill_viridis(guide = "none", labels = c("No diabetes", "Diabetes")) +
  labs(y = "Poverty Index",
       x = "Diabetes Diagnosis",
       title = "Poverty Index by Diabetes Diagnosis") #index range and median roughly the same among all categories
db_hisp <- ggplot(selected_data, aes(x = hispanic_origin, fill = factor(diabetes_binary))) +
  geom_bar(position = "fill") +
  labs(y = "Diabetes Diagnosis",
       x = "Hispanic Origin",
       fill = "Diabetes Diagnosis",
       title = "Diabetes Diagnosis by Hispanic Origin") #majority of counts fall in category 0 (No diabetes) 
db_nonhisp <- ggplot(selected_data, aes(x = non_hispanic_origin, fill = factor(diabetes_binary))) +
  geom_bar(position = "fill") +
  labs(y = "Diabetes Diagnosis",
       x = "Non-Hispanic Origin",
       fill = "Diabetes Diagnosis",
       title = "Diabetes Diagnosis by Non-Hispanic Origin") #majority of counts fall in category 0 (No diabetes)
db_age <- ggplot(selected_data, aes(x =  age)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ factor(diabetes_binary), ncol = 1)
  labs (y = "Age",
        x = "Diabetes Diagnosis",
        fill = "Diabetes Diagnosis",
        title = "Diabetes Diagnosis by Age") #wide age distribution for no diabetes with most data points in the 0-25 age range, most data points are located in the older age group for those with diabetes
combined_plot_2 <- (db_house + db_SNAP) / (db_pov_index) / (db_hisp + db_nonhisp)
print(combined_plot_2)
print(db_age)

#Data cleaning
selected_data$household_fs_cat <- as.factor(selected_data$household_fs_cat) #Handling missing values
selected_data$SNAP_current <- as.factor(selected_data$SNAP_current)
selected_data$hispanic_origin <- as.factor(selected_data$hispanic_origin)
selected_data$non_hispanic_origin <- as.factor(selected_data$non_hispanic_origin)
sum(is.na(selected_data))

data_clean_1 <- selected_data %>%
  mutate(
    #categorical variables, using mode
    household_fs_cat = ifelse(is.na(household_fs_cat),
                              names(sort(table(household_fs_cat), decreasing = TRUE)) [1], #sorting to give the more frequent values
                              household_fs_cat),
    SNAP_current = ifelse(is.na(SNAP_current),
                          names(sort(table(SNAP_current), decreasing = TRUE)) [1],
                          SNAP_current),
    hispanic_origin = ifelse(is.na(hispanic_origin),
                             names(sort(table(hispanic_origin), decreasing = TRUE)) [1],
                             hispanic_origin),
    non_hispanic_origin = ifelse(is.na(non_hispanic_origin),
                                 names(sort(table(non_hispanic_origin), decreasing = TRUE)) [1],
                                 non_hispanic_origin),
    #numerical variables, using median
    poverty_index = ifelse(is.na(poverty_index), median(poverty_index, na.rm = TRUE), poverty_index),
    age = ifelse(is.na(age), median(age, na.rm = TRUE), age)
  )
sum(is.na(data_clean_1))

#Removing NA values in the diabetes_binary column
data_clean_2 <- data_clean_1 %>%
filter(!is.na(diabetes_binary))
print(names(data_clean_2))
sum(is.na(data_clean_2))

#Combining race columns
analysis_data <- data_clean_2 %>%
  mutate(
    #race_ethnicity
    race_ethnicity = dplyr::case_when(
      hispanic_origin == 1 ~ "Hispanic_MexicanAmerican",
      hispanic_origin == 2 ~ "Hispanic_Other",
      hispanic_origin == 3 ~ "NonHispanic_White",
      hispanic_origin == 4 ~ "NonHispanic_Black",
      hispanic_origin == 5 ~ "NonHispanic_Other",
      non_hispanic_origin == 6 ~ "Hispanic_Asian",
      non_hispanic_origin == 7 ~ "NonHispanic_Other",
      TRUE ~ NA_character_ #in case other NA prevalent in cleaned dataset
    )
  )

#Categorizing categorical variables
final_analysis_data <- analysis_data %>%
  mutate(
    #Household_fs_cat
    household_fs_cat = factor(as.character(household_fs_cat),
                              levels = c("1", "2", "3", "4"),
                              labels = c("FullSecurity", "MarginalSecurity", "LowSecurity", "VeryLowSecurity")),
    
    household_fs_cat = relevel(household_fs_cat, ref = "FullSecurity"),
    #SNAP_current
    SNAP_current = factor(as.character(SNAP_current),
                          levels = c("1", "2"),
                          labels = c("Yes_SNAP", "No_SNAP")),
    SNAP_current = ifelse(is.na(SNAP_current),
                          names(sort(table(SNAP_current), decreasing = TRUE)) [1],
                          SNAP_current),
    #race_ethnicity
    race_ethnicity = factor(race_ethnicity), #changing new column as factor
    # Set NonHispanic_White as reference
    race_ethnicity = relevel(race_ethnicity, ref = "NonHispanic_White"),
    #poverty_index
    poverty_index = as.numeric(poverty_index),
    #age
    age = as.numeric(age)
  
  ) %>%
  
  # Selecting only relevant columns and excluding original diabetes_diag, hispanic_origin, non_hispanic_origin
  select(
    diabetes_binary,
    household_fs_cat,
    SNAP_current,
    poverty_index,
    age,
    race_ethnicity
  )


#Assessing distribution of response variable
table(final_analysis_data$diabetes_binary)
prop.table(table(final_analysis_data$diabetes_binary))

#Logistic regression - outcome variable (diabetes_diag, is binary; this method is useful for predicting binary outcomes that are easier to interpret while using multiple predictors)
logistic_model <- glm(diabetes_binary ~ household_fs_cat + SNAP_current + poverty_index + age + race_ethnicity,
             data = final_analysis_data,
             family = binomial(link = "logit"))
print(summary(logistic_model))

#Obtaining odds ratio
odds_ratio <- exp(coef(logistic_model))
print(odds_ratio)

#Calculating confidence interval
ci <- exp(confint(logistic_model))
print(ci)

#Visualizing
#install.packages("ggeffects")
library(ggeffects)
pred_prob_age <- ggpredict(logistic_model, terms = "age")
plot(pred_prob_age) +
  labs(
    title = "Estimated Prevelance of Diabetes with Age",
    y = "Estimated Probability of Having Diabetes",
    x = "Age"
  ) +
  theme_minimal()

pred_prob_fs <- ggpredict(logistic_model, terms = "household_fs_cat")
plot(pred_prob_fs) +
  labs(
    title = "Estimated Prevelance of Diabetes with Food Security Level",
    y = "Estimated Probability of Having Diabetes",
    x = "Household Food Security Category"
  ) +
  ylim(0, NA) + #y-axis starts at 0 (probability)
  theme_minimal()

pred_prob_race <- ggpredict(logistic_model, terms = "race_ethnicity")
plot(pred_prob_race) +
  labs(
    title = "Estimated Prevelance of Diabetes with by Race/Ethnicity",
    y = "Estimated Probability of Having Diabetes",
    x = "Race / Ethnicity"
  ) +
  ylim(0, NA) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))