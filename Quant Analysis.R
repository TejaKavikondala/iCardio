#Descriptive Statistics for Pre Survey Questionnaires 
#Load packages
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(psych)
library(corrplot)
library(nortest)
library(purrr)
library(reshape2)
#Presurvey data 
file_path <- "C:/Users/bhavy/Desktop/ProjectData/PreSurveyResponses.csv"

# Read the dataset
pre_data <- read.csv(file_path)

#Checking structure 
str(pre_data)

# Check for missing values
sum(is.na(pre_data))

#new column name
new_column_names <- paste("Q", 1:36, sep="")

names(pre_data) <- new_column_names

dataframe1P <- pre_data[, 1:10]
dataframe2P <- pre_data[, 11:24]
dataframe3P <- pre_data[, 25:36]

head(dataframe1P)
head(dataframe2P)
head(dataframe3P)


#DataFrame 1 - Perception of Effectiveness
# Calculate descriptive statistics for dataframe1
desc_stats <- dataframe1P %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))

print(desc_stats)

# Calculate composite score (mean across all questions for each respondent)
dataframe1P$composite_score <- rowMeans(dataframe1P, na.rm = TRUE)

# Calculate overall descriptive statistics for composite score
composite_stats <- dataframe1P %>%
  summarise(
    mean_composite = mean(composite_score, na.rm = TRUE),
    median_composite = median(composite_score, na.rm = TRUE),
    sd_composite = sd(composite_score, na.rm = TRUE),
    var_composite = var(composite_score, na.rm = TRUE)
  )

# Print the composite statistics
print(composite_stats)

# Calculate reliability (Cronbach's alpha) for the scale
alpha(dataframe1P)

# Correlation matrix to see the relationships between items
correlation_matrix <- cor(dataframe1P, use = "complete.obs")

# Boxplots for each question in dataframe1P
ggplot(dataframe1P, aes(y = composite_score)) + 
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Composite Scores for Perception of Effectiveness",
       y = "Composite Score") +
  theme(axis.text.x = element_blank())


# Visualizing the distribution of composite scores
ggplot(dataframe1P, aes(x = composite_score)) +
  geom_histogram(binwidth = 0.5, fill = 'skyblue', color = 'black') +
  labs(title = "Distribution of Composite Perception of Effectiveness Scores") +
  theme_minimal()


#DataFrame 2 - Trust
# Calculate descriptive statistics for dataframe1

desc_stats <- dataframe1P %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))

# Calculate composite score (mean across all questions for each respondent)
dataframe2P$composite_score <- rowMeans(dataframe2P, na.rm = TRUE)

# Calculate overall descriptive statistics for composite score
composite_stats <- dataframe2P %>%
  summarise(
    mean_composite = mean(composite_score, na.rm = TRUE),
    median_composite = median(composite_score, na.rm = TRUE),
    sd_composite = sd(composite_score, na.rm = TRUE),
    var_composite = var(composite_score, na.rm = TRUE)
  )

# Print the composite statistics
print(composite_stats)

# Calculate reliability (Cronbach's alpha) for the scale
alpha(dataframe2P)

# Correlation matrix to see the relationships between items
correlation_matrix <- cor(dataframe2P, use = "complete.obs")

# Boxplots for each question in dataframe2P
ggplot(dataframe2P, aes(y = composite_score)) + 
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Composite Scores for Trust",
       y = "Composite Score") +
  theme(axis.text.x = element_blank())


# Visualizing the distribution of composite scores
ggplot(dataframe2P, aes(x = composite_score)) +
  geom_histogram(binwidth = 0.5, fill = 'skyblue', color = 'black') +
  labs(title = "Distribution of Composite Trust Scores") +
  theme_minimal()



#DataFrame 3 - Changes in Health Behaviors and Decisions
# Calculate descriptive statistics for dataframe1

desc_stats <- dataframe3P %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))

print(desc_stats)

# Calculate composite score (mean across all questions for each respondent)
dataframe3P$composite_score <- rowMeans(dataframe3P, na.rm = TRUE)

# Calculate overall descriptive statistics for composite score
composite_stats <- dataframe3P %>%
  summarise(
    mean_composite = mean(composite_score, na.rm = TRUE),
    median_composite = median(composite_score, na.rm = TRUE),
    sd_composite = sd(composite_score, na.rm = TRUE),
    var_composite = var(composite_score, na.rm = TRUE)
  )

# Print the composite statistics
print(composite_stats)

# Calculate reliability (Cronbach's alpha) for the scale
alpha(dataframe3P)

# Correlation matrix to see the relationships between items
correlation_matrix <- cor(dataframe3P, use = "complete.obs")

# Boxplots for each question in dataframe2P
ggplot(dataframe3P, aes(y = composite_score)) + 
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Composite Scores for Changes in Health Behaviors and Decisions",
       y = "Composite Score") +
  theme(axis.text.x = element_blank())


# Visualizing the distribution of composite scores
ggplot(dataframe3P, aes(x = composite_score)) +
  geom_histogram(binwidth = 0.5, fill = 'skyblue', color = 'black') +
  labs(title = "Distribution of Composite Changes in Health Behaviors and Decisions Scores") +
  theme_minimal()




#Postsurvey Data

file_path <- "C:/Users/bhavy/Desktop/ProjectData/PostSurveyResponses.csv"

# Read the dataset
post_data <- read.csv(file_path)

#Checking structure 
str(post_data)

# Check for missing values
sum(is.na(post_data))

#new column name
new_column_names <- paste("Q", 1:35, sep="")

names(post_data) <- new_column_names

dataframe1Po <- post_data[, 1:10]
dataframe2Po <- post_data[, 11:23]
dataframe3Po <- post_data[, 24:35]

head(dataframe1Po)
head(dataframe2Po)
head(dataframe3Po)


#DataFrame 1 - Perception of Effectiveness
# Calculate descriptive statistics for dataframe1
desc_stats <- dataframe1Po %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))

print(desc_stats)

# Calculate composite score (mean across all questions for each respondent)
dataframe1Po$composite_score <- rowMeans(dataframe1Po, na.rm = TRUE)

# Calculate overall descriptive statistics for composite score
composite_stats <- dataframe1Po %>%
  summarise(
    mean_composite = mean(composite_score, na.rm = TRUE),
    median_composite = median(composite_score, na.rm = TRUE),
    sd_composite = sd(composite_score, na.rm = TRUE),
    var_composite = var(composite_score, na.rm = TRUE)
  )

# Print the composite statistics
print(composite_stats)

# Calculate reliability (Cronbach's alpha) for the scale
alpha(dataframe1Po)

# Correlation matrix to see the relationships between items
correlation_matrix <- cor(dataframe1Po, use = "complete.obs")

# Boxplots for each question in dataframe1P
ggplot(dataframe1Po, aes(y = composite_score)) + 
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Composite Scores for Perception of Effectiveness",
       y = "Composite Score") +
  theme(axis.text.x = element_blank())

# Visualizing the distribution of composite scores
ggplot(dataframe1Po, aes(x = composite_score)) +
  geom_histogram(binwidth = 0.5, fill = 'green', color = 'black') +
  labs(title = "Distribution of Composite Perception of Effectiveness Scores") +
  theme_minimal()


#DataFrame 2 - Trust
# Calculate descriptive statistics for dataframe1

desc_stats <- dataframe2Po %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))

# Calculate composite score (mean across all questions for each respondent)
dataframe2Po$composite_score <- rowMeans(dataframe2Po, na.rm = TRUE)

# Calculate overall descriptive statistics for composite score
composite_stats <- dataframe2Po %>%
  summarise(
    mean_composite = mean(composite_score, na.rm = TRUE),
    median_composite = median(composite_score, na.rm = TRUE),
    sd_composite = sd(composite_score, na.rm = TRUE),
    var_composite = var(composite_score, na.rm = TRUE)
  )

# Print the composite statistics
print(composite_stats)

# Calculate reliability (Cronbach's alpha) for the scale
alpha(dataframe2Po)

# Correlation matrix to see the relationships between items
correlation_matrix <- cor(dataframe2Po, use = "complete.obs")

# Boxplots for each question in dataframe2P
ggplot(dataframe2Po, aes(y = composite_score)) + 
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Composite Scores for Trust",
       y = "Composite Score") +
  theme(axis.text.x = element_blank())


# Visualizing the distribution of composite scores
ggplot(dataframe2Po, aes(x = composite_score)) +
  geom_histogram(binwidth = 0.5, fill = 'green', color = 'black') +
  labs(title = "Distribution of Composite Trust Scores") +
  theme_minimal()

#DataFrame 3 - Changes in Health Behaviors and Decisions
# Calculate descriptive statistics for dataframe1

desc_stats <- dataframe3Po %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  )))

print(desc_stats)

# Calculate composite score (mean across all questions for each respondent)
dataframe3Po$composite_score <- rowMeans(dataframe3Po, na.rm = TRUE)

# Calculate overall descriptive statistics for composite score
composite_stats <- dataframe3Po %>%
  summarise(
    mean_composite = mean(composite_score, na.rm = TRUE),
    median_composite = median(composite_score, na.rm = TRUE),
    sd_composite = sd(composite_score, na.rm = TRUE),
    var_composite = var(composite_score, na.rm = TRUE)
  )

# Print the composite statistics
print(composite_stats)

# Calculate reliability (Cronbach's alpha) for the scale
alpha(dataframe3P)

# Correlation matrix to see the relationships between items
correlation_matrix <- cor(dataframe3Po, use = "complete.obs")

# Boxplots for each question in dataframe2P
ggplot(dataframe3Po, aes(y = composite_score)) + 
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Composite Scores for Changes in Health Behaviors and Decisions",
       y = "Composite Score") +
  theme(axis.text.x = element_blank())

# Visualizing the distribution of composite scores
ggplot(dataframe3Po, aes(x = composite_score)) +
  geom_histogram(binwidth = 0.5, fill = 'skyblue', color = 'black') +
  labs(title = "Distribution of Composite Changes in Health Behaviors and Decisions Scores") +
  theme_minimal()



#Pre Survey

# Create a new dataframe combining composite scores from the three dataframes
composite_scores_dfP <- data.frame(
  Perception_of_Effect = dataframe1P$composite_score,
  Trust = dataframe2P$composite_score,
  HBehaviorChanges = dataframe3P$composite_score
)

print(composite_scores_dfP)


# Check normality for questions Q1 to Q10
Prenormality_results <- composite_scores_dfP %>%
  map(~ if(is.numeric(.x)) shapiro.test(.x) else NULL)

print(Prenormality_results)

data_long <- melt(composite_scores_dfP)

# Histogram with overlaid density curve
ggplot(data_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 5, fill = 'skyblue', alpha = 0.5) +  # Histogram normalized to density
  geom_density(alpha = .2, fill = "#FF6666") +  # Density plot overlay
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(x = "Likert Scale Response", y = "Density")

# Optionally, visualize the distributions for each question
#data_long <- reshape2::melt(composite_scores_dfP)
#ggplot(data_long, aes(x = value)) +
#  geom_histogram(bins = 5, fill = 'skyblue') +  # Adjust the number of bins for your Likert scale
#  facet_wrap(~variable, scales = "free") +
#  theme_minimal() +
#  labs(x = "Likert Scale Response", y = "Count")

# Q-Q plot for 'composite_score1'
qqnorm(dataframe1P$composite_score, main = "Q-Q Plot for Perception of Effectiveness")
qqline(dataframe1P$composite_score, col = "red")

# Q-Q plot for 'composite_score2'
qqnorm(dataframe2P$composite_score, main = "Q-Q Plot for Trust")
qqline(dataframe2P$composite_score, col = "red")

# Q-Q plot for 'composite_score3'
qqnorm(dataframe3P$composite_score, main = "Q-Q Plot for Changes in Health behaviors")
qqline(dataframe3P$composite_score, col = "red")




#Post Survey
# Create a new dataframe combining composite scores from the three dataframes
composite_scores_dfPo <- data.frame(
  Perception_of_Effect = dataframe1Po$composite_score,
  Trust = dataframe2Po$composite_score,
  HBehaviorChanges = dataframe3Po$composite_score
)

print(composite_scores_dfPo)


# Check normality for questions Q1 to Q10
Post_normality_results <- composite_scores_dfPo %>%
  map(~ if(is.numeric(.x)) shapiro.test(.x) else NULL)

print(Post_normality_results)

# Optionally, visualize the distributions for each question
data_long <- reshape2::melt(composite_scores_dfPo)
ggplot(data_long, aes(x = value)) +
  geom_histogram(bins = 5, fill = 'skyblue') +  # Adjust the number of bins for your Likert scale
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(x = "Likert Scale Response", y = "Count")

# Q-Q plot for Perception of Effectiveness
qqnorm(dataframe1Po$composite_score, main = "Q-Q Plot for Perception of Effectiveness")
qqline(dataframe1Po$composite_score, col = "red")

# Q-Q plot for Trust
qqnorm(dataframe2Po$composite_score, main = "Q-Q Plot for Trust")
qqline(dataframe2Po$composite_score, col = "red")

# Q-Q plot for Changes in Health behaviors
qqnorm(dataframe3Po$composite_score, main = "Q-Q Plot for Changes in Health behaviors")
qqline(dataframe3Po$composite_score, col = "red")




#Hypothesis- 1- Two way Repeated Measures ANOVA and Pearson rank Correlation Analysis


composite_scores_dfP$ParticipantID <- factor(row.names(composite_scores_dfP))
composite_scores_dfP$Trust <- as.numeric(as.character(composite_scores_dfP$Trust))
composite_scores_dfP$HBehaviorChanges <- as.numeric(as.character(composite_scores_dfP$HBehaviorChanges))

print(composite_scores_dfP)

# Run the Two-Way Repeated Measures ANOVA on presurvey
fit <- aov(HBehaviorChanges ~ Trust + Error(ParticipantID), data = composite_scores_dfP)

# Get a summary of the ANOVA
summary(fit)

# Pearson Correlation Analysis on pre data
pre_correlation_test <- cor.test(composite_scores_dfP$Trust, composite_scores_dfP$HBehaviorChanges, method = "pearson")
print(pre_correlation_test)

# Interpret the Pearson correlation coefficient value
rho <- pre_correlation_test$estimate
cat("Pearson's r:", rho, "\n")

# Interpret the p-value
p_value <- pre_correlation_test$p.value
cat("p-value:", p_value, "\n")


composite_scores_dfPo$ParticipantID <- factor(row.names(composite_scores_dfPo))
composite_scores_dfPo$Trust <- as.numeric(as.character(composite_scores_dfPo$Trust))
composite_scores_dfPo$HBehaviorChanges <- as.numeric(as.character(composite_scores_dfPo$HBehaviorChanges))

print(composite_scores_dfPo)

# Run the Two-Way Repeated Measures ANOVA on presurvey
fit <- aov(HBehaviorChanges ~ Trust + Error(ParticipantID), data = composite_scores_dfPo)

# Get a summary of the ANOVA
summary(fit)

# Pearson Correlation Analysis on post data
post_correlation_test <- cor.test(composite_scores_dfPo$Trust, composite_scores_dfPo$HBehaviorChanges, method = "pearson")
print(post_correlation_test)

# Interpret the Pearson correlation coefficient value
rho <- post_correlation_test$estimate
cat("Pearson's r:", rho, "\n")

# Interpret the p-value
p_value <- post_correlation_test$p.value
cat("p-value:", p_value, "\n")


combined_data <- rbind(
  transform(composite_scores_dfP, Time = 'Pre'),
  transform(composite_scores_dfPo, Time = 'Post')
)

print(combined_data)

combined_data$ParticipantID <- factor(row.names(combined_data))
combined_data$Trust <- as.factor(combined_data$Trust)
combined_data$Time <- as.factor(combined_data$Time)
combined_data$Trust <- as.numeric(as.character(combined_data$Trust))
combined_data$HBehaviorChanges <- as.numeric(as.character(combined_data$HBehaviorChanges))

print(combined_data$Trust)

# Run the Two-Way Repeated Measures ANOVA on combined data 
fit <- aov(HBehaviorChanges ~ Trust * Time + Error(ParticipantID), data = combined_data)

# Get a summary of the ANOVA
summary(fit)

# Perform Pearson Correlation Analysis on the combined pre and post data
correlation_test <- cor.test(combined_data$Trust, combined_data$HBehaviorChanges, method = "pearson")

# Output the result
print(correlation_test)

# Interpret the Pearson correlation coefficient value
rho <- correlation_test$estimate
cat("Pearson's r:", rho, "\n")

# Interpret the p-value
p_value <- correlation_test$p.value
cat("p-value:", p_value, "\n")

# Recoding the 'Trust' variable into 'Low', 'Medium', and 'High'
combined_data$Trust_Category <- cut(combined_data$Trust,
                                    breaks = c(0.5, 2.5, 3.5, 5.5),
                                    labels = c("Low", "Medium", "High"),
                                    include.lowest = TRUE)

# Now your Trust variable is categorized, you can visualize the distribution with boxplots
ggplot(combined_data, aes(x = Trust_Category, y = HBehaviorChanges, fill = Time)) +
  geom_boxplot() +
  labs(title = "Boxplot of Health Behavior Changes by Trust Category",
       x = "Trust Category",
       y = "Health Behavior Changes Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Generating a scatter plot with a regression line
ggplot(combined_data, aes(x = Trust, y = HBehaviorChanges)) +
  geom_point(aes(color = Time), size = 3) + 
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  
  labs(title = "Correlation Between Trust and Health Behavior Changes",
       x = "Trust",
       y = "Health Behavior Changes") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top") 


#Hypothesis 2


#paired-t test

paired_test_result <- t.test(composite_scores_dfP$Perception_of_Effect, composite_scores_dfPo$Perception_of_Effect, paired = TRUE)

print(paired_test_result)

# Perform Pearson Correlation Analysis on the pre and post Perception of Effect data
pearson_result <- cor.test(composite_scores_dfP$Perception_of_Effect, 
                           composite_scores_dfPo$Perception_of_Effect, 
                           method = "pearson")

# Print the results
print(pearson_result)


# Plot the pre vs post perceived effectiveness
ggplot() +
  geom_point(aes(x = composite_scores_dfP$Perception_of_Effect, y = composite_scores_dfPo$Perception_of_Effect)) +
  geom_smooth(aes(x = composite_scores_dfP$Perception_of_Effect, y = composite_scores_dfPo$Perception_of_Effect), 
              method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of Pre- vs. Post-Perceived Effectiveness",
       x = "Pre-Survey Perceived Effectiveness",
       y = "Post-Survey Perceived Effectiveness") +
  theme_minimal()

# Prepare the data frame
effectiveness_data <- data.frame(
  Time = c("Pre", "Post"),
  Effectiveness = c(mean(composite_scores_dfP$Perception_of_Effect), mean(composite_scores_dfPo$Perception_of_Effect))
)

# Create a bar plot
ggplot(effectiveness_data, aes(x = Time, y = Effectiveness, fill = Time)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  labs(title = "Comparison of Perceived Effectiveness Before and After",
       x = "Survey Time",
       y = "Mean Perceived Effectiveness") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") 




#Hypothesis3 

composite_scores_dfP <- composite_scores_dfP %>%
  rename(Perception_Pre = Perception_of_Effect, Behavior_Pre = HBehaviorChanges)

composite_scores_dfPo <- composite_scores_dfPo %>%
  rename(Perception_Post = Perception_of_Effect, Behavior_Post = HBehaviorChanges)

# Merging the datasets on ParticipantID

merged_data <- full_join(composite_scores_dfP, composite_scores_dfPo, by = "ParticipantID")

print(merged_data)

# Calculate the differences in perceptions and behaviors
diff_data <- merged_data %>%
  mutate(
    Perception_Diff = Perception_Post - Perception_Pre,
    Behavior_Diff = Behavior_Post - Behavior_Pre
  )

print(diff_data)

# Perform paired t-test on the differences
t_test_results <- t.test(diff_data$Behavior_Pre, diff_data$Behavior_Post, paired = TRUE)

print(t_test_results)

# Perform Pearson Correlation Analysis between changes in perception of effectiveness and behavior change
correlation_results <- cor.test(diff_data$Perception_Diff, diff_data$Behavior_Diff, method = "pearson")

# Print correlation results
print(correlation_results)

# Interpret the Pearson correlation coefficient value
rho <- correlation_results$estimate
cat("Pearson's r:", rho, "\n")

# Interpret the p-value
p_value <- correlation_results$p.value
cat("p-value:", p_value, "\n")


# Plotting the pre and post perceptions and behaviors
ggplot(diff_data, aes(x = Perception_Diff, y = Behavior_Diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Change in Perception vs. Change in Behavior",
       x = "Change in Perception Score",
       y = "Change in Behavior Score") +
  theme_minimal()

# Boxplot to visualize the distribution of changes
ggplot(diff_data, aes(x = factor(0), y = Perception_Diff)) +
  geom_boxplot() +
  labs(title = "Distribution of Perception Score Changes", y = "Change in Perception Score") +
  theme_minimal()

ggplot(diff_data, aes(x = factor(0), y = Behavior_Diff)) +
  geom_boxplot() +
  labs(title = "Distribution of Behavior Score Changes", y = "Change in Behavior Score") +
  theme_minimal()