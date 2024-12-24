# Loading the necessary libraries
library(readxl)
library(dplyr)
library(car)
library(ggplot2)

# Reading data from an Excel file
data <- read_excel("C:\\Users\\hp\\Downloads\\frequency_exercise_data.xlsx")

# Viewing a summary of the data
summary(data)

# Assumption Tests for ANOVA
# 1. Normality (Shapiro-Wilk Test)
norm_test <- data %>%
  group_by(Group) %>%
  summarise(Normality_p = shapiro.test(ExerciseFrequency)$p.value)
cat("Normality Test:\n")
print(norm_test)

# 2. Homogeneity of Variances (Levene's Test)
levene_test <- leveneTest(ExerciseFrequency ~ Group, data = data)
cat("\nHomogeneity of Variances Test:\n")
print(levene_test)

# 3. Independence (Assumed to be satisfied as this is simulated data)

# ANOVA Analysis
anova_result <- aov(ExerciseFrequency ~ Group, data = data)
cat("\nANOVA Results:\n")
summary(anova_result)

# Visualizing the results
ggplot(data, aes(x = Group, y = ExerciseFrequency, fill = Group)) +
  geom_boxplot() +
  labs(
    title = "Exercise Frequency by Group",
    x = "Group",
    y = "Exercise Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Interpretation of Results
if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("\nThe ANOVA results indicate a significant difference between groups at the 95% confidence level.\n")
} else {
  cat("\nThe ANOVA results indicate no significant difference between groups at the 95% confidence level.\n")
}
