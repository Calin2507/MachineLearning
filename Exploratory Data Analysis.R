# Install necessary packages
install.packages(c("tidyverse", "corrplot", "GGally", "readxl"))
install.packages("conflicted")
install.packages("reshape2")

# Load necessary libraries
library(tidyverse)
library(corrplot)
library(GGally)
library(conflicted)
library(ggplot2)
library(reshape2)
library(readxl)

# Load the dataset
dataset <- read_excel("C:/Users/calin/Downloads/Project.xlsx")

# Preview the dataset
head(dataset)
str(dataset)

# Check for missing values
sum(is.na(dataset))

# Summary Statistics for Numerical Variables
numeric_vars <- dataset %>%
  select_if(is.numeric)

summary(numeric_vars)

# Histograms for Numerical Variables
numeric_var_names <- names(numeric_vars)
for (var in numeric_var_names) {
  h <- ggplot(dataset, aes(x = .data[[var]])) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency")
  print(h)
}

# Correlation Matrix and Heatmap

# Exclude specific variables from numerical variables
numeric_vars_filtered <- numeric_vars %>%
  select(-EmployeeCount, -EmployeeNumber, -StandardHours)  # Exclude specific variables

# Compute the correlation matrix
cor_matrix_filtered <- cor(numeric_vars_filtered, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix_filtered)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix_filtered, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, number.cex = 0.7,
         addCoef.col = "black")

# Create a heatmap using ggplot2
# Melt the correlation matrix into long format
melted_cor <- melt(cor_matrix_filtered)

# Plot the heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "", y = "")

# Bar Charts for Categorical Variables
categorical_vars <- dataset %>%
  select_if(~ is.factor(.) || is.character(.))

categorical_var_names <- names(categorical_vars)
for (var in categorical_var_names) {
  if (var != "Attrition") {
    b <- ggplot(dataset, aes(x = .data[[var]])) +
      geom_bar(fill = "coral") +
      labs(title = paste("Bar Chart of", var), x = var, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(b)
  }
}

# Attrition Rate
attrition_rate <- dataset %>%
  count(Attrition) %>%
  mutate(Percentage = n / sum(n) * 100)
print(attrition_rate)

# Attrition by Category
for (var in categorical_var_names) {
  if (var != "Attrition") {
    p <- ggplot(dataset, aes(x = .data[[var]], fill = Attrition)) +
      geom_bar(position = "fill") +
      labs(title = paste("Attrition by", var), x = var, y = "Proportion") +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}

