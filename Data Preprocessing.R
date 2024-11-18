# Install necessary packages
#install.packages(c("dplyr", "caret", "readxl"))
#install.packages("openxlsx")

# Load necessary libraries
library(dplyr)
library(caret)
library(readxl)
library(openxlsx)

# Load the dataset
dataset <- read_excel("C:/Users/calin/Downloads/Project.xlsx")

# View the frequency distribution
attrition_counts <- table(dataset$Attrition)
print(attrition_counts)

# Calculate proportions
attrition_props <- prop.table(attrition_counts)
print(attrition_props)

# STEP 1: OUTLIER Detection

# Check for numerical variables
num_vars <- dataset %>%
  select_if(is.numeric) %>%
  names()

# Initialize a logical vector to flag outliers
outlier_flags <- rep(FALSE, nrow(dataset))

# Loop over numerical variables to identify outliers
for (var in num_vars) {
  Q1 <- quantile(dataset[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataset[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify outliers
  is_outlier <- dataset[[var]] < lower_bound | dataset[[var]] > upper_bound
  
  # Update the outlier flags
  outlier_flags <- outlier_flags | is_outlier
}

# Number of outliers detected
sum(outlier_flags)

# List outliers for each variable
for (var in num_vars) {
  Q1 <- quantile(dataset[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataset[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outlier_values <- dataset[[var]][dataset[[var]] < lower_bound | dataset[[var]] > upper_bound]
  
  if (length(outlier_values) > 0) {
    cat('\nOutliers detected in', var, ':\n')
    print(outlier_values)
  }
}

# Will not remove any outliers as the data set is on the small side, and observations look relevant 

# Step 2: Exclude pointless variables & apply one-hot encoding

# List of features to remove
unwanted_features <- c("EmployeeCount", "StandardHours", "Over18", "EmployeeNumber")

# Remove the unwanted features from the data set
dataset <- dataset %>%
  select(-one_of(unwanted_features))

# Proceed with your existing preprocessing steps

# List of relevant categorical variables to encode
categorical_vars <- c(
  "Attrition",
  "BusinessTravel",
  "Department",
  "EducationField",
  "Gender",
  "JobRole",
  "MaritalStatus",
  "OverTime"
)

# Convert categorical variables to factors if they are not already
dataset[categorical_vars] <- lapply(dataset[categorical_vars], as.factor)

# Apply one-hot encoding using the dummyVars function from the caret package
dummies_model <- dummyVars(~ ., data = dataset[categorical_vars], fullRank = TRUE)

# Create a new data frame with the encoded variables
encoded_categorical <- predict(dummies_model, newdata = dataset[categorical_vars])

# Convert the result to a data frame
encoded_categorical <- as.data.frame(encoded_categorical)

# Remove the original categorical variables from the dataset
dataset_numeric <- dataset %>%
  select(-one_of(categorical_vars))

# Combine the numeric variables with the encoded categorical variables
preprocessed_dataset <- cbind(dataset_numeric, encoded_categorical)

# STEP 3: Scale the data


# 'Attrition.Yes' is the target variable after one-hot encoding
target_var <- "Attrition.Yes"

# Identify one-hot encoded variables
# Assuming that one-hot encoded variables contain a dot (.) in their names
one_hot_vars <- grep("\\.", names(preprocessed_dataset), value = TRUE)

# Identify continuous numerical variables to scale
# Exclude the target variable and one-hot encoded variables from scaling
numeric_vars <- setdiff(names(preprocessed_dataset), c(target_var, one_hot_vars))

# Apply scaling to the continuous numerical variables only
preprocessed_dataset_scaled <- preprocessed_dataset
preprocessed_dataset_scaled[numeric_vars] <- scale(preprocessed_dataset[numeric_vars])

# View the first few rows of the scaled dataset
head(preprocessed_dataset_scaled)





# STEP 4: Duplicate the scaled dataset with specified names

# Duplicate the dataset for Decision Tree and Random Forest
dataset_preprocessing_dt_rf <- preprocessed_dataset_scaled

# Duplicate the dataset for PCA
dataset_preprocessing_PCA <- preprocessed_dataset_scaled

# Save 'dataset_preprocessing_dt_rf' to an Excel file
write.xlsx(dataset_preprocessing_dt_rf, file = "dataset_preprocessing_dt_rf.xlsx", rownames = FALSE)

# Save 'dataset_preprocessing_PCA' to an Excel file
write.xlsx(dataset_preprocessing_PCA, file = "dataset_preprocessing_PCA.xlsx", rownames = FALSE)

