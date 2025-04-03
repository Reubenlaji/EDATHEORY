# -------------------------------
# 1. Load Dataset and Pre-processing
# -------------------------------
rm(.Random.seed)  # Remove any corrupted random seed
set.seed(123)  
library(readr)
library(dplyr)
library(mice)
library(ggplot2)
library(class)
library(stats)
library(cluster)

# Read dataset from CSV
url <- "https://raw.githubusercontent.com/salemprakash/EDA/main/Data/SuicideChina.csv"
data <- read_csv(url)

# Convert categorical variables to factors
data <- data %>% mutate(across(where(is.character), as.factor))

# Handling missing values using MICE imputation
imputed_data <- mice(data, method="pmm", m=5)
data <- complete(imputed_data)

# -------------------------------
# 2. Data Transformation
# -------------------------------
transform_data <- function(data_column, new_min, new_max) {
  old_min <- min(data_column)
  old_max <- max(data_column)
  scaled_data <- (data_column - old_min) / (old_max - old_min)
  transformed_data <- scaled_data * (new_max - new_min) + new_min
  return(transformed_data)
}

# Example transformations
data$Age_scaled <- transform_data(data$Age, -3, 3)
data$Age_normalized <- transform_data(data$Age, 0, 1)

# -------------------------------
# 3. Basic Statistical Analysis
# -------------------------------
mean_value <- mean(data$Age, na.rm = TRUE)
median_value <- median(data$Age, na.rm = TRUE)
variance_value <- var(data$Age, na.rm = TRUE)
sd_value <- sd(data$Age, na.rm = TRUE)

# Mode function
generate_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_value <- generate_mode(data$Age)

# -------------------------------
# 4. Covariance and Correlation
# -------------------------------
cov_value <- cov(data$Year, data$Age, use = "complete.obs")
cor_value <- cor(data$Year, data$Age, use = "complete.obs")

# -------------------------------
# 5. Exploratory Data Analysis (EDA)
# -------------------------------
# Univariate Analysis - Histogram
hist(data$Age, breaks=20, col="blue", main="Age Distribution", xlab="Age")

# Boxplot to check outliers
boxplot(data$Age, main="Boxplot of Age", col="red", horizontal = TRUE)

# Bivariate Analysis - Scatter plot
plot(data$Year, data$Age, main="Age vs Year", xlab="Year", ylab="Age", col=data$Sex)

# -------------------------------
# 6. Clustering - K-Means
# -------------------------------
set.seed(123)
kmeans_result <- kmeans(data[,c("Age", "Year")], centers = 3)
data$Cluster <- as.factor(kmeans_result$cluster)
ggplot(data, aes(x = Age, y = Year, color = Cluster)) + geom_point(size = 3)

# -------------------------------
# 7. Nearest Neighbour Classification
# -------------------------------
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index,]
test_data <- data[-train_index,]
predicted_class <- knn(train_data[,c("Age", "Year")], test_data[,c("Age", "Year")], train_data$Sex, k = 3)

# -------------------------------
# 8. Principal Component Analysis (PCA) & Regression
# -------------------------------
pca_result <- prcomp(data[,c("Age", "Year")], scale. = TRUE)
summary(pca_result)

lm_model <- lm(Age ~ Year, data = data)
summary(lm_model)
ggplot(data, aes(x = Year, y = Age)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red")
