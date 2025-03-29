#Install packages
#install(ggfortify)


#Loading Libraries
library(datasets)
library(ggplot2)
library(dplyr)
library(ggfortify)



# Dataset: 1
# IRIS Dataset


# Loading the data set
data(iris)


#1: Data Overview
head(iris)

# Display the structure of the data set
str(iris)

# Dimension and Column names of dataset
dim(iris)
colnames(iris)

# Number of observations
num_observations_iris <- nrow(iris)

# Number of variables
num_variables_iris <- ncol(iris)

# Print the results
cat("Number of observations in IRIS Dataset:", num_observations_iris, "\n")
cat("Number of variables in IRIS Dataset:", num_variables_iris, "\n")



#2 Summary Statistics for key variable: Sepal.Length
summary(iris$Sepal.Length)

mean_Sepal.Length <- mean(iris$Sepal.Length, na.rm = TRUE)
median_Sepal.Length <- median(iris$Sepal.Length, na.rm = TRUE)
sd_Sepal.Length <- sd(iris$Sepal.Length, na.rm = TRUE)
min_Sepal.Length <- min(iris$Sepal.Length, na.rm = TRUE)
max_Sepal.Length <- max(iris$Sepal.Length, na.rm = TRUE)

cat("Mean of Sepal.Length:", mean_Sepal.Length, "\n")
cat("Median of Sepal.Length:", median_Sepal.Length, "\n")
cat("Standard deviation of Sepal.Length:", sd_Sepal.Length, "\n")
cat("Minimum of Sepal.Length:", min_Sepal.Length, "\n")
cat("Maximum of Sepal.Length:", max_Sepal.Length, "\n")


#3 Distribution visualization for Sepal.Length


# Visualization of distribution
# Create a histogram
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.3, fill = "pink", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Sepal Length", 
       x = "Sepal Length", 
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Create a box plot
ggplot(iris, aes(y = Sepal.Length, x = "", fill = Species)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(title = "Box Plot of Sepal Length", 
       x = "", 
       y = "Sepal Length") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_fill_brewer(palette = "Set3")


#4. Categorical variable analysis

# Bar plot for 'Species'
ggplot(iris, aes(x = Species, fill = Species)) +
  geom_bar(width = 0.6, color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Bar Plot of Iris Species", 
       x = "Species", 
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13)) +
  scale_fill_brewer(palette = "Set2")




# Multivariate Analysis:


#5. Correlation analysis

# Select two numerical variables: Sepal.Length and Petal.Length
var1 <- iris$Sepal.Length
var2 <- iris$Petal.Length

# Calculate the Pearson correlation coefficient
correlation_iris <- cor(var1, var2, method = "pearson")

# Print the correlation coefficient
cat("Pearson Correlation Coefficient between Sepal.Length and Petal.Length:", correlation_iris, "\n")


#5. Scatter Plot Visualization
# Create a scatter plot with a trend line
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Scatter Plot of Sepal.Length vs Petal.Length", 
       x = "Sepal Length", 
       y = "Petal Length") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 13))


#7. Multiple regression


# Fit a multiple regression model predicting Sepal.Length
model_iris <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)

# Display the summary of the model
summary(model_iris)

# Plot the fitted line (Sepal.Length vs Fitted values)
library(ggplot2)
ggplot(iris, aes(x = fitted(model_iris), y = Sepal.Length)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "magenta", se = TRUE) +
  theme_minimal() +
  labs(title = "Fitted Values vs Actual Sepal.Length",
       x = "Fitted Values",
       y = "Actual Sepal.Length") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


#8. Model diagnostics

# Residuals vs Fitted plot
ggplot(data = data.frame(fitted = fitted(model_iris), residuals = residuals(model_iris)),
       aes(x = fitted, y = residuals)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Normal Q-Q plot
qqnorm(residuals(model_iris), main = "Normal Q-Q Plot", col = "steelblue")
qqline(residuals(model_iris), col = "red", lwd = 2)

# Scale-Location plot
plot(model_iris, which = 3)

# Residuals vs Leverage plot
plot(model_iris, which = 5)


par(mfrow = c(2, 2))
plot(model_iris)




#9: Principal component analysis (PCA)


# Select only the numerical variables (remove the Species column)
numerical_data_iris <- iris[, 1:4]

# Perform PCA
pca_iris <- prcomp(numerical_data_iris, center = TRUE, scale. = TRUE)


# Summary of PCA to see the proportion of variance explained by each principal component
summary_pca_iris <- summary(pca_iris)

# Print the summary of PCA (explained variance)
summary(pca_iris)


# Store explained variance and cumulative variance in variables
explained_variance_iris <- summary_pca_iris$importance[2,]  # Proportion of variance explained by each PC
cumulative_variance_iris <- cumsum(explained_variance_iris)  # Cumulative variance


# Plot the scree plot (explained variance)
screeplot(pca_iris, main = "Scree Plot", col = "steelblue", type = "lines", lwd = 2)



# Bar plot of explained variance
variance_plot <- data.frame(PC = 1:length(explained_variance_iris), ExplainedVariance = explained_variance_iris)
ggplot(variance_plot, aes(x = factor(PC), y = ExplainedVariance)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_line(aes(x = factor(PC), y = cumulative_variance_iris), color = "brown", size = 1) +
  geom_point(aes(x = factor(PC), y = cumulative_variance_iris), color = "brown") +
  labs(title = "Explained Variance in PCA", x = "Principal Components", y = "Explained Variance") +
  theme_minimal()


# Print number of PCs that contribute to 90% variance
num_pcs_90_variance_iris <- which(cumulative_variance_iris >= 0.90)[1]
cat("Number of PCs contributing to 90% variance:", num_pcs_90_variance_iris, "\n")




#10. PCA Interpretation

# Biplot of the first two principal components
biplot(pca_iris, main = "PCA Biplot", col = c("steelblue", "red"),cex=0.8)

# Alternatively, using ggplot2 for a clearer visualization
library(ggplot2)

# Get PCA scores
pca_scores_iris <- data.frame(pca_iris$x)

# Add the Species column to the PCA scores for coloring
pca_scores_iris$Species <- iris$Species

# Plot the first two principal components
ggplot(pca_scores_iris, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_minimal() +
  labs(title = "PCA - First Two Principal Components",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  scale_color_manual(values = c("setosa" = "blue", "versicolor" = "green", "virginica" = "red"))

#_______________________________________________________________________________________________________________________________________



# Dataset: 2
# Diamonds Dataset


# Loading the data set
data(diamonds)


#1: Data Overview
head(diamonds)


# Display the structure of the data set
str(diamonds)

# No. of observations and Variables
dim(diamonds)
colnames(diamonds)

# Dimension and Column names of dataset
dim(diamonds)
colnames(diamonds)

# Number of observations
num_observations_diamonds <- nrow(diamonds)

# Number of variables
num_variables_diamonds <- ncol(diamonds)

# Print the results
cat("Number of observations in diamonds dataset:", num_observations_diamonds, "\n")
cat("Number of variables in diamonds dataset:", num_variables_diamonds, "\n")



#2 Summary Statistics for key variable: depth
summary(diamonds$depth)

mean_depth <- mean(diamonds$depth, na.rm = TRUE)
median_depth <- median(diamonds$depth, na.rm = TRUE)
sd_depth <- sd(diamonds$depth, na.rm = TRUE)
min_depth <- min(diamonds$depth, na.rm = TRUE)
max_depth <- max(diamonds$depth, na.rm = TRUE)

cat("Mean of depth:", mean_depth, "\n")
cat("Median of depth:", median_depth, "\n")
cat("Standard depth:", sd_depth, "\n")
cat("Minimum of depth:", min_depth, "\n")
cat("Maximum of depth:", max_depth, "\n")



#3 Distribution visualization for depth


# Histogram for the depth column
ggplot(data = diamonds, aes(x = depth)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Diamond Depth",
       x = "Depth",
       y = "Frequency") +
  theme_minimal()

# Boxplot for the depth column
ggplot(data = diamonds, aes(y = depth)) +
  geom_boxplot(fill = "tomato", color = "black", alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  labs(title = "Boxplot of Diamond Depth",
       y = "Depth") +
  theme_minimal()


#4. Categorical variable analysis

# Bar plot for the 'cut' variable
ggplot(data = diamonds, aes(x = cut, fill = cut)) +
  geom_bar(color = "black", alpha = 0.8) +
  scale_fill_brewer(palette = "Set3") +  # Nice color palette
  labs(title = "Distribution of Diamond Cuts",
       x = "Cut Quality",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend for a cleaner look
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity



# Multivariate Analysis:


#5. Correlation analysis

# Select two numerical variables
var1 <- diamonds$carat
var2 <- diamonds$price

# Calculate the Pearson correlation coefficient
correlation_diamond <- cor(var1, var2, method = "pearson")

# Print the correlation
cat("Pearson Correlation Coefficient between carat and price:", correlation_diamond, "\n")


#5. Scatter Plot Visualization

# Scatter plot with trend line
ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point(color = "pink", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line
  labs(title = "Scatter Plot of Carat vs Price",
       x = "Carat",
       y = "Price") +
  theme_minimal()



#7. Multiple regression


# Fit the regression model predicting price of diamond
model_diamonds <- lm(price ~ carat + cut + depth, data = diamonds)

# Display the summary of the model
summary(model_diamonds)

# Add fitted values to the dataset
diamonds$fitted_values <- fitted(model_diamonds)

# Scatter plot of fitted vs actual price
ggplot(diamonds, aes(x = fitted_values, y = price)) +
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Fitted vs Actual Prices",
       x = "Fitted Values",
       y = "Actual Prices") +
  theme_minimal()




#8. Model diagnostics

# Add residuals to the dataset
diamonds$residuals <- residuals(model_diamonds)

# Residual plot for homoscedasticity
ggplot(diamonds, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "darkorange", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()



# Q-Q plot for residuals
qqnorm(diamonds$residuals, main = "Q-Q Plot of Residuals", col = "blue")
qqline(diamonds$residuals, col = "red")


# Scale-Location plot
plot(model_diamonds, which = 3)

# Residuals vs Leverage plot
plot(model_diamonds, which = 5)

par(mfrow = c(2, 2))
plot(model_diamonds)



# Histogram of residuals for normality
ggplot(diamonds, aes(x = residuals)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()




#9: Principal component analysis (PCA)

# Select numerical variables for PCA
numerical_vars_diamonds <- diamonds[, c("carat", "depth", "table", "x", "y", "z")]

# Standardize the data
scaled_data_diamonds <- scale(numerical_vars_diamonds)

# Perform PCA
pca_diamonds_result <- prcomp(scaled_data_diamonds, center = TRUE, scale. = TRUE)



# Summary of PCA to see the proportion of variance explained by each principal component
summary_pca_diamonds <- summary(pca_diamonds_result)

# Print the summary of PCA (explained variance)
summary(pca_diamonds_result)



# Explained variance
explained_variances_diamonds <- pca_diamonds_result$sdev^2 / sum(pca_diamonds_result$sdev^2)
cumulative_variance_diamonds <- cumsum(explained_variances_diamonds)

# Scree plot
library(ggplot2)
scree_data <- data.frame(
  Component = seq_along(explained_variances_diamonds),
  Variance = explained_variances_diamonds,
  CumulativeVariance = cumulative_variance_diamonds
)

ggplot(scree_data, aes(x = Component)) +
  geom_bar(aes(y = Variance), stat = "identity", fill = "lightgreen", alpha = 0.7) +
  geom_line(aes(y = CumulativeVariance), group = 1, color = "red", size = 1.2) +
  geom_point(aes(y = CumulativeVariance), color = "red", size = 2) +
  labs(title = "Scree Plot: Explained Variance",
       x = "Principal Component",
       y = "Variance Explained") +
  theme_minimal()


# Plot the scree plot (explained variance)
screeplot(pca_diamonds_result, main = "Scree Plot: Diamonds", col = "steelblue", type = "lines", lwd = 2)



num_components_diamonds <- which(cumulative_variance_diamonds >= 0.90)[1]  # Components explaining 90% variance
cat("Number of components chosen:", num_components_diamonds, "\n")



#10. PCA Interpretation

# Select numerical columns for PCA
diamonds_numeric <- diamonds[, c("carat", "depth", "table", "price", "x", "y", "z")]

# Log-transform the data (optional, to reduce skewness)
diamonds_numeric <- log1p(diamonds_numeric)

# Perform PCA
pca_result_diamonds_2 <- prcomp(diamonds_numeric, scale. = TRUE)

# Visualize PCA using a biplot
biplot(pca_result_diamonds_2, scale = 0)

# Alternative: Create a ggplot-based biplot
autoplot(pca_result_diamonds_2, data = diamonds, colour = 'cut', 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  theme_minimal() +
  labs(title = "PCA Biplot of Diamonds Data",
       x = "PC1",
       y = "PC2")



#______________________________________________________________________________________________________________________________


#Dataset: 3 
#MPG Dataset
# Miles per Gallon dataset


#Loading the data set

data(mpg)

#1: Data Overview
head(mpg)

# Display the structure of the data set
str(mpg)

# Dimension and Column names of dataset
dim(mpg)
colnames(mpg)

# Number of observations
num_observations_mpg <- nrow(mpg)

# Number of variables
num_variables_mpg <- ncol(mpg)

# Print the results
cat("Number of observations in MPG dataset:", num_observations_mpg, "\n")
cat("Number of variables in MPG dataset:", num_variables_mpg, "\n")



#2 Summary Statistics for key variable: displ
summary(mpg$displ)

mean_displ <- mean(mpg$displ, na.rm = TRUE)
median_displ <- median(mpg$displ, na.rm = TRUE)
sd_displ <- sd(mpg$displ, na.rm = TRUE)
min_displ <- min(mpg$displ, na.rm = TRUE)
max_displ <- max(mpg$displ, na.rm = TRUE)

cat("Mean of Engine Displacement:", mean_displ, "\n")
cat("Median of Engine Displacement:", median_displ, "\n")
cat("Standard deviation of Engine Dispalcement:", sd_displ, "\n")
cat("Minimum of Engine Displacement:", min_displ, "\n")
cat("Maximum of Engine Displacement:", max_displ, "\n")



#3 Distribution visualization for displ

# Visualization of distribution
# Create a histogram
ggplot(mpg, aes(x = displ)) +
  geom_histogram(binwidth = 0.5, fill = "coral", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Engine Displacement (displ)", x = "Engine Displacement (L)", y = "Frequency")

# Create a box plot
ggplot(mpg, aes(y = displ)) +
  geom_boxplot(fill = "lightgreen", color = "black", outlier.colour = "red", outlier.shape = 16) +
  theme_minimal() +
  labs(title = "Box Plot of Engine Displacement (displ)", y = "Engine Displacement (L)")



#4. Categorical variable analysis

# Create a bar plot for the 'class' categorical variable 
ggplot(mpg, aes(x = class, fill = class)) +
  geom_bar(color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Car Classes", x = "Car Class", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  scale_fill_brewer(palette = "Set3") # Use a color palette for better color variation


# Multivariate Analysis:


#5. Correlation analysis


#Pearson correlation coefficient between 'displ' and 'hwy'
correlation <- cor(mpg$displ, mpg$hwy)
print(paste("Pearson Correlation Coefficient between displ and hwy:", round(correlation, 3)))



#6. Scatter Plot Visualization

#Scatter Plot Visualization with trend line
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "coral", alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line (linear model)
  theme_minimal() +
  labs(title = "Scatter Plot of Displacement vs Highway MPG", x = "Engine Displacement (L)", y = "Highway MPG")


#7. Multiple regression

#Fit a linear regression model
model_mpg <- lm(hwy ~ displ + cty + year, data = mpg)

# Display the summary of the model
summary(model_mpg)

# Interpret the coefficients:
# The coefficients will be displayed in the summary, and they represent the effect
# of each predictor on the response variable 'hwy', holding other variables constant.

# Plot the fit line:
# First, we'll predict the 'hwy' values based on the model
mpg$predicted_hwy <- predict(model_mpg, newdata = mpg)

# Now, plot the data points and the regression line


library(ggplot2)
ggplot(mpg, aes(x = fitted(model_mpg), y = hwy)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Fitted Values vs Actual Highway miles per gallon",
       x = "Fitted Values",
       y = "Actual hwy") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



# 8. Model Diagnostics - Plot residuals
# Calculate residuals
mpg$residuals <- residuals(model_mpg)

# Plot residuals vs fitted values
ggplot(mpg, aes(x = predicted_hwy, y = residuals)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Zero line
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")

# Histogram of residuals to check for normality
ggplot(mpg, aes(x = residuals)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# Q-Q plot for normality check
ggplot(mpg, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "Q-Q Plot of Residuals")


# Scale-Location plot
plot(model_mpg, which = 3)

# Residuals vs Leverage plot
plot(model_mpg, which = 5)

par(mfrow = c(2, 2))
plot(model_mpg)



#9: Principal component analysis (PCA)

# Select only the numerical columns (excluding 'class' and other non-numerical columns)
numerical_vars_mpg <- mpg %>%
  select(displ, cty, hwy, year)

#Perform PCA on the numerical variables
pca_mpg_result <- prcomp(numerical_vars_mpg, scale. = TRUE)

#Scree Plot (Variance explained by each component)
screeplot(pca_mpg_result, main = "Scree Plot", col = "blue", type = "lines", pch = 19)


# Summary of PCA to see the proportion of variance explained by each principal component
summary_pca_mpg <- summary(pca_mpg_result)

# Print the summary of PCA (explained variance)
summary(pca_mpg_result)

# Store explained variance and cumulative variance in variables
explained_variance_mpg <- summary_pca_mpg$importance[2,]  # Proportion of variance explained by each PC
cumulative_variance_mpg <- cumsum(explained_variance_mpg)  # Cumulative variance


# Bar plot of explained variance
variance_plot <- data.frame(PC = 1:length(explained_variance_mpg), ExplainedVariance = explained_variance_mpg)
ggplot(variance_plot, aes(x = factor(PC), y = ExplainedVariance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(x = factor(PC), y = cumulative_variance_mpg), color = "red", size = 1) +
  geom_point(aes(x = factor(PC), y = cumulative_variance_mpg), color = "red") +
  labs(title = "Explained Variance in PCA", x = "Principal Components", y = "Explained Variance") +
  theme_minimal()


# Print number of PCs that contribute to 90% variance
num_pcs_90_variance_mpg <- which(cumulative_variance_mpg >= 0.90)[1]
cat("Number of PCs contributing to 90% variance:", num_pcs_90_variance_mpg, "\n")




# 10. PCA Interpretation - Visualize PCA Results (Biplot)

# Select numerical columns for PCA
mpg_numeric <- mpg[, c("displ", "hwy", "cty")]

# Perform PCA
pca_result_mpg_2 <- prcomp(mpg_numeric, scale. = TRUE)

# Visualize PCA using a biplot
biplot(pca_result_mpg_2, scale = 0)

# Alternative: Create a ggplot-based biplot
autoplot(pca_result_mpg_2, data = mpg, colour = 'class', 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  theme_minimal() +
  labs(title = "PCA Biplot of MPG Data",
       x = "PC1",
       y = "PC2")


#_____________________________________________________________________________________________________________________

#Dataset 4
# Gapminder dataset

library(gapminder)

# Loading the data set
data(gapminder)

#1: Data Overview
head(gapminder)

# Display the structure of the data set
str(gapminder)

# Dimension and Column names of dataset
dim(gapminder)
colnames(gapminder)

# Number of observations
num_observations_gapminder <- nrow(gapminder)

# Number of variables
num_variables_gapminder <- ncol(gapminder)

# Print the results
cat("Number of observations in Dataset gapminder:", num_observations_gapminder, "\n")
cat("Number of variables in Dataset gapminder:", num_variables_gapminder, "\n")



#2 Summary Statistics for key variable: lifeExp
summary(gapminder$lifeExp)

mean_lifeExp <- mean(gapminder$lifeExp, na.rm = TRUE)
median_lifeExp <- median(gapminder$lifeExp, na.rm = TRUE)
sd_lifeExp <- sd(gapminder$lifeExp, na.rm = TRUE)
min_lifeExp <- min(gapminder$lifeExp, na.rm = TRUE)
max_lifeExp <- max(gapminder$lifeExp, na.rm = TRUE)

cat("Mean of lifeExp:", mean_lifeExp, "\n")
cat("Median of lifeExp:", median_lifeExp, "\n")
cat("Standard deviation of lifeExp:", sd_lifeExp, "\n")
cat("Minimum of lifeExp:", min_lifeExp, "\n")
cat("Maximum of lifeExp:", max_lifeExp, "\n")



#3 Distribution visualization for Life Expectancy

# 1. Histogram for 'lifeExp' column
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Life Expectancy", x = "Life Expectancy", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Box Plot for 'lifeExp' column
ggplot(gapminder, aes(y = lifeExp)) +
  geom_boxplot(fill = "lightcoral", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Box Plot of Life Expectancy", y = "Life Expectancy") +
  theme(axis.title.x = element_blank()) # Remove x-axis label since it's not needed for boxplot



#4. Categorical variable analysis

#Categorical Variable Analysis - Bar plot for the 'continent' column
ggplot(gapminder, aes(x = continent, fill = continent)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +  # Color palette for different categories
  theme_minimal() +
  labs(title = "Distribution of Continent in the Gapminder Dataset", 
       x = "Continent", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



#Multivariate Analysis

#5. Correlation Analysis

# Select two numerical variables (e.g., GDP per capita and life expectancy)
gdp_per_capita <- gapminder$gdpPercap
life_expectancy <- gapminder$lifeExp

# Calculate Pearson correlation coefficient
correlation_gapminder <- cor(gdp_per_capita, life_expectancy, method = "pearson")

# Print the correlation value
print(paste("Pearson correlation coefficient:", round(correlation_gapminder, 3)))



#6. Scatter Plot Visualization

# Create a scatter plot with a trend line
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), alpha = 0.6) +  # Scatter plot with points colored by continent
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trend line
  scale_x_log10() +  # Use logarithmic scale for the x-axis (gdpPercap)
  labs(title = "Relationship between GDP per Capita and Life Expectancy",
       x = "GDP per Capita (log scale)", 
       y = "Life Expectancy") +
  theme_minimal()



# 7. Multiple Regression

# Fit a linear regression model predicting life expectancy (lifeExp) using GDP per capita (gdpPercap) and population (pop) as predictors
model_gapminder <- lm(lifeExp ~ gdpPercap + pop, data = gapminder)

# Display the summary of the model
summary(model_gapminder)


# Plot the fitted line (lifeExp vs Fitted values)
library(ggplot2)
ggplot(gapminder, aes(x = fitted(model_gapminder), y = lifeExp)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Fitted Values vs Actual Life Expectancy",
       x = "Fitted Values",
       y = "Actual lifeExp") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



# 8. Model Diagnostics

# Plot the residuals of the regression model
# Extract residuals and fitted values from the model
residuals_gapminder <- residuals(model_gapminder)
fitted_values_gapminder <- fitted(model_gapminder)

# Create a plot for residuals vs fitted values (Check for homoscedasticity)
ggplot(data = data.frame(fitted_values = fitted_values_gapminder, residuals = residuals_gapminder), 
       aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", 
       y = "Residuals") +
  theme_minimal()

# Check for normality of residuals using a Q-Q plot
qqnorm(residuals_gapminder, main = "Q-Q Plot of Residuals")
qqline(residuals_gapminder, col = "red")


# Scale-Location plot
plot(model_gapminder, which = 3)

# Residuals vs Leverage plot
plot(model_gapminder, which = 5)

par(mfrow = c(2, 2))
plot(model_gapminder)



#9: Principal component analysis (PCA)

# Select only the numerical variables (gdpPercap, lifeExp, and pop)
numerical_data_gapminder <- gapminder[, c("gdpPercap", "lifeExp", "pop")]

# Standardize the data (important for PCA)
numerical_data_scaled_gapminder <- scale(numerical_data_gapminder)

# Perform PCA
pca_gapminder <- prcomp(numerical_data_scaled_gapminder, center = TRUE, scale. = TRUE)

# Summary of PCA to see the proportion of variance explained by each principal component
summary_pca_gapminder <- summary(pca_gapminder)


# Print the summary of PCA (explained variance)
summary(pca_gapminder)


# Store explained variance and cumulative variance in variables
explained_variance_gapminder <- summary_pca_gapminder$importance[2,]  # Proportion of variance explained by each PC
cumulative_variance_gapminder <- cumsum(explained_variance_gapminder)  # Cumulative variance

# Bar plot of explained variance
variance_plot <- data.frame(PC = 1:length(explained_variance_gapminder), ExplainedVariance = explained_variance_gapminder)
ggplot(variance_plot, aes(x = factor(PC), y = ExplainedVariance)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  geom_line(aes(x = factor(PC), y = cumulative_variance_gapminder), color = "black", size = 1) +
  geom_point(aes(x = factor(PC), y = cumulative_variance_gapminder), color = "black") +
  labs(title = "Explained Variance in PCA", x = "Principal Components", y = "Explained Variance") +
  theme_minimal()

# Scree plot

# Plot the scree plot (explained variance)
screeplot(pca_gapminder, main = "Scree Plot", col = "steelblue", type = "lines", lwd = 2)


# Print number of PCs that contribute to 90% variance
num_pcs_90_variance <- which(cumulative_variance_gapminder >= 0.90)[1]
cat("Number of PCs contributing to 90% variance:", num_pcs_90_variance, "\n")



# 10. PCA Interpretation

# Prepare the data for PCA
# Select numerical columns for PCA (e.g., lifeExp, pop, gdpPercap)
gapminder_numeric <- gapminder[, c("lifeExp", "pop", "gdpPercap")]

# Log-transform "pop" and "gdpPercap" to reduce skewness (optional)
gapminder_numeric$log_pop <- log10(gapminder$pop)
gapminder_numeric$log_gdpPercap <- log10(gapminder$gdpPercap)

# Remove the original skewed columns
gapminder_pca <- gapminder_numeric[, c("lifeExp", "log_pop", "log_gdpPercap")]

# Perform PCA
pca_result_gapminder <- prcomp(gapminder_pca, scale. = TRUE)

# Visualize PCA using a biplot
biplot(pca_result_gapminder, scale = 0)

# Alternative: Create a ggplot-based biplot
autoplot(pca_result_gapminder, data = gapminder, colour = 'continent',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3) +
  theme_minimal() +
  labs(title = "PCA Biplot of Gapminder Data",
       x = "PC1",
       y = "PC2")


#Thankyou
#Prashant Bisht
#24BM6JP42


#_______________________________________________________________________________________________________
