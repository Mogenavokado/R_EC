install.packages("dplyr")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("olsrr")
install.packages("moments")
library(dplyr)
library(ggplot2)
library("readxl")
library(gridExtra)
library(olsrr)
library(moments)
#-------------------------------
# IMPORT DATA
#-------------------------------

file_path <- "E:/Github_SQL/Cars_complete(preworked).xlsx"
car_data <- read_excel(file_path)

View(car_data)
#-----------------------------------------------------------------------------
# DATA CLEANING
#-----------------------------------------------------------------------------
# Check for missing values
sum(is.na(car_data))

# Missing value locations
missing_value <- function(x)
  sum(is.na(x))
car_empty <- sapply(car_data, missing_value)
print(car_empty)

# Remove unneeded rows 
car_data_fixed <- subset(car_data, select = -c(11:13))

# Convert Uppercase letter to small letters if datatype is of character-type
small_letter <- function(x)
  if (is.character(x))
    tolower(x) else x
car_data_fixed <- data.frame(lapply(car_data_fixed, small_letter))
View(car_data_fixed)

# Convert non-numeric characters in column Pris  to numeric.
car_data_fixed$Pris <- as.numeric(gsub("[^0-9.]", "", as.character(car_data_fixed$Pris)))

# Remove square brackets from the "Color" column
car_data_fixed$Färg <- gsub("\\[|\\]", "", car_data_fixed$Färg)

# String manipulation, replaces spaces with underscores
# names(car_data_fixed) <- gsub(" ", "_", names(car_data_fixed))
-----------------------------------------------------------------------
# DATA EXPLORATION
-----------------------------------------------------------------------
# Structure
str(car_data_fixed)
# Summary of data frame
summary(car_data_fixed)


# Region analysis 
region <- data.frame(car_data_fixed$Län)

View(region)

# Count number of regions, including duplicates
region_counts <- region %>% count(car_data_fixed.Län)  # %>% works as a pipeline in r
region_counts <- region_counts[order(region_counts$n),]
print(region_counts)
sum(region_counts$n)

# Shows distinct regions
unique_region <- distinct(region)
#View(unique_region)

# Total car companies
company_counts <- car_data_fixed %>% count(Märke)
company_counts <- company_counts[order(company_counts$n),]
sum(company_counts$n)

#-------------------------------------------------------------------------------------
# CORRELATION USING PEARSON
#-------------------------------------------------------------------------------------
# is there correlation? using Pearson r method, 0 = no correlation
Mil_Pris <- cor(car_data_fixed$Miltal, car_data_fixed$Pris, method = "pearson")
print(correlation)

Häst_Pris <- cor(car_data_fixed$Hästkrafter, car_data_fixed$Pris, method = "pearson")
print(correlation)

År_Pris <- cor(car_data_fixed$Modellår, car_data_fixed$Pris)
print(correlation)

Drivning_Pris<- cor(car_data_fixed$Pris, car_data_fixed$Drivning)
print(correlation)

#Create Correlation dataframe
correlation <- as.data.frame(c(Mil_Pris,Häst_Pris,År_Pris,Drivning_Pris))
factor_names <- as.data.frame(c("Miltal-Pris","Hästkrafter-Pris","Modellår-Pris","Drivning-Pris"))
correlation$names <- as.data.frame(factor_names)
View(correlation)

-----------------------------------------------------------------------------------------------------
# CREATE PLOTS AND SEARCH FOR INSIGHTS
-----------------------------------------------------------------------------------------------------

pris_observation <- car_data_fixed[c("Miltal", "Pris", "Märke", "Modell", "Modellår")]
#View(pris_observation)

# Plot Price data

plot2 <- ggplot(pris_observation, aes(x = Pris)) +
  geom_histogram(binwidth = 10000, fill = "lightgreen", color = "black") +
  labs(title="Price Frequency",x = "Pris", y = "Frequency") +
  theme_minimal()

plot3 <- ggplot(pris_observation, aes(x = Modellår, y = Pris, color = factor(Märke))) +
  geom_point() +
  labs(title="Vehicle Price | per year | split by Company",x = "År", y = "Pris", color = "Märke") +
  theme(panel.background = element_rect(fill = 'black'))

grid.arrange(plot2, plot3, ncol = 2)

# Plot all continuous variables, observe which plot is correlated!
pairs(car_data_fixed[c("Miltal", "Modellår","Hästkrafter","Pris")],
      col="orange",
      pch=16,
      main = "Pairplot of all continous variable"
      )
-----------------------------------------------------------------------------------------------------
# EXPLORING THE SELECTED VARIABLE, IN THIS CASE HÄSTKRAFTER
-----------------------------------------------------------------------------------------------------
# Is the variable normally distributed?
hist(car_data_fixed$Hästkrafter)
ggplot(car_data_fixed, aes(x= Hästkrafter)) +
  geom_histogram(
    color="orange",
    fill="orange"
  )
boxplot(car_data_fixed$Hästkrafter)
ggplot(car_data_fixed, aes(x= Hästkrafter)) +
  geom_boxplot(fill="orange")

#-----------------------------------------------------------
# FITTING A LINEAR MODEL WITH NON NORMAL DISTRIBUTION
#-----------------------------------------------------------
attach(car_data_fixed)
lm_model <- lm(Pris ~ Hästkrafter)

summary(lm_model)
plot(lm_model)
abline(lm_model, col="red")
#-------------------------------------------------------------
# RESIDUALS ANALYSIS
#-------------------------------------------------------------
# is the model normal distributed?
ols_plot_resid_hist(lm_model)
ols_plot_resid_qq(lm_model)
skewness(car_data_fixed$Hästkrafter)
# p-value > 0.05 indicates we cannot reject h0 hypotheses, which means there is normality
shapiro.test(residuals(lm_model))

hist(residuals(lm_model))

qqnorm(residuals(lm_model), main = "Non Normal Q-Q Plot")
qqline(residuals(lm_model),col="red")
#------------------------------------------------------------------
# TRANSFORM NON NORMAL DISTRIBUTION TO NORMAL
#------------------------------------------------------------------
# The larger the value of skewness, the larger it differs from a normal distribution
skewness(car_data_fixed$Hästkrafter)
skewness(car_data_fixed$Pris)

# For moderate skew use sqrt() function
# Identify numerical columns (excluding 'Category')
numerical_columns <- sapply(car_data_fixed, is.numeric)
car_sqrt <- data.frame()

# Apply square root transformation to numerical columns
car_sqrt <- sqrt(car_data_fixed[numerical_columns])

# Fit normal distribution to linear model
lm_sqrt <- lm(Pris ~ Hästkrafter, data = car_sqrt)
summary(lm_sqrt)
#--------------------------------------------------------------------
# REDO RESIDUAL ANALYSIS
# At this point p-value should be 0.05868, thus we have attained normality
#--------------------------------------------------------------------
# is the model normal distributed
ols_plot_resid_hist(lm_sqrt)
ols_plot_resid_qq(lm_sqrt)

# p-value > 0.05 indicates we cannot reject h0 hypotheses, which means there is normality
shapiro.test(residuals(lm_sqrt))

hist(residuals(lm_sqrt))

# Manual qqplot
qqnorm(residuals(lm_sqrt))
qqline(residuals(lm_sqrt),col="red")
#------------------------------------------------------------
# EVALTUATION OF THE SIMPLE LINEAR MODEL
#------------------------------------------------------------
# Random sample
set.seed(1)
row.number <- sample(1:nrow(car_sqrt), 0.8*nrow(car_sqrt))

#subset our data
train <- car_sqrt[row.number,] # 80%
test <- car_sqrt[-row.number,] # 20%

# Estimate the linear fit with the training set
lm_fit0.8 <- lm(Pris~Hästkrafter, data = train)
summary(lm_fit0.8)

# Predict in the test dataset
prediction0.8 <- predict(lm_fit0.8, newdata = test)
summary(prediction0.8)
err0.8 <- prediction0.8 - test$Pris

# Root mean square error
rmse <- sqrt(mean(err0.8^2))
# Mean absolute percentage error
mape <- mean(abs(err0.8/test$Pris))
# Results
c(RMSE= rmse, mape=mape, R2=summary(lm_fit0.8)$r.squared)

#-------------------------------------------------------------
# MULTIPLE LINEAR REGRESSION
#-------------------------------------------------------------

attach(car_sqrt)
lm_multi <- lm(Pris~Hästkrafter+Modellår)

summary(lm_multi)

#-------------------------------------------------------------------------
# EVALUATIN OF THE MULTIPLE LINEAR REGRESSION
#-------------------------------------------------------------------------
# Random sample
set.seed(1)
row.number <- sample(1:nrow(car_sqrt), 0.8*nrow(car_sqrt))
# Subset our data
train <- car_sqrt[row.number,] # 80%
test <- car_sqrt[-row.number,] # 20%

# Estimate the linear fit with the training set
multi_lm_fit0.8 <- lm(Pris~Hästkrafter+Miltal+Modellår+Drivning, data = train)
summary(multi_lm_fit0.8)

# Predict in the test dataset
prediction0.8 <- predict(multi_lm_fit0.8, newdata = test)

err0.8 <- prediction0.8 - test$Pris

# Root mean square error
rmse <- sqrt(mean(err0.8^2))
# Mean absolute percentage error
mape <- mean(abs(err0.8/test$Pris))
# Result
c(RMSE= rmse, mape=mape, R2=summary(multi_lm_fit0.8)$r.squared)