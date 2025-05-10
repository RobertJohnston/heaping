# Data quality analysis 

# Analysis of heaping in height/weight/age in months in anthro surveys

# Clear environment
rm(list = ls())

# Load libraries
library(readxl)
library(readr)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(mgcv)

df <- read_excel("C:/Users/rojohnston/Downloads/png2009_hgt_wgt.xlsx")

View(df)

df$height <- round(df$height, 1)
df$weight <- round(df$weight, 1)
df$agemons <- round(df$agemons, 1)

# Display Height 
ggplot(df, aes(x = height)) +
  geom_bar() +
  labs(title = "Height Distribution (Counts)",
       x = "Height",
       y = "Count") +
  theme_minimal()

# Display Weight  
ggplot(df, aes(x = weight)) +
  geom_bar() +
  labs(title = "Height Distribution (Counts)",
       x = "Height",
       y = "Count") +
  theme_minimal()

# Display Age in months (1 digit)  
ggplot(df, aes(x = agemons)) +
  geom_bar() +
  labs(title = "Age Distribution (Counts)",
       x = "Age in Months",
       y = "Count") +
  theme_minimal()

# GLM Poisson and Negative Binomial do not fit well 
# Polynomial deg 3 does not fit well 

# Fit a polynomial regression (degree 3)
df_clean <- df_count %>%
  filter(!is.na(height) & !is.na(n))

# Fit polynomial model (degree 3)
model <- lm(n ~ poly(height, 3), data = df_clean)

df_pred <- df_clean %>%
  mutate(predicted = predict(model, newdata = .),
         predicted = pmax(predicted, 0))  # set to zero

ggplot(df_pred, aes(x = height, y = n)) +
  geom_point(size = 1) +
  geom_line(aes(y = predicted), color = "purple", size = 1) +
  labs(title = "Polynomial Regression Fit (Clipped at 0)",
       x = "Height",
       y = "Count") +
  theme_minimal()

r2 <- summary(model)$r.squared
print(paste("R-squared:", round(r2, 3))) 


# GAM Poisson fits better
# Height
df_count <- df %>% count(height)

df_clean <- df_count %>% filter(!is.na(height) & !is.na(n))

model_gam <- gam(n ~ s(height), data = df_clean, family = poisson)

df_pred <- df_clean %>%
  mutate(predicted = predict(model_gam, newdata = ., type = "response"),
         predicted = pmax(predicted, 0))

ggplot(df_pred, aes(x = height, y = n)) +
  geom_point(size = 1) +
  geom_line(aes(y = predicted), color = "darkred", size = 1) +
  labs(title = "GAM Fit (Poisson, Nonlinear Smoothing)",
       x = "Height",
       y = "Count") +
  theme_minimal()

r2_gam <- summary(model_gam)$dev.expl
print(paste("Pseudo R-squared (Deviance explained):", round(r2_gam, 3)))

# WEIGHT
df_count <- df %>% count(weight)

df_clean <- df_count %>% filter(!is.na(weight) & !is.na(n))

model_gam <- gam(n ~ s(weight), data = df_clean, family = poisson)

df_pred <- df_clean %>%
  mutate(predicted = predict(model_gam, newdata = ., type = "response"),
         predicted = pmax(predicted, 0))

ggplot(df_pred, aes(x = weight, y = n)) +
  geom_point(size = 1) +
  geom_line(aes(y = predicted), color = "purple", size = 1) +
  labs(title = "GAM Fit (Poisson, Nonlinear Smoothing)",
       x = "Weight",
       y = "Count") +
  theme_minimal()

r2_gam <- summary(model_gam)$dev.expl
print(paste("Pseudo R-squared (Deviance explained):", round(r2_gam, 3)))

# Agemons
df_count <- df %>% count(agemons)

df_clean <- df_count %>% filter(!is.na(agemons) & !is.na(n))

model_gam <- gam(n ~ s(agemons), data = df_clean, family = poisson)

df_pred <- df_clean %>%
  mutate(predicted = predict(model_gam, newdata = ., type = "response"),
         predicted = pmax(predicted, 0))

ggplot(df_pred, aes(x = agemons, y = n)) +
  geom_point(size = 1) +
  geom_line(aes(y = predicted), color = "yellow", size = 1) +
  labs(title = "GAM Fit (Poisson, Nonlinear Smoothing)",
       x = "Age in months",
       y = "Count") +
  theme_minimal()

r2_gam <- summary(model_gam)$dev.expl
print(paste("Pseudo R-squared (Deviance explained):", round(r2_gam, 3)))
# Here the fit is good, but R2 is very low. This analysis does not work well. 