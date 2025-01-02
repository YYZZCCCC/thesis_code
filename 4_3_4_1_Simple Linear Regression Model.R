
setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
data_frame <- read.csv("01_CleanTable.csv")

# Ensure Columns are Factors
data_frame$sex <- factor(data_frame$sex)
data_frame$sleepgroup <- factor(data_frame$sleepgroup) # Correct column name

# Linear Regression Model
model <- lm(vitmaindcheckresult ~ age + sex + sleepgroup, data = data_frame)
summary(model)

# Q-Q Plot for Residuals
qqnorm(residuals(model)) 
qqline(residuals(model))

# Tukey's Post-Hoc Test
library(multcomp)
tukey_sex <- glht(model, linfct = mcp(sex = "Tukey"))
summary(tukey_sex)

tukey_sleep <- glht(model, linfct = mcp(sleepgroup = "Tukey"))
summary(tukey_sleep)

# AIC and BIC
aic_value <- round(AIC(model), 0)
bic_value <- round(BIC(model), 0)
print(paste("AIC:", aic_value))
print(paste("BIC:", bic_value))
