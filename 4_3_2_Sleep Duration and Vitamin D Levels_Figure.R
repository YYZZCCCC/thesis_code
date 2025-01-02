## scatter plot about the relationship between Sleep Duration and Vit D Level 

setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
DF <- read.csv("01_CleanTable.csv")
str(DF)

library(ggplot2)

# Fit the linear model
lm_model <- lm(vitmaindcheckresult ~ sleeptimecheckresult, data = DF)

# Extract the equation and R-squared value
slope <- round(coef(lm_model)[2], 4)
intercept <- round(coef(lm_model)[1], 4)
r_squared <- round(summary(lm_model)$r.squared, 4)
equation <- paste0("Vit D levels = ",
                   slope, " * Sleep Duration + ",
                   intercept, "\nR² = ", r_squared)

# Create the scatter plot
ggplot(DF, aes(x = sleeptimecheckresult, y = vitmaindcheckresult)) +
  geom_point(color = "black", alpha = 0.6) +  
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) + 
  annotate("text", x = max(DF$sleeptimecheckresult) * 0.8, 
           y = max(DF$vitmaindcheckresult) * 0.9,
           label = equation, color = "red", size = 5, hjust = 0.5) +
  scale_x_continuous(breaks = seq(floor(min(DF$sleeptimecheckresult)),
                                  ceiling(max(DF$sleeptimecheckresult)),
                                  by = 1)) + 
  labs(x = "Sleep Duration", y = "Vit D Level", 
       title = "Correlation between Sleep Duration and Vit D Level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  