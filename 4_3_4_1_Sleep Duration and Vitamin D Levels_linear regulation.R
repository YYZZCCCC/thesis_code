library(ggplot2)
library(multcomp)

setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
data_frame <- read.csv("01_CleanTable.csv")

# Ensure sleepgroup is a factor type and set the correct display order
data_frame$sleepgroup <- factor(data_frame$sleepgroup, 
                                levels = c("less5", "6", "7", "8", "over9"))

# Set the reference group to "over9" (this affects regression analysis)
data_frame$sleepgroup <- relevel(data_frame$sleepgroup, ref = "over9")

# Verify factor levels order
#print(levels(data_frame$sleepgroup))

# Build linear regression model
model <- lm(vitmaindcheckresult ~ sleepgroup, data = data_frame)

# Output model summary
summary(model)

# Tukey post hoc test
tukey_sleepgroup <- glht(model, linfct = mcp(sleepgroup = "Tukey"))

# Test results
summary(tukey_sleepgroup)

# Extract regression model coefficients and R²
coef_summary <- summary(model)
formula_text <- paste0(
  "Vitamin D Levels = ", round(coef(model)[1], 2),
  paste(sapply(2:length(coef(model)), function(i) {
    paste0(" + ", round(coef(model)[i], 2), "*", names(coef(model))[i])
  }), collapse = "")
)
r_squared <- paste0("R² = ", round(coef_summary$r.squared, 3))

# Residual QQ Plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red", lwd = 2)

# Create scatter plot with regression formula and force X-axis order
ggplot(data_frame, aes(x = factor(sleepgroup, 
                                  levels = c("less5", "6", "7", "8", "over9")), 
                       y = vitmaindcheckresult)) +
  geom_jitter(alpha = 0.6, size = 1, width = 0.2) + 
  stat_summary(fun = mean, geom = "point", color = "blue", size = 3) + 
  labs(
    title = "Scatter plot of Sleep Group vs Vitamin D Levels",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.title = element_text(size = 12)              
  ) +
  annotate(
    "text",
    x = 3, 
    y = max(data_frame$vitmaindcheckresult, na.rm = TRUE) * 0.75, 
    label = paste(formula_text, "\n", r_squared),
    size = 4,
    color = "black",
    hjust = 0.5)

################################################################################################
# Retain groups less5, 6, over9 => Model performance does not improve
filtered_data <- data_frame[data_frame$sleepgroup %in% c("less5", "6", "over9"), ]
model_filtered <- lm(vitmaindcheckresult ~ sleepgroup, data = filtered_data)
summary(model_filtered)  # Multiple R-squared:  0.006413 (Original: 0.007501)

################################################################################################
# Retain groups less5, 6, 8, over9 => Model performance improves
filtered_data <- data_frame[data_frame$sleepgroup %in% c("less5", "6", "8", "over9"), ]
model_filtered <- lm(vitmaindcheckresult ~ sleepgroup, data = filtered_data)
summary(model_filtered)  # Multiple R-squared:  0.01004 (Original: 0.007501)
