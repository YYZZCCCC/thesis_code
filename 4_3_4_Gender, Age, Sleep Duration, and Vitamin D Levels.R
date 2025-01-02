library(ggplot2)
library(multcomp)

setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
data_frame <- read.csv("01_CleanTable.csv")

# Build updated regression model
model_3 <- lm(vitmaindcheckresult ~ sex + age + sleepgroup + vitd_3_status, data = data_frame)

# Output updated model summary
summary(model_3)

# Enhanced scatter plot
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = c("less5", "6", "7", "8", "over9")),
  y = vitmaindcheckresult,
  color = vitd_3_status
)) +
  geom_jitter(alpha = 0.6, size = 1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  labs(
    title = "Sleep Group vs Vitamin D Levels with Status",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)",
    color = "Vitamin D Status"
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
    label = paste("R² =", round(summary(model_3)$r.squared, 3)),
    size = 4,
    color = "black",
    hjust = 0.5
  )
