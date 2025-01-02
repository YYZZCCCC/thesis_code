##Table 2. Distribution of Vitamin D Levels by Gender.

setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
data <- read.csv("01_CleanTable.csv")
str(data)

# Create subsets for each group
M_Normal <- subset(data, sex == "M" & vitd_2_status == "Normal")
F_Normal <- subset(data, sex == "F" & vitd_2_status == "Normal")
M_Abnormal <- subset(data, sex == "M" & vitd_2_status == "Abnormal")
F_Abnormal <- subset(data, sex == "F" & vitd_2_status == "Abnormal")

# Display the counts for each group
cat("M_Normal count:", nrow(M_Normal), "\n")
cat("F_Normal count:", nrow(F_Normal), "\n")
cat("M_Abnormal count:", nrow(M_Abnormal), "\n")
cat("F_Abnormal count:", nrow(F_Abnormal), "\n")

# Calculate means
M_Normal_mean <- round(mean(M_Normal$vitmaindcheckresult, na.rm = TRUE), 2)
F_Normal_mean <- round(mean(F_Normal$vitmaindcheckresult, na.rm = TRUE), 2)
M_Abnormal_mean <- round(mean(M_Abnormal$vitmaindcheckresult, na.rm = TRUE), 2)
F_Abnormal_mean <- round(mean(F_Abnormal$vitmaindcheckresult, na.rm = TRUE), 2)

# Calculate standard deviations
M_Normal_sd <- round(sd(M_Normal$vitmaindcheckresult, na.rm = TRUE), 2)
F_Normal_sd <- round(sd(F_Normal$vitmaindcheckresult, na.rm = TRUE), 2)
M_Abnormal_sd <- round(sd(M_Abnormal$vitmaindcheckresult, na.rm = TRUE), 2)
F_Abnormal_sd <- round(sd(F_Abnormal$vitmaindcheckresult, na.rm = TRUE), 2)

# Output the results
cat("M_Abnormal mean:", M_Abnormal_mean, "SD:", M_Abnormal_sd, "\n")
cat("F_Abnormal mean:", F_Abnormal_mean, "SD:", F_Abnormal_sd, "\n")
cat("M_Normal mean:", M_Normal_mean, "SD:", M_Normal_sd, "\n")
cat("F_Normal mean:", F_Normal_mean, "SD:", F_Normal_sd, "\n")


Male <- c(nrow(M_Abnormal), nrow(M_Normal))
Female <- c(nrow(F_Abnormal), nrow(F_Normal))

# Function to calculate Odds Ratio, 95% Confidence Interval, and p-value
calculate_OR_CI_pvalue <- function(group1, group2) {
  a <- group1[1]  # Abnormal in group 1
  b <- group1[2]  # Normal in group 1
  c <- group2[1]  # Abnormal in group 2
  d <- group2[2]  # Normal in group 2
  OR <- (a * d) / (b * c)  # Odds Ratio calculation
  
  log_OR <- log(OR)  # Log of Odds Ratio
  SE_log_OR <- sqrt(1/a + 1/b + 1/c + 1/d)  # Standard Error of log(OR)
  CI_lower_log <- log_OR - 1.96 * SE_log_OR  # Lower bound of CI (log scale)
  CI_upper_log <- log_OR + 1.96 * SE_log_OR  # Upper bound of CI (log scale)
  
  CI_lower <- exp(CI_lower_log)  # Convert CI lower bound back to original scale
  CI_upper <- exp(CI_upper_log)  # Convert CI upper bound back to original scale
  
  # p-value calculation
  z <- log_OR / SE_log_OR
  p_value <- 2 * (1 - pnorm(abs(z)))
  
  return(list(OR = OR, CI = c(CI_lower, CI_upper), p_value = p_value))
}

# Compute Odds Ratio, 95% CI, and p-value using Male as the reference group
results <- calculate_OR_CI_pvalue(Female, Male)

# Output the results
cat("Female vs Male (Male as reference)\n")
cat("Odds Ratio:", sprintf("%.4f", results$OR), "\n")
cat("95% Confidence Interval:", sprintf("%.4f", results$CI[1]), "-", sprintf("%.4f", results$CI[2]), "\n")
if (results$p_value < 0.0001) {
  cat("p-value:", format(results$p_value, scientific = TRUE), "\n")
} else {
  cat("p-value:", sprintf("%.4f", results$p_value), "\n")
}
