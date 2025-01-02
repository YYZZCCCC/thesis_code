##Table 3. Distribution of Vitamin D Levels by Sleep Duration.(Male)

# Function to calculate Odds Ratio, 95% CI, and p-value
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

# Set working directory and read the data
setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
data <- read.csv("01_CleanTable.csv")

# Filter data to include only males
data <- subset(data, sex == "M")

# Create subsets for each sleepgroup
group_less5 <- subset(data, sleepgroup == "less5")
group_6 <- subset(data, sleepgroup == "6")
group_7 <- subset(data, sleepgroup == "7")
group_8 <- subset(data, sleepgroup == "8")
group_over9 <- subset(data, sleepgroup == "over9")

# Function to calculate stats for Abnormal and Normal groups
calculate_stats <- function(group) {
  abnormal <- subset(group, vitmaindcheckresult < 30)
  normal <- subset(group, vitmaindcheckresult >= 30)
  list(
    Abnormal_Count = nrow(abnormal),
    Normal_Count = nrow(normal),
    Abnormal_Mean = round(mean(abnormal$vitmaindcheckresult, na.rm = TRUE), 2),
    Abnormal_SD = round(sd(abnormal$vitmaindcheckresult, na.rm = TRUE), 2),
    Normal_Mean = round(mean(normal$vitmaindcheckresult, na.rm = TRUE), 2),
    Normal_SD = round(sd(normal$vitmaindcheckresult, na.rm = TRUE), 2)
  )
}

# Calculate stats for each group
stats_less5 <- calculate_stats(group_less5)
stats_6 <- calculate_stats(group_6)
stats_7 <- calculate_stats(group_7)
stats_8 <- calculate_stats(group_8)
stats_over9 <- calculate_stats(group_over9)

# Combine results into a data frame
results <- data.frame(
  Group = c("less5", "6", "7", "8", "over9"),
  Abnormal_Count = c(stats_less5$Abnormal_Count, stats_6$Abnormal_Count, stats_7$Abnormal_Count, stats_8$Abnormal_Count, stats_over9$Abnormal_Count),
  Normal_Count = c(stats_less5$Normal_Count, stats_6$Normal_Count, stats_7$Normal_Count, stats_8$Normal_Count, stats_over9$Normal_Count),
  Abnormal_Mean = c(stats_less5$Abnormal_Mean, stats_6$Abnormal_Mean, stats_7$Abnormal_Mean, stats_8$Abnormal_Mean, stats_over9$Abnormal_Mean),
  Abnormal_SD = c(stats_less5$Abnormal_SD, stats_6$Abnormal_SD, stats_7$Abnormal_SD, stats_8$Abnormal_SD, stats_over9$Abnormal_SD),
  Normal_Mean = c(stats_less5$Normal_Mean, stats_6$Normal_Mean, stats_7$Normal_Mean, stats_8$Normal_Mean, stats_over9$Normal_Mean),
  Normal_SD = c(stats_less5$Normal_SD, stats_6$Normal_SD, stats_7$Normal_SD, stats_8$Normal_SD, stats_over9$Normal_SD)
)

# Print the results in a simple way
cat("Vitamin D Levels by Sleep Duration (Males Only):\n")
for (i in 1:nrow(results)) {
  cat(sprintf(
    "Group: %s\n  Abnormal -> Count: %d, Mean: %.2f, SD: %.2f\n  Normal -> Count: %d, Mean: %.2f, SD: %.2f\n\n",
    results$Group[i],
    results$Abnormal_Count[i], results$Abnormal_Mean[i], results$Abnormal_SD[i],
    results$Normal_Count[i], results$Normal_Mean[i], results$Normal_SD[i]
  ))
}

# Extract counts from the `results` data frame
less5hr_Abnormal <- results$Abnormal_Count[results$Group == "less5"]
less5hr_Normal <- results$Normal_Count[results$Group == "less5"]

S6hr_Abnormal <- results$Abnormal_Count[results$Group == "6"]
S6hr_Normal <- results$Normal_Count[results$Group == "6"]

S7hr_Abnormal <- results$Abnormal_Count[results$Group == "7"]
S7hr_Normal <- results$Normal_Count[results$Group == "7"]

S8hr_Abnormal <- results$Abnormal_Count[results$Group == "8"]
S8hr_Normal <- results$Normal_Count[results$Group == "8"]

over9hr_Abnormal <- results$Abnormal_Count[results$Group == "over9"]
over9hr_Normal <- results$Normal_Count[results$Group == "over9"]

# Create sleep duration group vectors
less5hr <- c(less5hr_Abnormal, less5hr_Normal)
S6hr <- c(S6hr_Abnormal, S6hr_Normal)
S7hr <- c(S7hr_Abnormal, S7hr_Normal)
S8hr <- c(S8hr_Abnormal, S8hr_Normal)
over9hr <- c(over9hr_Abnormal, over9hr_Normal)

# Perform Odds Ratio calculations
OR_results <- list()
OR_results[["S6hr vs over9hr"]] <- calculate_OR_CI_pvalue(S6hr, over9hr)
OR_results[["S7hr vs over9hr"]] <- calculate_OR_CI_pvalue(S7hr, over9hr)
OR_results[["S8hr vs over9hr"]] <- calculate_OR_CI_pvalue(S8hr, over9hr)
OR_results[["less5hr vs over9hr"]] <- calculate_OR_CI_pvalue(less5hr, over9hr)

# Output Odds Ratio results
cat("\nOdds Ratio Comparisons (Males Only):\n")
for (comparison in names(OR_results)) {
  cat(comparison, "\n")
  cat("Odds Ratio:", sprintf("%.4f", OR_results[[comparison]]$OR), "\n")
  cat("95% CI:", sprintf("%.4f", OR_results[[comparison]]$CI[1]), "-", sprintf("%.4f", OR_results[[comparison]]$CI[2]), "\n")
  
  # Check and format p-value
  if (OR_results[[comparison]]$p_value < 0.0001) {
    cat("p-value:", format(OR_results[[comparison]]$p_value, scientific = TRUE), "\n\n")
  } else {
    cat("p-value:", sprintf("%.4f", OR_results[[comparison]]$p_value), "\n\n")
  }
}
