library(ggplot2)
library(multcomp)

setwd("D:/課程/@論文/【論文電子檔】/GitHub_Code")
data_frame <- read.csv("01_CleanTable.csv")

# 把 sleepgroup 設為因子，並指定 base group 為 "over9"
data_frame$sleepgroup <- factor(data_frame$sleepgroup,
                                levels = c("over9", "less5", "6", "7", "8"))

# 建立回歸模型
model_3 <- lm(vitmaindcheckresult ~ sex + age + sleepgroup + vitd_3_status, data = data_frame)
coefs <- summary(model_3)$coefficients

# 顯示回歸摘要（可在 console 中看到 p 值）
summary(model_3)

# 所有 sleep groups（用於圖示排序）
sleep_groups <- c("less5", "6", "7", "8", "over9")

# 找出 base group（在係數中沒出現的）
dummy_names <- paste0("sleepgroup", sleep_groups)
present_dummies <- rownames(coefs)[grepl("sleepgroup", rownames(coefs))]
base_group <- setdiff(sleep_groups, gsub("sleepgroup", "", present_dummies))

# 建立每個 group 的係數：base 為 0，其他從係數表讀取
coef_list <- c()
for (grp in sleep_groups) {
  term <- paste0("sleepgroup", grp)
  if (grp == base_group) {
    coef_list <- c(coef_list, paste0("0*", grp, "group"))
  } else if (term %in% rownames(coefs)) {
    est <- round(coefs[term, "Estimate"], 2)
    coef_list <- c(coef_list, paste0(est, "*", grp, "group"))
  } else {
    coef_list <- c(coef_list, paste0("NA*", grp, "group"))  # fallback
  }
}

# 組合整條方程式
intercept <- round(coefs["(Intercept)", "Estimate"], 2)
equation_full <- paste0("vitamin D = ", intercept, " + ", paste(coef_list, collapse = " + "))

# 繪圖
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = sleep_groups),  # 保持順序一致
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
  ) +
  annotate(
    "text",
    x = 3,
    y = max(data_frame$vitmaindcheckresult, na.rm = TRUE) * 0.6,
    label = equation_full,
    size = 4.5,
    color = "black",
    hjust = 0.5
  )


##########################################################################
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = c("less5", "6", "7", "8", "over9")),
  y = vitmaindcheckresult,
  color = sex
)) +
  geom_jitter(alpha = 0.6, size = 1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  labs(
    title = "Sleep Group vs Vitamin D Levels Faceted by Sex",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12)
  ) +
  facet_wrap(~sex) +  # 這一行加進來：根據性別分面
  annotate(
    "text",
    x = 3,
    y = max(data_frame$vitmaindcheckresult, na.rm = TRUE) * 0.75,
    label = paste("R² =", round(summary(model_3)$r.squared, 3)),
    size = 4,
    color = "black",
    hjust = 0.5
  )

####(性別圖1)#########################################################################
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = c("less5", "6", "7", "8", "over9")),
  y = vitmaindcheckresult,
  color = sex  # 改這一行：用 sex 來區分顏色
)) +
  geom_jitter(alpha = 0.6, size = 1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  labs(
    title = "Sleep Group vs Vitamin D Levels by Sex",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)",
    color = "Sex"  # 圖例標題也要一起改
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
####(性別圖2)#########################################################################
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = c("less5", "6", "7", "8", "over9")),
  y = vitmaindcheckresult,
  color = sex,               # 用顏色顯示性別
  shape = vitd_3_status      # 用形狀顯示 Vitamin D 狀態
)) +
  geom_jitter(alpha = 0.6, size = 1.5, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "black") +
  labs(
    title = "Sleep Group vs Vitamin D Levels by Sex and Vitamin D Status",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)",
    color = "Sex",             # 顏色圖例
    shape = "Vitamin D Status" # 形狀圖例
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
####(性別圖3)#########################################################################
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = c("less5", "6", "7", "8", "over9")),
  y = vitmaindcheckresult,
  shape = vitd_3_status,       # 用形狀表示 Vitamin D 狀態
  color = vitd_3_status        # 可選：也用顏色表示 Vitamin D 狀態
)) +
  geom_jitter(alpha = 0.6, size = 1.5, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "black") +
  labs(
    title = "Sleep Group vs Vitamin D Levels Faceted by Sex",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)",
    shape = "Vitamin D Status",
    color = "Vitamin D Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12)
  ) +
  facet_wrap(~sex) +  # 這一行會根據性別分面
  annotate(
    "text",
    x = 3,
    y = max(data_frame$vitmaindcheckresult, na.rm = TRUE) * 0.75,
    label = paste("R² =", round(summary(model_3)$r.squared, 3)),
    size = 4,
    color = "black",
    hjust = 0.5
  )

####(性別圖4)#########################################################################
data_frame$group_combo <- paste(data_frame$sleepgroup, data_frame$sex, sep = "_")

ggplot(data_frame, aes(
  x = factor(group_combo),
  y = vitmaindcheckresult,
  shape = vitd_3_status,
  color = sex
)) +
  geom_jitter(alpha = 0.6, size = 1.5, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "black") +
  labs(
    title = "Vitamin D Levels by Sleep Group and Sex",
    x = "Sleep Group + Sex",
    y = "Vitamin D Levels (ng/ml)",
    shape = "Vitamin D Status",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)  # 轉斜避免擁擠
  ) +
  annotate(
    "text",
    x = 5,
    y = max(data_frame$vitmaindcheckresult, na.rm = TRUE) * 0.75,
    label = paste("R² =", round(summary(model_3)$r.squared, 3)),
    size = 4,
    color = "black",
    hjust = 0.5
  )

####(性別圖5)#########################################################################
ggplot(data_frame, aes(
  x = factor(sleepgroup, levels = c("less5", "6", "7", "8", "over9")),
  y = vitmaindcheckresult,
  color = sex
)) +
  geom_jitter(alpha = 0.6, size = 1, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +
  labs(
    title = "Sleep Group vs Vitamin D Levels Faceted by Sex",
    x = "Sleep Group (hours)",
    y = "Vitamin D Levels (ng/ml)",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12)
  ) +
  facet_wrap(~sex) +  # 這一行加進來：根據性別分面
  annotate(
    "text",
    x = 3,
    y = max(data_frame$vitmaindcheckresult, na.rm = TRUE) * 0.75,
    label = paste("R² =", round(summary(model_3)$r.squared, 3)),
    size = 4,
    color = "black",
    hjust = 0.5
  )
####(性別圖6)#########################################################################
