# 载入所需的包
library(ggplot2)
library(dplyr)
library(car) # 用于方差齐性检验
library(readr) # 用于读取csv文件
library(broom) # 用于整洁格式化的统计结果
library(tidyverse) # 用于数据操作

# 读取数据
data <- read_csv('user_feedback.csv')

# 计算均值
mean_values <- data %>%
  group_by(type) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

print("平均值：")
print(mean_values)

# 保存均值结果
write_csv(mean_values, 'mean_values.csv')

# 绘制箱线图
g <- ggplot(data, aes(x = type, y = satisfaction)) +
  geom_boxplot() +
  ggtitle('Satisfaction by Type') +
  theme_minimal()
print(g)

# 保存箱线图
ggsave('boxplot_satisfaction.png', plot = g)

# 方差分析（ANOVA）及样本t检验
anova_results <- list()
t_test_results <- list()

# 方差分析
for (column in c('satisfaction', 'ease_of_use', 'comprehension', 'task_time', 'cognitive_load')) {
  formula <- as.formula(paste(column, "~ type"))
  anova_model <- aov(formula, data = data)
  anova_result <- tidy(anova_model)
  anova_results[[column]] <- anova_result
  
  print(paste("方差分析结果 -", column, ":"))
  print(anova_result)
  
  # 保存方差分析结果
  write_csv(anova_result, paste0('anova_', column, '.csv'))
}

# 样本t检验
types <- unique(data$type)
for (i in 1:(length(types)-1)) {
  for (j in (i+1):length(types)) {
    for (column in c('satisfaction', 'ease_of_use', 'comprehension', 'task_time', 'cognitive_load')) {
      group1 <- filter(data, type == types[i])[[column]]
      group2 <- filter(data, type == types[j])[[column]]
      t_test <- t.test(group1, group2)
      test_result <- tidy(t_test)
      t_test_results[[paste(types[i], 'vs', types[j], '-', column)]] <- test_result
      
      print(paste("样本t检验结果 -", types[i], "vs", types[j], "-", column, ":"))
      print(test_result)
      
      # 保存t检验结果
      write_csv(test_result, paste0('t_test_', types[i], '_vs_', types[j], '_', column, '.csv'))
    }
  }
}

# 保存所有t检验结果到一个文件中
all_t_test_results <- bind_rows(t_test_results)
write_csv(all_t_test_results, 't_test_results.csv')