library(readxl)
library(writexl)

file_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/数据_人口统计变量_各量表总分.xlsx"
save_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/清洗数据_人口统计变量_各量表总分.xlsx"

# 读取数据
data <- read_excel(file_path)

# 提取第三列（院校列）
school_col <- data[[3]]

# 将NA替换为“某985大学”
school_col[is.na(school_col)] <- "某985大学"

# 把替换后的列赋值回数据框
data[[3]] <- school_col

# 保留只含“学院”或“大学”的行
valid_rows <- grepl("学院|大学", school_col)

# 筛选数据
cleaned_data <- data[valid_rows, ]

# 保存为新xlsx文件
write_xlsx(cleaned_data, save_path)

cat("清洗完成，NA替换为‘某985大学’，保留含‘学院’或‘大学’的行，文件保存至：", save_path, "\n")

