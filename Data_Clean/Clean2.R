library(readxl)
library(writexl)

file_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/清洗数据_仅院校及档次_各量表总分.xlsx"
save_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/二次清洗_仅院校及档次_各量表总分.xlsx"

# 读取第一轮清洗后的数据
data <- read_excel(file_path)

# 提取院校列
school_col <- data[[3]]

# 定义需要排除的虚构或二级学院名称列表
exclude_names <- c(
  "思慕大学",
  "皇家学院",
  "五道口职业技术学院",
  "大数据与人工智能学院",
  "经济管理学院",
  "医药工程学院",
  "计算机学院",
  "人文教育学院",
  "财政与公共管理学院"
)

# 构建排除条件，匹配排除列表中任一内容的行
exclude_rows <- school_col %in% exclude_names

# 保留非排除行
cleaned_data <- data[!exclude_rows, ]

# 保存结果
write_xlsx(cleaned_data, save_path)

cat("第二轮清洗完成，剔除虚构及二级学院，保存路径：", save_path, "\n")
