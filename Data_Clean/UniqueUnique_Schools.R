# 安装并加载readxl包（如果未安装）
if(!require(readxl)) install.packages("readxl")
library(readxl)

# 读取Excel文件（默认读取第一个工作表）
file_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/二次清洗_仅院校及档次_各量表总分.xlsx"
data <- read_excel(file_path)

# 提取第三列（院校列）
school_column <- data[[3]]  # 用[[3]]选取第三列

# 去重，得到所有唯一院校名称
unique_schools <- unique(school_column)

# 计算每个院校出现的次数
school_counts <- table(school_column)

# 输出唯一院校
print(unique_schools)

# 输出每个院校的计数
print(school_counts)
