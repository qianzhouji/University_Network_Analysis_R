library(readxl)
library(writexl)

file_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/二次清洗_仅院校及档次_各量表总分.xlsx"
save_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/带院校分类_各量表总分.xlsx"

# 读取数据
data <- read_excel(file_path)

# 提取第三列（院校列）
school_col <- data[[3]]

# 定义分类列表
category_1 <- c("哈尔滨工业大学", "清华大学", "上海交通大学", "某985大学")
category_2 <- c("安徽工程大学", "安徽师范大学", "宿州学院", "皖南医学院", "安徽工业大学", 
                "安徽理工大学", "安徽财经大学", "河北科技大学", "商洛学院")
category_3 <- c("芜湖学院", "阜阳理工学院", "安徽信息工程学院", "陕西科技大学镐京学院")

# 初始化分类向量，默认为NA
category_vec <- rep(NA, length(school_col))

# 赋值分类
category_vec[school_col %in% category_1] <- 1
category_vec[school_col %in% category_2] <- 2
category_vec[school_col %in% category_3] <- 3

# 将分类向量加入数据框D列
data[[4]] <- category_vec

# 保存新文件
write_xlsx(data, save_path)

cat("院校分类完成，结果保存到：", save_path, "\n")
