# 载入必要包
library(mgm)

# 模拟一个简单网络邻接矩阵（4节点）
# 邻接矩阵（绝对边权）
wadj <- matrix(c(
  0, 0.5, 0.2, 0,
  0.5, 0, 0, 0.3,
  0.2, 0, 0, 0.6,
  0, 0.3, 0.6, 0
), nrow = 4, byrow = TRUE)

# 符号矩阵（1正，-1负）
signs <- matrix(c(
  0, 1, -1, 0,
  1, 0, 0, 1,
  -1, 0, 0, -1,
  0, 1, -1, 0
), nrow = 4, byrow = TRUE)

# 分组向量，假设节点1和2为组1，3和4为组2
group_vector <- c(1, 1, 2, 2)

# 节点名称
var_names <- c("A", "B", "C", "D")

# BEI计算函数体（你给出的版本）
compute_BEI_for_network <- function(wadj, signs, group_vector, var_names = NULL) {
  wadj_signed <- wadj * signs
  p <- ncol(wadj_signed)
  
  if (is.null(var_names)) {
    var_names <- paste0("V", 1:p)
  }
  
  compute_node_BEI <- function(node_i) {
    node_group <- group_vector[node_i]
    neighbors <- setdiff(1:p, node_i)
    inter_group_neighbors <- neighbors[group_vector[neighbors] != node_group]
    sum(abs(wadj_signed[node_i, inter_group_neighbors]))
  }
  
  BEI_values <- sapply(1:p, compute_node_BEI)
  
  BEI_df <- data.frame(
    Node = var_names,
    Group = as.factor(group_vector),
    BEI = BEI_values,
    stringsAsFactors = FALSE
  )
  
  BEI_df$Node <- factor(BEI_df$Node, levels = BEI_df$Node[order(BEI_df$BEI, decreasing = TRUE)])
  return(BEI_df)
}

# 运行BEI计算
BEI_result <- compute_BEI_for_network(wadj, signs, group_vector, var_names)
print(BEI_result)
