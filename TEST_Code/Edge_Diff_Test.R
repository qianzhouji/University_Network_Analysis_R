dim(dataGD)
# 运行 MGM 模型
mgm_fit <- mgm(
  data = dataGD,
  type = c("g", "g", "c", "g", "c", "g", "c"),
  level = c(1, 1, 2, 1, 3, 1, 3),
  moderators = 7,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND"
)

#————————————————————————————————————————Debug Part————————————————————————————————————————————————————
# 分条件mgm
Cond_mgm_res <- run_mgm_per_condition(
  data_mat = dataGD,        # 数据矩阵
  moderator_index = 7,      # 调节变量是第7列
  maxit = 300000,           # 最大迭代次数
  lambdaSel = "EBIC",       # 正则化选择方法
  lambdaGam = 0.25,         # 正则化调节参数
  ruleReg = "AND",          # 正则化规则
  scale = TRUE,             # 是否标准化
  type_vec = c("g", "g", "c", "g", "c", "g", "c"),        # 用户传入的type向量（如果没有则自动推断）
  level_vec = c(1, 1, 2, 1, 3, 1, 3)        # 用户传入的level向量（如果没有则自动推断）
)

# 查看每个条件下的mgm模型
cond_list <- Cond_mgm_res$cond_list

# 查看每个条件下的子数据集
data_subsets <- Cond_mgm_res$data_subsets

# 分条件bootstrap并存储显著边
results <- run_bootstrap_for_conditions(
  cond_list = cond_list,
  data_subsets = data_subsets,
  nB = 100,
  maxit = 300000,
  lambdaSel = "EBIC",
  lambdaGam = 0.8,
  ruleReg = "OR",
  top_k = 10
)

# 查看显著边结果
print(results$significant_edges_df)

#————————————————————————————————————————————————————————————————————————————————————————

# 一步完成分条件 mgm + bootstrap + 显著边输出
results_all <- run_mgm_and_bootstrap(
  data_mat    = dataGD,                                  # 数据矩阵
  moderator_index = 7,                                   # 调节变量是第7列
  maxit       = 300000,                                  # 最大迭代次数
  lambdaSel   = "EBIC",                                  # 正则化选择方法
  lambdaGam   = 0.25,                                    # 正则化调节参数（mgm）
  ruleReg     = "AND",                                   # 正则化规则（mgm）
  scale       = TRUE,                                    # 是否标准化
  type_vec    = c("g", "g", "c", "g", "c", "g", "c"),    # type 向量
  level_vec   = c(1, 1, 2, 1, 3, 1, 3),                  # level 向量
  nB          = 100                                      # bootstrap 次数
)

# 查看显著边结果
print(results_all$significant_edges_df)



# 调用边权差异统计函数
var_names_sub <- colnames(results_all$data_subsets[[1]])  # 去掉了调节变量后的变量名

edge_diff_df <- edge_difference_test(
  bootstrap_results_list = results_all$bootstrap_results_list,
  cond_list             = results_all$cond_list,
  var_names             = var_names_sub,
  condition_labels      = c("Level 1","Level 2","Level 3"),
  conf_level            = 0.95,
  top_k                 = NULL,   # 或者 50
  signed                = FALSE   # 比较“强度差异”；要方向差异就改 TRUE
)

# 查看显著差异的结果（CI不包含0的边）
subset(edge_diff_df, Significant)
