library(readxl)
library(mgm)
library(qgraph)
library(ggplot2)
library(readr)
library(showtext)
library(dplyr)
library(igraph)
library(patchwork)
library(NetworkComparisonTest)
library(purrr)
library(tidyr)
library(glue)
library(glasso)

# 设置工作路径
setwd("/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/Formal_Code")
source("Supporting_Function.R")

# 添加并启用中文字体（macOS的宋体）
font_add("myfont", regular = "/System/Library/Fonts/Supplemental/Songti.ttc")
showtext_auto(enable = TRUE)

#数据准备与处理————————————————————————————————————————————————————————————————————————————————
file_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/三分院校_各总分.xlsx"
data_3 <- read_excel(file_path)

vars <- c("学校分类", "行为投入", "认知投入", "情感投入", "学习投入总分", "自我导向",
          "权力", "普遍性", "成就", "安全", "刺激", "遵从", "传统", "享乐主义",
          "善意", "支持", "干涉", "缺乏参与", "作息时间总得分", "作息类型分类",
          "设置目标", "优先级", "反馈性", "时间分配", "抑郁症状总得分", "抑郁症状程度",
          "焦虑得分", "焦虑程度分类", "心理韧性总分")

data_3 <- data_3[, vars]

# 定义分类变量
cat_vars <- c("学校分类", "作息类型分类", "抑郁症状程度", "焦虑程度分类")

# 连续变量保留原数值，分类变量转整数编码
for (v in cat_vars) {
  data_3[[v]] <- as.integer(as.factor(data_3[[v]]))  # 转成整数编码
}

# 变量类型向量
type_vec <- ifelse(vars %in% cat_vars, "c", "g")

# 变量水平数，分类变量为类别数，连续变量为1
level_vec <- sapply(data_3, function(x) {
  if (is.integer(x)) {
    length(unique(na.omit(x)))
  } else {
    1
  }
})

data_3 = as.matrix(data_3)
moderator_index = 1

#—————————————————————————————————-———正则化网络建模—————————————————————————————————
# 总数据集运行mgm模型
mgm_fit <- mgm(
  data = data_3,
  type = type_vec,
  level = level_vec,
  k = 2,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND",
  scale = TRUE,
  glmnet.control = list(maxit = 300000)
)
mgm_list_total <- list(mgm_fit)       # 把总 mgm 放进列表
data_list_total <- list(data_3)       # 把总数据放进列表

#  提取邻接矩阵与符号矩阵 
wadj <- mgm_fit$pairwise$wadj
signs <- mgm_fit$pairwise$signs  # 1 = 正向，-1 = 负向

# 整体网络绘图
par(family = "STHeiti")
# 可视化
qgraph(
  input = wadj,
  layout = "spring",
  labels = vars,
  edge.color = mgm_fit$pairwise$edgecolor,
  vsize = 6,
  esize = 6,
  label.cex = 1.2,
  title = "大学生学习习惯-心理健康网络模型")


#分条件数据集运行mgm模型——用自定义函数
Cond_mgm_res_3 <- run_mgm_per_condition(
  data_mat = data_3,        # 数据矩阵
  moderator_index = 1,      # 调节变量是第1列
  maxit = 300000,           # 最大迭代次数
  lambdaSel = "EBIC",       # 正则化选择方法
  lambdaGam = 0.25,         # 正则化调节参数
  ruleReg = "AND",          # 正则化规则
  scale = TRUE,             # 是否标准化
  type_vec = type_vec,        # 用户传入的type向量（如果没有则自动推断）
  level_vec = level_vec        # 用户传入的level向量（如果没有则自动推断）
)

# 查看每个条件下的mgm模型
cond_list_3 <- Cond_mgm_res_3$cond_list
# 查看每个条件下的子数据集
data_subsets_3 <- Cond_mgm_res_3$data_subsets


#———————————————总数据集Bootrstrap+显著边输出+显著边绘图+置信区间可视化——————————————————
# 对总数据集进行 bootstrap


boot_total <- run_bootstrap_for_conditions(
  cond_list    = mgm_list_total,
  data_subsets = data_list_total,
  nB = 50,        # bootstrap次数，可调
  maxit = 300000,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND",
  top_k = 20       # 如果想提取Top边
)

##========= 查看显著边
significant_edges_df <- boot_total$significant_edges_df
head(significant_edges_df,10)
print(significant_edges_df)

##========= 显著边网络绘图
# 提取显著边矩阵
significant_edges <- boot_total$sig_list[[1]]

#提取置信区间内显著边的边权（带符号）
sig_wadj <- wadj * signs #符号恢复
sig_wadj[!(significant_edges)] <- 0  # 非显著边权置零

p <- ncol(data_3)
# 根据置信区间确定边颜色，正边绿色，负边红色，零边灰色
edge_colors <- matrix("gray", p, p)
edge_colors[sig_wadj > 0] <- "green"
edge_colors[sig_wadj < 0] <- "red"

# qgraph可视化仅显著边网络
qgraph(
  sig_wadj,
  layout = "spring",
  labels = vars,
  edge.color = edge_colors,
  edge.label.cex = 1.2,
  title = "显著边网络图"
)

##========= 置信区间可视化
#把resample结果提出来
boot_no_md <- boot_total$bootstrap_results_list[[1]]   

# 直接调用Top20绘图函数
plot_res <- plot_topN_bootstrap_edges(
  res_obj = boot_no_md,      # 这里传 bootstrap 的 resample 对象
  mgm_fit      = mgm_fit,           # 需要 mgm 拟合对象以提取 wadj
  var_names    = vars,  # 注意这里要换成 data_3
  top_n        = 20,                # 取前20条边
  mode         = "split",           # 正负边各取一半
  main_title   = "大学生心理健康与学习投入网络"
)


#——————————————调节网络建模————————————————

mgm_fit_3 <- mgm(
  data = data_3,
  type = type_vec,
  level = level_vec,
  moderators = 1,            # 指定第一列为调节变量
  lambdaSel = "EBIC",
  lambdaGam = 0.8,
  ruleReg = "OR",
  scale = TRUE,
  glmnet.control = list(maxit = 300000)
)
wadj_3 <- mgm_fit$pairwise$wadj
signs_3 <- mgm_fit$pairwise$signs

#条件网络提取和比较（moderator是第1个变量）
cond_1_3 <- condition(mgm_fit_3, values = list("1" = 1))
cond_2_3 <- condition(mgm_fit_3, values = list("1" = 2))
cond_3_3 <- condition(mgm_fit_3, values = list("1" = 3))

# 假设条件网络对象列表
cond_list_3 <- list(cond_1_3, cond_2_3, cond_3_3)
# 调节变量不同水平标签
condition_levels_3 <- c("Level 1", "Level 2", "Level 3")

# ——————————————————————三阶交互及二阶交互效应输出——————————————————————————————

mgm_fit_3$interactions$indicator
mgm_fit_3$interactions$weightsAgg

#调用自定义函数创建拓展的边权矩阵
##拓展矩阵带调节的变量及其效应
moderation_df_3 <- make_moderation_df(mgm_fit_3, var_names = vars, moderator_index = 1)
head(moderation_df_3)

K <- 10
moderation_df_topK_3 <- moderation_df_3 %>%
  arrange(desc(abs(Weight))) %>%
  slice_head(n = K)

#自定义函数绘图：把调节变量也带进来（只显示三阶交互作用Top10显示）
New_qgraph_res_3 <- plot_moderation_qgraph(
  mgm_fit    = mgm_fit_3,
  var_names  = vars,
  moderator_index = 1,
  moderation_df   = moderation_df_3,   # 或者不传，自动从 k=3 的 interactions 抽
  top_k_mod  = 10,                  # 只绘制|调节效应|前10条
  # 自定义颜色（可选）
  pos_edge_color = "#2E7D32",        # 深绿
  neg_edge_color = "#C62828",        # 深红
  mod_edge_color = "#4E4E4E",        # 深灰
  edge_alpha     = 1,                # 完全不透明
  mod_node_color = "#1F77B4",        # 调节节点底色蓝
  triangle_label = "学校档次",
  main_title     = "学校档次调节作用"
)

#自定义函数：输出调节作用及各condition
# 调用函数
res_text_3 <- summarize_moderation_text(
  mgm_fit = mgm_fit_3,
  data_mat = data_3, 
  moderation_df = moderation_df_3,
  var_names = vars,
  moderator_index = 1,
  moderator_name  = "学校档次",
  condition_values = c(1,2,3), 
  condition_labels = condition_levels_3,
  cond_list = cond_list_3,   # 直接传入已有 cond_list
  digits = 3
)

# 查看整洁结果表
print(res_text_3$table)

#——————————————————————————————————边权差异统计检验————————————————————————————————————————————
#用的还是之前提的子数据集以及各子数据集直接mgm得到的结果

moderator_index <- 1

##  1) 一键分组 + mgm + bootstrap

boot_results_3 <- run_bootstrap_for_conditions(
  cond_list = cond_list_3,
  data_subsets = data_subsets_3,
  nB = 120,
  maxit = 300000,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND"
)

bootstrap_results_list_3 <- boot_results_3$bootstrap_results_list

# 查看分组子数据、分组mgm、分组bootstrap及显著边
str(Cond_mgm_res_3, max.level = 1)
head(boot_results_3$significant_edges_df)

vars_wo_mod <- vars[-moderator_index]

## 2) 不同 condition 的边权差异（bootstrap置信区间差）
#自定义函数，直接对三个子数据集边权的置信区间做差检验，并且展示显著边
edge_diff_df_3 <- edge_difference_test(
  bootstrap_results_list = bootstrap_results_list_3,
  cond_list              = cond_list_3,       # 每个条件下的 mgm 对象
  var_names              = vars_wo_mod,             # 注意：长度 = 分条件网络的 p
  condition_labels       = condition_levels_3,       # 如 c("Level 1","Level 2","Level 3")
  conf_level             = 0.95,
  top_k                  = NULL                     # 可选：只看 |差值| 最大的前K条
)

# 查看显著差异（差异CI不含0）
edge_diff_sig_3 <- subset(edge_diff_df_3, Significant)
edge_diff_sig_3 <- edge_diff_sig_3[order(-abs(edge_diff_sig_3$Diff_AB)), ]
head(edge_diff_sig_3, 20)

#————————————————————————3个condition合并绘图——————————————————————————

#自定义函数，实现了绘图+拼图
out_plot <- plot_conditions_topN(
  bootstrap_results_list = bootstrap_results_list_3,
  cond_list       = cond_list_3,
  var_names       = vars_wo_mod,
  condition_labels= condition_levels_3,
  top_n           = 20,
  mode            = "split",      # 正负边各取一半
  unify_y         = TRUE,
  y_limits        = c(-1,1),            # 如需固定到 [-1,1] 就写 c(-1,1)
  save_pdf        = "plot/Top20_CI_all_conditions.pdf"
)


#————————————————————————————社群分析与网络分析————————————————————————————————————

# 社群分析并绘图
group_res <- community_detection_and_plot(
  wadj = wadj,
  signs = signs,
  var_names = vars,
  output_dic = TRUE,
  return_full = TRUE
)
group_vector = group_res$group_vector

#根据社群分析的分组进行BEI计算并绘图
# 1. 不带调节变量
res1 <- compute_and_plot_BEI(
  cond_list        = mgm_list_total,
  group_vector     = group_vector,   # 长度必须等于网络节点数 p
  var_names        = vars,           # 与网络节点顺序一致
  condition_labels = "Overall",      # 单一标签
  plot_title       = "总体网络的 BEI"
)
# 2. 带调节变量
group_vector_wo  <- group_vector[-moderator_index]
res2 <- compute_and_plot_BEI(
  cond_list        = cond_list_3,
  group_vector     = group_vector_wo,
  var_names        = vars_wo_mod,
  condition_labels = condition_levels_3,
  plot_title       = "不同学校分类条件下的 BEI"
)

# 提取出连续变量列（顺便把调节变量列也剃掉了）
vars_for_NCT <- vars[which(type_vec == "g")]

NCTres <- run_nct_pairs(
  data_subsets_3, 
  vars_for_NCT,
  it = 200,
  test.centrality = TRUE,
  progressbar = TRUE
  )

NCTres$global_strength
NCTres$network_structure
NCTres$edge_differences
