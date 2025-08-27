library(readxl)
library(writexl)  
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
library(bruceR)
library(lme4)
library(emmeans)
#支持函数
source("/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/Formal_Code/Supporting_Function.R")

# 添加并启用中文字体（macOS的宋体）
font_add("myfont", regular = "/System/Library/Fonts/Supplemental/Songti.ttc")
showtext_auto(enable = TRUE)


#——————————————————————————————————————数据准备与处理————————————————————————————————————————————————————————————————————————————————
# 设置工作路径
setwd("/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/专业分类")
if (!dir.exists("plot")) {
  dir.create("plot", recursive = TRUE)
}
if (!dir.exists("xls")) {
  dir.create("xls", recursive = TRUE)
}
if (!dir.exists("Rdata")) {
  dir.create("Rdata", recursive = TRUE)
}
file_path <- "/Users/yuke/Desktop/8_1_大学层次学习习惯和心理健康/专业分类_各总分.xlsx"
clean_xlsx(file_path)

moderator_index = 1
moderator_name = "专业分类"

data <- read_excel(file_path)

vars <- c("调节变量", "行为投入", "认知投入", "情感投入", "学习投入总分", "自我导向",
          "权力", "普遍性", "成就", "安全", "刺激", "遵从", "传统", "享乐主义",
          "善意", "支持", "干涉", "缺乏参与", "作息时间总得分", "作息类型分类",
          "设置目标", "优先级", "反馈性", "时间分配", "抑郁症状总得分", "抑郁症状程度",
          "焦虑得分", "焦虑程度分类", "心理韧性总分")
vars[moderator_index] <- moderator_name
data <- data[, vars]

# 定义分类变量
cat_vars <- c("调节变量", "作息类型分类", "抑郁症状程度", "焦虑程度分类")
cat_vars[moderator_index] <- moderator_name
# 连续变量保留原数值，分类变量转整数编码
for (v in cat_vars) {
  data[[v]] <- as.integer(as.factor(data[[v]]))  # 转成整数编码
}
# 变量类型向量
type_vec <- ifelse(vars %in% cat_vars, "c", "g")
# 变量水平数，分类变量为类别数，连续变量为1
level_vec <- sapply(data, function(x) {
  if (is.integer(x)) {
    length(unique(na.omit(x)))
  } else {
    1
  }
})

data = as.matrix(data)                                #随着数据类型转换为矩阵，总数据表处理完毕

#通过自定义函数进行数据集拆分，按调节变量的条件
split_res <- split_by_moderator(data, 1, vars)

data_subsets <- split_res$data_subsets                #子数据集列表
moderator_values <- split_res$moderator_values        #调节变量各水平
condition_levels <- as.factor(moderator_values)       #调节变量水平因子型向量，有些函数会用到

type_vec_NoMod <- type_vec[-moderator_index]          #无调节变量的类型向量，用于分水平mgm
level_vec_NoMod <- level_vec[-moderator_index]        #同上，无调节变量的各变量水平向量


Basics <- data_frame(
  vars_num = 1:29,
  vars,
  type_vec,
  level_vec
)
print(Basics,n = 29)
export(Basics, file = "./xls/Basics.xlsx")
cat ("变量名、变量类型、变量水平基础信息已存储为./xls/Basics.xlsx")

#—————————————————————————————————-———正则化网络建模—————————————————————————————————
# 总数据集运行mgm模型
mgm_fit <- mgm(
  data = data,
  type = type_vec,
  level = level_vec,
  k = 2,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND",
  scale = TRUE,
  glmnet.control = list(maxit = 300000),
  pbar = FALSE
)
export(mgm_fit, file = "./Rdata/All_mgm_fit.Rdata")
cat("总数据集mgm原始数据已存储为./Rdata/All_mgm_fit.Rdata")

mgm_list_total <- list(mgm_fit)       # 把总 mgm 放进列表，为了调用后面的自定义bootstrap函数
data_list_total <- list(data)         # 把总数据放进列表

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
  edge.labels = TRUE,
  edge.color = mgm_fit$pairwise$edgecolor,
  title = "大学生学习习惯-心理健康网络模型"
)

pdf(file.path("plot", "All_network.pdf"))
qgraph(input = wadj,layout = "spring",labels = vars,edge.labels = TRUE,edge.color = mgm_fit$pairwise$edgecolor,title = "大学生学习习惯-心理健康网络模型")
dev.off()
#虽然把图存下来了但是连续调用两遍qgraph真的很狼狈
cat ("总网络图已存储为./plot/All_network")


#———————————————总数据集Bootrstrap+显著边输出+显著边绘图+置信区间可视化——————————————————
# 对总数据集进行 bootstrap

boot_total <- run_bootstrap_for_conditions(
  cond_list    = mgm_list_total,           #这里传之前存的1维表，因为我这个函数接收的是list
  data_subsets = data_list_total,          #同
  nB = 20,        # bootstrap次数，可调
  maxit = 300000,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND",
  pbar = FALSE
)

#数据存储，bruce老师的包真好用
export(boot_total$significant_edges_df, file = "./xls/All_significant_edges.xlsx")
cat("总数据集的显著边表格已存储在./xls/All_significant_edges.xlsx")
export(boot_total$sig_list, file = "./xls/All_sig_list.xlsx")
cat("总数据集的显著边矩阵已存储在./xls/All_sig_list.xlsx")
export(boot_total$bootstrap_results_list, file = "./Rdata/All_boot_res.RData")
cat("总数据集的bootstrap原始结果已存储在./Rdata/All_boot_res.RData")


##========= 查看显著边
significant_edges_df <- boot_total$significant_edges_df
head(significant_edges_df,20)
print(significant_edges_df)

##========= 显著边网络绘图
# 提取显著边矩阵
significant_edges <- boot_total$sig_list[[1]]

#提取置信区间内显著边的边权（带符号）
sig_wadj <- wadj * signs #符号恢复
sig_wadj[!(significant_edges)] <- 0  # 非显著边权置零

p <- ncol(data)

# qgraph可视化仅显著边网络
qgraph(
  sig_wadj,
  layout = "spring",
  labels = vars,
  edge.labels = TRUE,
  edge.color = mgm_fit$pairwise$edgecolor,
  title = "显著边网络图"
)
pdf(file.path("plot", "All_sig_network.pdf"))
qgraph(sig_wadj,layout = "spring",labels = vars,edge.labels = TRUE,edge.color = mgm_fit$pairwise$edgecolor,title = "显著边网络图")
dev.off()
cat ("显著边网络已存储为./plot/All_sig_network.pdf")


##========= 置信区间可视化
#把resample结果提出来
boot_no_md <- boot_total$bootstrap_results_list[[1]]   

# 直接调用绘图函数
plot_res <- plot_topN_bootstrap_edges(
  res_obj = boot_no_md,      # 这里传 bootstrap 的 resample 对象
  mgm_fit      = mgm_fit,           # 需要 mgm 拟合对象以提取 wadj
  var_names    = vars,  # 注意这里要换成 data_3
  top_n        = 20,                # 取前20条边
  mode         = "split",           # 正负边各取一半
  main_title   = "大学生心理健康与学习投入网络",
  save_prefix = "./plot/All_top20"
)
cat("Top20显著边置信区间图已存储为./plot/All_top20_bootstrap_CI_plot.pdf")

#——————————————调节网络建模————————————————

mgm_fit_md <- mgm(
  data = data,
  type = type_vec,
  level = level_vec,
  moderators = 1,            # 指定第一列为调节变量
  lambdaSel = "EBIC",
  lambdaGam = 0.8,
  ruleReg = "OR",
  scale = TRUE,
  glmnet.control = list(maxit = 300000),
  pbar = TRUE
)
wadj_md <- mgm_fit$pairwise$wadj
signs_md <- mgm_fit$pairwise$signs

# ——————————————————————三阶交互及二阶交互效应输出——————————————————————————————

mgm_fit_md$interactions$indicator
mgm_fit_md$interactions$weightsAgg

#调用自定义函数创建拓展的边权矩阵
##拓展矩阵带调节的变量及其效应
moderation_df <- make_moderation_df(mgm_fit_md, var_names = vars, moderator_index = 1)
head(moderation_df)

K <- 10
moderation_df_topK <- moderation_df %>%
  arrange(desc(abs(Weight))) %>%
  slice_head(n = K)

#自定义函数绘图：把调节变量也带进来（只显示三阶交互作用Top10显示）
New_qgraph_res <- plot_moderation_qgraph(
  mgm_fit    = mgm_fit_md,
  var_names  = vars,
  moderator_index = 1,
  moderation_df   = moderation_df,   # 或者不传，自动从 k=3 的 interactions 抽
  top_k_mod  = 10,                  # 只绘制|调节效应|前10条
  # 自定义颜色（可选）
  pos_edge_color = "#2E7D32",        # 深绿
  neg_edge_color = "#C62828",        # 深红
  mod_edge_color = "#4E4E4E",        # 深灰
  edge_alpha     = 1,                # 完全不透明
  mod_node_color = "#1F77B4",        # 调节节点底色蓝
  triangle_label = moderator_name,
  main_title     = "学校档次调节作用"
)
cat("带调节变量的网络图已存储为./plot/network_with_mod.pdf")

#已遗弃！
#自定义函数：输出调节作用及各condition
# 调用函数
#res_text <- summarize_moderation_text(
#mgm_fit = mgm_fit_md,
#data_mat = data, 
#moderation_df = moderation_df,
#var_names = vars,
#moderator_index = 1,
#moderator_name  = "学校档次",
#condition_values = c(1,2,3),
#condition_labels = condition_levels,
#cond_list = cond_list,   # 直接传入已有 cond_list
#digits = 3
#)
# 查看整洁结果表
#print(res_text_3$table)

#——————————————————————————————————边权差异统计检验————————————————————————————————————————————
##一键拆分数据集+mgm

#通过自定义函数进行数据集拆分，按调节变量的条件
split_res <- split_by_moderator(data, moderator_index, vars)

data_subsets <- split_res$data_subsets
moderator_values <- split_res$moderator_values
condition_levels <- as.factor(moderator_values)

type_vec_NoMod <- type_vec[-moderator_index]
level_vec_NoMod <- level_vec[-moderator_index]

#先处理一下数据集，避免bootstrap中因某分类变量个别水平数过少导致无法采样到而报错
#去掉调节变量的所有分类型变量的向量
CatVar_NoMod = cat_vars[-moderator_index]
#调用自定义函数，简单过采样（简单复制）
data_subsets <- oversample_rare_levels_list(data_subsets, CatVar_NoMod, 666)
export(data_subsets, file = "./Rdata/data_subsets.Rdata")
cat("过采样（简单复制）后的分条件子数据集列表已存储为./Rdata/data_subsets.Rdata")

#分条件数据集运行mgm模型——用自定义函数
cond_list <- run_mgm_for_conditions(
  data_subsets = data_subsets,        # 数据矩阵
  maxit = 300000,           # 最大迭代次数
  lambdaSel = "EBIC",       # 正则化选择方法
  lambdaGam = 0.25,         # 正则化调节参数
  ruleReg = "AND",          # 正则化规则
  scale = TRUE,             # 是否标准化
  type_vec = type_vec_NoMod,        # 用户传入的type向量（如果没有则自动推断）
  pbar = FALSE
)

export(cond_list, file = "./Rdata/cond_list.Rdata")
cat("分条件进行的mgm数据列表已存储为./Rdata/cond_list.Rdata")

##一键bootstrap

boot_results <- run_bootstrap_for_conditions(
  cond_list = cond_list,
  data_subsets = data_subsets,
  nB = 10,
  maxit = 300000,
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND",
  pbar = TRUE
)

export(boot_results$significant_edges_df, file = "./xls/Conditioned_significant_edges.xlsx")
cat("显著边表格（带调节变量条件条件）已存储在./xls/Conditioned_significant_edges.xlsx")
export(boot_results$sig_list, file = "./xls/Conditioned_sig_list.xlsx")
cat("调节变量不同条件下的显著边矩阵已存储在./xls/Conditioned_sig_list.xlsx")
export(boot_results$bootstrap_results_list, file = "./Rdata/Conditioned_boot_res.RData")
cat("调节变量不同条件下子数据集的bootstrap原始结果已存储在./Rdata/Conditioned_boot_res.RData")

bootstrap_results_list <- boot_results$bootstrap_results_list

# 查看分组bootstrap及显著边
head(boot_results$significant_edges_df)

vars_wo_mod <- vars[-moderator_index]

## 2) 不同 condition 的边权差异（bootstrap置信区间差）
#自定义函数，直接对三个子数据集边权的置信区间做差检验，并且展示显著边
edge_diff_df <- edge_difference_test(
  bootstrap_results_list = bootstrap_results_list,
  cond_list              = cond_list,       # 每个条件下的 mgm 对象
  var_names              = vars_wo_mod,             # 注意：长度 = 分条件网络的 p
  condition_labels       = condition_levels,       # 如 c("Level 1","Level 2","Level 3")
  conf_level             = 0.95,
  top_k                  = NULL                     # 可选：只看 |差值| 最大的前K条
)
head(edge_diff_df, 20)
export(edge_diff_df, file = "./xls/edge_diff_df.xlsx")
cat("不同条件下边权差异显著性及信息表已存储在./xls/edge_diff_df.xlsx")

edge_diff_sig <- subset(edge_diff_df, Significant)
edge_diff_sig <- edge_diff_sig[order(-abs(edge_diff_sig$Diff_AB)), ]
head(edge_diff_sig, 20)
export(edge_diff_sig, file = "./xls/edge_diff_sig.xlsx")
cat("不同条件下差异显著的边权信息表（排序）已存储在./xls/edge_diff_sig.xlsx")


#3个condition合并绘图

#自定义函数，实现了绘图+拼图
out_plot <- plot_conditions_topN(
  bootstrap_results_list = bootstrap_results_list,
  cond_list       = cond_list,
  var_names       = vars_wo_mod,
  condition_labels= condition_levels,
  top_n           = 20,
  mode            = "split",      # 正负边各取一半
  unify_y         = TRUE,
  y_limits        = c(-1,1),            # 如需固定到 [-1,1] 就写 c(-1,1)
  save_pdf        = "plot/Top20_CI_all_conditions.pdf"
)
cat("多条件下CI对比图已存储为./plot/Top20_CI_all_conditions.pdf")


#————————————————————————————社群分析与网络分析————————————————————————————————————

# 社群分析并绘图
group_res <- community_detection_and_plot(
  wadj = wadj,
  signs = signs,
  var_names = vars,
  output_dic = TRUE,
  return_full = TRUE,
  save_path = "./plot/community_network.png"
)
group_vector = group_res$group_vector

GP = as.factor(group_vector)
Groups <- split(vars, GP)
print(Groups)#输出具体社群情况


#根据社群分析的分组进行BEI计算并绘图
# 1. 不带调节变量
res1 <- compute_and_plot_BEI(
  cond_list        = mgm_list_total,
  group_vector     = group_vector,   # 长度必须等于网络节点数 p
  var_names        = vars,           # 与网络节点顺序一致
  condition_labels = "Overall",      # 单一标签
  plot_title       = "总体网络的 BEI",
  save_name = "BEI_for_all.pdf"
)
BEI_for_all = res1$BEI_data
export(BEI_for_all, file = "./xls/BEI_for_all.xlsx")
cat("总BEI已存储为./xls/BEI_for_all.xlsx")
# 2. 带调节变量
group_vector_wo  <- group_vector[-moderator_index]
res2 <- compute_and_plot_BEI(
  cond_list        = cond_list,
  group_vector     = group_vector_wo,
  var_names        = vars_wo_mod,
  condition_labels = condition_levels,
  plot_title       = "不同学校分类条件下的 BEI",
  save_name = "BEI_in_conditions.pdf"
)
BEI_for_conditions = res2$BEI_data
export(BEI_for_conditions, file = "./xls/BEI_for_conditions.xlsx")
cat("分条件BEI已存储为./xls/BEI_for_conditions.xlsx")


# 提取出连续变量列（顺便把调节变量列也剃掉了）
vars_for_NCT <- vars[which(type_vec == "g")]

NCTres <- run_nct_pairs(
  data_subsets, 
  vars_for_NCT,
  it = 200,
  test.centrality = TRUE,
  progressbar = TRUE
)

global_strength <- NCTres$global_strength
export(global_strength, file = "./xls/global_strength.xlsx") 
cat("网络强度差异表已存储为./xls/global_strength.xlsx")
print(global_strength)
network_structure <- NCTres$network_structure
export(network_structure, file = "./xls/network_structure.xlsx") 
cat("网络结构差异表已存储为./xls/network_structure.xlsx")
print(network_structure)
edge_differences <- NCTres$edge_differences
export(edge_differences, file = "./xls/edge_differences_for_nct.xlsx") 
cat("nct计算的各边权差异表已存储为./xls/edge_differences_for_nct.xlsx")
cat("这个太长了就不输出了")
nct_results <- NCTres$nct_results
export(nct_results, file = "./xls/nct_results.Rdata") 
cat("nct原始结果已存储为./xls/nct_results.Rdata")

plot_nct_pairs(nct_results, edge_differences, vars_for_NCT, "plot")
