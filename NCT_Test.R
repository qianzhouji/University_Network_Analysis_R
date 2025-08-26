# 假设已有一个包含多个子数据集的列表 data_subsets
n_sets <- length(data_subsets_3)

# 仅选择连续型变量（根据你的变量名自行调整）
vars_for_NCT <- vars[which(type_vec == "g")]

# 对每个子数据集提取并标准化这些变量
scaled_subsets <- lapply(data_subsets, function(dat) {
  scale(dat[, vars_for_NCT])
})

# 计算所有条件对（组合）
pairs_idx <- combn(n_sets, 2)

# 存放所有 NCT 结果
nct_results <- list()

for (i in seq_len(ncol(pairs_idx))) {
  i1 <- pairs_idx[1, i]
  i2 <- pairs_idx[2, i]
  
  nct_res <- NCT(
    data1 = scaled_subsets[[i1]],
    data2 = scaled_subsets[[i2]],
    gamma = 0.5,
    it = 100,
    test.edges = TRUE,
    test.centrality = FALSE,
    paired = FALSE
  )
  
  # 以“1_vs_2”格式命名结果列表
  nct_results[[paste0(i1, "_vs_", i2)]] <- nct_res
}

# 查看结果
head(nct_results,50)
