# Assume a list of data subsets called data_subsets
n_sets <- length(data_subsets)

# Select numeric variables (adjust according to your variable names)
vars_for_NCT <- vars[which(type_vec == "g")]

# Standardize these variables for each subset
scaled_subsets <- lapply(data_subsets, function(dat) {
  scale(dat[, vars_for_NCT])
})

# Create all condition pairs
pairs_idx <- combn(n_sets, 2)

# Lists to store results
nct_results <- list()
global_strength_list <- list()
network_structure_list <- list()
edge_diff_list <- list()

for (i in seq_len(ncol(pairs_idx))) {
  i1 <- pairs_idx[1, i]
  i2 <- pairs_idx[2, i]
  pair_name <- paste0(i1, "_vs_", i2)

  nct_res <- NCT(
    data1 = scaled_subsets[[i1]],
    data2 = scaled_subsets[[i2]],
    gamma = 0.5,
    it = 100,
    test.edges = TRUE,
    test.centrality = FALSE,
    paired = FALSE
  )

  nct_results[[pair_name]] <- nct_res

  # Global strength difference table
  global_strength_list[[pair_name]] <- data.frame(
    condition_pair = pair_name,
    network1_strength = nct_res$glstrinv.sep[1],
    network2_strength = nct_res$glstrinv.sep[2],
    strength_difference = nct_res$glstrinv.real,
    p_value = nct_res$glstrinv.pval,
    row.names = NULL
  )

  # Network structure difference table
  network_structure_list[[pair_name]] <- data.frame(
    condition_pair = pair_name,
    structure_difference = nct_res$nwinv.real,
    p_value = nct_res$nwinv.pval,
    row.names = NULL
  )

  # Edge differences (only significant edges)
  edge_df <- nct_res$einv.pvals
  colnames(edge_df) <- c("Var1", "Var2", "p_value", "E")
  edge_df <- subset(edge_df, p_value < 0.05)
  edge_df <- edge_df[order(edge_df$p_value), ]
  if (nrow(edge_df) > 0) {
    edge_df$condition_pair <- pair_name
    edge_df <- edge_df[, c("condition_pair", "Var1", "Var2", "E", "p_value")]
  }
  edge_diff_list[[pair_name]] <- edge_df
}

# Combine lists into tables
global_strength_table <- do.call(rbind, global_strength_list)
network_structure_table <- do.call(rbind, network_structure_list)
# edge_diff_list remains a list of tables for each pair

# View results
global_strength_table
network_structure_table
edge_diff_list
