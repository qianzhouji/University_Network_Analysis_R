library(NetworkComparisonTest)

#' Run NCT across all pairs of data subsets
#'
#' @param data_subsets List of data frames for each condition
#' @param vars_for_NCT Character vector of variable names to include in the networks
#' @param gamma EBIC hyperparameter passed to `NCT`
#' @param it Number of permutations for `NCT`
#' @param alpha Significance level for edge differences
#' @param test.centrality Logical; whether to test centrality differences
#' @param paired Logical; whether data sets are paired
#' @return A list containing tables for global strength, network structure,
#'         and significant edge differences, along with full NCT results
run_nct_pairs <- function(data_subsets,
                          vars_for_NCT,
                          gamma = 0.5,
                          it = 100,
                          alpha = 0.05,
                          test.centrality = FALSE,
                          paired = FALSE) {
  n_sets <- length(data_subsets)
  subset_names <- names(data_subsets)
  if (is.null(subset_names)) subset_names <- as.character(seq_len(n_sets))

  # Standardize selected variables for each subset
  scaled_subsets <- lapply(data_subsets, function(dat) {
    scale(dat[, vars_for_NCT])
  })

  # Generate all condition pairs
  pairs_idx <- combn(n_sets, 2)

  # Storage lists
  nct_results <- list()
  global_strength_list <- list()
  network_structure_list <- list()
  edge_diff_list <- list()

  for (i in seq_len(ncol(pairs_idx))) {
    i1 <- pairs_idx[1, i]
    i2 <- pairs_idx[2, i]
    cat("Comparing", subset_names[i1], "vs", subset_names[i2], "\n")
    pair_name <- paste0(i1, "_vs_", i2)

    nct_res <- NCT(
      data1 = scaled_subsets[[i1]],
      data2 = scaled_subsets[[i2]],
      gamma = gamma,
      it = it,
      test.edges = TRUE,
      test.centrality = test.centrality,
      paired = paired
    )

    nct_results[[pair_name]] <- nct_res

    # Global strength difference
    global_strength_list[[pair_name]] <- data.frame(
      condition_pair = pair_name,
      network1_strength = nct_res$glstrinv.sep[1],
      network2_strength = nct_res$glstrinv.sep[2],
      strength_difference = nct_res$glstrinv.real,
      p_value = nct_res$glstrinv.pval,
      row.names = NULL
    )

    # Network structure difference
    network_structure_list[[pair_name]] <- data.frame(
      condition_pair = pair_name,
      structure_difference = nct_res$nwinv.real,
      p_value = nct_res$nwinv.pval,
      row.names = NULL
    )

    # Edge differences - keep significant edges only
    edge_df <- nct_res$einv.pvals
    colnames(edge_df) <- c("Var1", "Var2", "p_value", "E")
    edge_df <- subset(edge_df, p_value < alpha)
    edge_df <- edge_df[order(edge_df$p_value), ]
    if (nrow(edge_df) > 0) {
      edge_df$condition_pair <- pair_name
      edge_df <- edge_df[, c("condition_pair", "Var1", "Var2", "E", "p_value")]
    }
    edge_diff_list[[pair_name]] <- edge_df
  }

  global_strength_table <- do.call(rbind, global_strength_list)
  network_structure_table <- do.call(rbind, network_structure_list)
  cat("All NCT comparisons completed.\n")

  list(
    global_strength = global_strength_table,
    network_structure = network_structure_table,
    edge_differences = edge_diff_list,
    nct_results = nct_results
  )
}

# Example usage:
# vars_for_NCT <- c("var1", "var2", "var3")
# results <- run_nct_pairs(data_subsets, vars_for_NCT)
