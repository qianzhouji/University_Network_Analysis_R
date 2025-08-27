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
library(tidyr)
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数：对Bootstrap结果边权绝对值最高的TopN进行置信区间可视化————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

plot_topN_bootstrap_edges <- function(
    res_obj,                      # 必填: mgm::resample(...) 的返回对象
    mgm_fit    = NULL,            # 建议传: 对应条件下的 mgm 对象，用于点估计(wadj*signs)
    var_names  = NULL,            # 可选: 变量名；缺省用 mgm_fit 或自动生成 V1..Vp
    top_n      = 20,              # 总的 Top N（用于 mode="abs"/"pos"/"neg"；或 split 的默认拆分基数）
    mode       = c("abs","split","pos","neg"),#abs取绝对边权前n，split各取一半
    top_pos    = NULL,            # 仅在 mode="split" 生效：正边取多少
    top_neg    = NULL,            # 仅在 mode="split" 生效：负边取多少
    main_title = "网络分析结果",
    subtitle   = "Bootstrap 95%置信区间（TopN）",
    save_prefix = NULL,           # 若提供则保存 csv/png
    digits      = 3,
    font_family = "myfont"
){
  mode <- match.arg(mode)
  
  # ---- 0) 取 p、变量名 ----
  if (!is.null(res_obj$bootParameters)) {
    dims <- dim(res_obj$bootParameters)  # p x p x nB
    p    <- dims[1]
  } else if (!is.null(res_obj$bootQuantiles)) {
    p <- dim(res_obj$bootQuantiles)[1]
  } else {
    stop("res_obj 缺少 bootParameters/bootQuantiles。")
  }
  
  if (is.null(var_names)) {
    if (!is.null(mgm_fit) && !is.null(mgm_fit$pairwise$wadj)) {
      var_names <- if (!is.null(colnames(mgm_fit$pairwise$wadj))) {
        colnames(mgm_fit$pairwise$wadj)
      } else paste0("V", seq_len(p))
    } else {
      var_names <- paste0("V", seq_len(p))
    }
  }
  if (length(var_names) != p) stop("var_names 长度应等于 p。")
  
  # ---- 1) Point estimate（权重） ----
  if (!is.null(mgm_fit)) {
    W <- mgm_fit$pairwise$wadj
    S <- mgm_fit$pairwise$signs
    point_mat <- W * S
    point_mat[is.na(point_mat)] <- 0
  } else {
    # 没有 mgm_fit 就用 bootstrap 的中位数做点估计
    point_mat <- apply(res_obj$bootParameters, c(1,2), median, na.rm=TRUE)
  }
  
  # ---- 2) 置信区间 ----
  if (!is.null(res_obj$bootQuantiles)) {
    ci_low  <- res_obj$bootQuantiles[, , 1]
    ci_high <- res_obj$bootQuantiles[, , 2]
  } else {
    bp <- res_obj$bootParameters
    ci_low  <- apply(bp, c(1,2), quantile, probs = 0.025, na.rm = TRUE)
    ci_high <- apply(bp, c(1,2), quantile, probs = 0.975, na.rm = TRUE)
  }
  
  # 显著性（CI 不含 0）
  significant <- (ci_low > 0) | (ci_high < 0)
  
  # ---- 3) 展开为长表 ----
  edge_df <- data.frame(
    Var1 = character(0), Var2 = character(0),
    Weight = numeric(0), CI_lower = numeric(0), CI_upper = numeric(0),
    Significant = logical(0), stringsAsFactors = FALSE
  )
  for (i in 1:(p-1)) for (j in (i+1):p) {
    edge_df <- rbind(edge_df, data.frame(
      Var1 = var_names[i],
      Var2 = var_names[j],
      Weight = point_mat[i, j],
      CI_lower = ci_low[i, j],
      CI_upper = ci_high[i, j],
      Significant = significant[i, j],
      stringsAsFactors = FALSE
    ))
  }
  
  # ---- 4) 选择 TopN（按 mode）----
  pick_abs <- function(df, n) df[order(-abs(df$Weight)), , drop=FALSE][seq_len(min(n, nrow(df))), , drop=FALSE]
  pick_pos <- function(df, n) {
    dfp <- df[df$Weight > 0, , drop=FALSE]
    dfp <- dfp[order(-dfp$Weight), , drop=FALSE]
    dfp[seq_len(min(n, nrow(dfp))), , drop=FALSE]
  }
  pick_neg <- function(df, n) {
    dfn <- df[df$Weight < 0, , drop=FALSE]
    dfn <- dfn[order(dfn$Weight), , drop=FALSE] # 越负越靠前
    dfn[seq_len(min(n, nrow(dfn))), , drop=FALSE]
  }
  
  if (mode == "abs") {
    top_df <- pick_abs(edge_df, top_n)
  } else if (mode == "pos") {
    top_df <- pick_pos(edge_df, top_n)
  } else if (mode == "neg") {
    top_df <- pick_neg(edge_df, top_n)
  } else { # split
    if (is.null(top_pos) && is.null(top_neg)) {
      top_pos <- ceiling(top_n/2); top_neg <- floor(top_n/2)
    }
    pos_df <- pick_pos(edge_df, top_pos)
    neg_df <- pick_neg(edge_df, top_neg)
    top_df <- rbind(pos_df, neg_df)
    # 为了展示顺序更清晰：先正再负，各自按幅度
    top_df <- rbind(
      pos_df[order(-pos_df$Weight), , drop=FALSE],
      neg_df[order(neg_df$Weight), , drop=FALSE]
    )
  }
  
  # ---- 5) 画图 ----
  suppressPackageStartupMessages({
    library(ggplot2)
  })
  top_df$Edge <- paste(top_df$Var1, top_df$Var2, sep = " - ")
  # 为了让坐标轴顺序与权重一致：正边按大到小，负边按小到大；abs/pos/neg 用权重或绝对值一致排序
  if (mode %in% c("pos","neg")) {
    ord <- if (mode=="pos") order(-top_df$Weight) else order(top_df$Weight)
  } else {
    ord <- order(-abs(top_df$Weight))
  }
  top_df <- top_df[ord, , drop=FALSE]
  
  plt <- ggplot(top_df, aes(x = reorder(Edge, Weight), y = Weight)) +
    geom_point(aes(color = Significant), size = 2.8) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, color = Significant), width = 0.2) +
    coord_flip() +
    labs(
      title = main_title,
      subtitle = subtitle,
      x = "Edge (Var1 - Var2)",
      y = "Edge weight"
    ) +
    scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey60")) +
    theme_minimal() +
    theme(
      axis.text.y   = element_text(size = 10, family = font_family),
      axis.title    = element_text(family = font_family),
      plot.title    = element_text(family = font_family, hjust = 0.5, size = 15, face = "bold"),
      plot.subtitle = element_text(family = font_family, hjust = 0.5, size = 11, face = "italic"),
      legend.position = "bottom"
    )
  
  print(plt)
  
  # ---- 6) 可选保存 ----
  if (!is.null(save_prefix)) {
    #utils::write.csv(edge_df, paste0(save_prefix, "_bootstrap_edges_full.csv"), row.names = FALSE)
    #utils::write.csv(top_df,  paste0(save_prefix, "_bootstrap_edges_top.csv"),  row.names = FALSE)
    ggplot2::ggsave(paste0(save_prefix, "_bootstrap_CI_plot.pdf"), plt, width = 9, height = 7, dpi = 300)
  }
  
  invisible(list(
    edge_stability = edge_df,
    top_edges      = top_df,
    plot           = plt
  ))
}


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：社群分析与可视化——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
community_detection_and_plot <- function(
    wadj, signs, var_names,
    output_dic = TRUE,
    return_full = FALSE,
    save_path = NULL
) {
  library(igraph)
  
  # 1. 恢复带符号边权矩阵
  wadj_signed <- wadj * signs
  
  # 2. 清理NA/NaN
  wadj_signed[is.na(wadj_signed)] <- 0
  wadj_signed[is.nan(wadj_signed)] <- 0
  
  # 3. 构造igraph对象（无向加权图，权重取绝对值）
  g <- graph_from_adjacency_matrix(abs(wadj_signed), mode = "undirected", weighted = TRUE, diag = FALSE)
  
  # 4. 社群检测
  comm <- cluster_louvain(g)
  group_vector <- membership(comm)
  
  cat("社区节点数量分布：\n")
  print(table(group_vector))
  
  # 5. 输出节点编号与变量名对应表
  node_map <- NULL
  if (output_dic) {
    node_ids <- as.numeric(V(g)$name)
    if (is.null(node_ids) || length(node_ids) != length(var_names)) {
      node_ids <- 1:length(var_names)
    }
    node_map <- data.frame(NodeID = node_ids, VariableName = var_names, stringsAsFactors = FALSE)
    cat("节点编号对应变量名:\n")
    print(node_map)
  }
  
  # 6. 绘图
  layout <- layout_with_fr(g)
  
  if (!is.null(save_path)) {
    png(save_path)
    plot(comm, g,
         main = "基于Louvain社群的网络可视化",
         vertex.size = 30,
         vertex.label.cex = 0.8
    )
    dev.off()
    cat(paste("社群网络图保存至:", save_path))
    
    plot(comm, g,
         main = "基于Louvain社群的网络可视化",
         vertex.size = 30,
         vertex.label.cex = 0.8
    )
  } else {
    plot(comm, g,
         main = "基于Louvain社群的网络可视化",
         vertex.label = V(g)$name,
         vertex.size = 30,
         vertex.label.cex = 0.8
    )
  }
  
  # 7. 返回结果
  if (return_full) {
    return(list(
      graph = g,
      communities = comm,
      group_vector = group_vector,
      node_map = node_map
    ))
  } else {
    return(group_vector)
  }
}
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#子函数1:计算BEI（针对单个网络）——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
compute_BEI_for_network <- function(wadj, signs, group_vector, var_names = NULL) {
  # 恢复带符号边权矩阵
  wadj_signed <- wadj * signs
  # 清理NA/NaN
  wadj_signed[is.na(wadj_signed)] <- 0
  wadj_signed[is.nan(wadj_signed)] <- 0
  
  p <- ncol(wadj_signed)
  
  if (is.null(var_names)) {
    var_names <- paste0("V", 1:p)
  }
  
  # 计算单个节点BEI函数
  compute_node_BEI <- function(node_i) {
    node_group <- group_vector[node_i]
    neighbors <- setdiff(1:p, node_i)
    inter_group_neighbors <- neighbors[group_vector[neighbors] != node_group]
    sum(abs(wadj_signed[node_i, inter_group_neighbors]))
  }
  
  # 计算所有节点BEI
  BEI_values <- sapply(1:p, compute_node_BEI)
  
  # 构建数据框
  BEI_df <- data.frame(
    Node = var_names,
    Group = as.factor(group_vector),
    BEI = BEI_values,
    stringsAsFactors = FALSE
  )
  
  # 按BEI降序排列因子方便绘图
  BEI_df$Node <- factor(BEI_df$Node, levels = BEI_df$Node[order(BEI_df$BEI, decreasing = TRUE)])
  
  return(BEI_df)
}
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#子函数2:绘制BEI结果（支持多条件）——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
plot_BEI_results <- function(BEI_df, plot_title = "Bridge Expected Influence (BEI) across Conditions") {
  # 如果包含条件变量，转成因子并排序
  if ("Condition" %in% colnames(BEI_df)) {
    BEI_df$Condition <- factor(BEI_df$Condition, levels = unique(BEI_df$Condition))
  } else {
    BEI_df$Condition <- "Overall"
  }
  
  p <- ggplot(BEI_df, aes(x = Node, y = BEI, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_wrap(~ Condition, scales = "free_y") +
    coord_flip() +
    labs(title = plot_title, x = "节点", y = "Bridge Expected Influence (BEI)") +
    theme_minimal() +
    theme(
      text = element_text(family = "myfont"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold")
    )
  
  print(p)
  return(p)
}
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#BEI主函数：计算+绘图，自选带不带调节变量——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

compute_and_plot_BEI <- function(
    cond_list,                 # list: 每个条件下的 mgm 对象（已去掉调节变量列拟合）
    group_vector,              # 数值/因子向量：长度需等于 p（分条件网络的节点数）
    var_names,                 # 变量名（不含调节变量），长度 = p
    condition_labels = NULL,   # 条件标签，如 c("Level 1","Level 2",...)
    plot_title = "Bridge Expected Influence (BEI) across Conditions",
    save_name = "BEI.pdf"
) {
  # --- 基本检查 ---
  stopifnot(is.list(cond_list), length(cond_list) >= 1)
  p <- ncol(cond_list[[1]]$pairwise$wadj)
  if (length(var_names) != p) {
    stop("var_names 长度必须等于每个条件网络的节点数 p（不含调节变量）。")
  }
  if (length(group_vector) != p) {
    stop("group_vector 长度必须等于 p（不含调节变量）。如先前按全量变量分组，请去掉调节变量对应的组别或重跑社群分析。")
  }
  if (is.null(condition_labels)) {
    condition_labels <- paste0("Condition ", seq_along(cond_list))
  }
  if (length(condition_labels) != length(cond_list)) {
    stop("condition_labels 的长度需与 cond_list 相同。")
  }
  
  # --- 逐条件计算 BEI ---
  BEI_results <- vector("list", length(cond_list))
  for (g in seq_along(cond_list)) {
    wadj_g  <- cond_list[[g]]$pairwise$wadj
    signs_g <- cond_list[[g]]$pairwise$signs
    BEI_df_g <- compute_BEI_for_network(
      wadj  = wadj_g,
      signs = signs_g,
      group_vector = group_vector,
      var_names = var_names
    )
    BEI_df_g$Condition <- condition_labels[g]
    BEI_results[[g]] <- BEI_df_g
  }
  
  # 合并 & 画图
  BEI_all <- do.call(rbind, BEI_results)
  pdf(file.path("plot", save_name))
  p_BEI   <- plot_BEI_results(BEI_all, plot_title = plot_title)
  dev.off()
  cat(paste0("BEI网络图已存储为", save_name))
  
  p_BEI   <- plot_BEI_results(BEI_all, plot_title = plot_title)
  
  invisible(list(
    BEI_data = BEI_all,
    BEI_plot = p_BEI
  ))
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：模拟Factor_Graph绘图函数：矩阵扩容+qgraph绘图——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
plot_moderation_qgraph <- function(
    mgm_fit,
    var_names,
    moderator_index = 1,
    moderation_df = NULL,
    pairwise_wadj = NULL,
    pairwise_signs = NULL,
    
    # —— 新增：仅显示|调节效应|前K条；NULL则显示全部 ——
    top_k_mod = NULL,
    
    # —— 颜色与样式 ——
    pos_edge_color = "#1E88E5",
    neg_edge_color = "#D81B60",
    mod_edge_color = "#6D6D6D",
    edge_alpha     = 1.0,
    edge_labels_cex = 0.9,
    
    var_node_color = "white",
    mod_node_color = "#1F77B4",     # 蓝色调节节点
    triangle_label = NULL,
    
    show_edge_labels = TRUE,
    edge_label_digits = 3,
    
    layout = "spring",
    vsize_vars = 6,
    vsize_mods = 6,
    main_title = "带调节作用的网络图"
) {
  stopifnot(is.matrix(mgm_fit$pairwise$wadj))
  p <- length(var_names)
  if (is.null(pairwise_wadj))  pairwise_wadj  <- mgm_fit$pairwise$wadj
  if (is.null(pairwise_signs)) pairwise_signs <- mgm_fit$pairwise$signs
  if (ncol(pairwise_wadj) != p || ncol(pairwise_signs) != p) {
    stop("pairwise_wadj / pairwise_signs 与 var_names 维度不一致。")
  }
  if (is.null(triangle_label)) triangle_label <- var_names[moderator_index]
  
  idx_of <- function(v) if (is.numeric(v)) as.integer(v) else match(v, var_names)
  
  # 1) 准备调节列表 mod_list：list(i, j, w, s)
  if (!is.null(moderation_df)) {
    df <- moderation_df
    if (!all(c("Var1","Var2","Weight") %in% colnames(df)))
      stop("moderation_df 需包含列: Var1, Var2, Weight，(可选)Sign。")
    v1 <- idx_of(df$Var1); v2 <- idx_of(df$Var2)
    if (anyNA(v1) || anyNA(v2)) stop("moderation_df$Var1/Var2 无法匹配 var_names。")
    w  <- as.numeric(df$Weight)
    s  <- if ("Sign" %in% colnames(df)) as.numeric(df$Sign) else sign(w)
    s[is.na(s) | s == 0] <- 1
    mod_list <- Map(function(i,j,ww,ss) list(i=i, j=j, w=ww, s=ss), v1, v2, w, s)
  } else {
    tri_idx  <- tryCatch(mgm_fit$interactions$indicator[[2]], error = function(e) NULL)
    tri_wagg <- tryCatch(mgm_fit$interactions$weightsAgg[[2]], error = function(e) NULL)
    tri_sign <- tryCatch(mgm_fit$interactions$signs[[2]],      error = function(e) NULL)
    if (is.null(tri_idx) || is.null(tri_wagg)) {
      stop("未提供 moderation_df，且模型对象中未检测到三阶交互（k=3）。请传入 moderation_df。")
    }
    if (is.null(dim(tri_idx))) tri_idx <- matrix(tri_idx, ncol = 3, byrow = TRUE)
    keep <- apply(tri_idx, 1, function(x) moderator_index %in% x)
    tri_idx  <- tri_idx[keep, , drop=FALSE]
    tri_wagg <- unlist(tri_wagg)[keep]
    tri_sign <- if (!is.null(tri_sign)) as.numeric(unlist(tri_sign)[keep]) else rep(NA_real_, sum(keep))
    
    mod_list <- vector("list", nrow(tri_idx))
    for (r in seq_len(nrow(tri_idx))) {
      ij <- setdiff(tri_idx[r, ], moderator_index)
      if (length(ij) != 2) { mod_list[[r]] <- NULL; next }
      w  <- as.numeric(tri_wagg[r])
      s  <- ifelse(is.na(tri_sign[r]) || tri_sign[r]==0, sign(w), tri_sign[r])
      if (s == 0) s <- 1
      mod_list[[r]] <- list(i = ij[1], j = ij[2], w = w, s = s)
    }
    mod_list <- Filter(Negate(is.null), mod_list)
    if (length(mod_list) == 0) stop("未筛到包含指定调节变量的三阶交互。")
  }
  
  # —— 1.5) 可选：仅保留|w|最大的前K条调节作用 ——
  if (!is.null(top_k_mod)) {
    if (!is.numeric(top_k_mod) || length(top_k_mod) != 1 || top_k_mod <= 0) {
      stop("top_k_mod 需要为正整数或 NULL。")
    }
    # 对 mod_list 按 |w| 降序排列并截取前K
    ord <- order(sapply(mod_list, function(x) -abs(x$w)))
    mod_list <- mod_list[ord]
    if (length(mod_list) > top_k_mod) mod_list <- mod_list[seq_len(top_k_mod)]
    main_title <- sprintf("%s（Top %d 调节）", main_title, length(mod_list))
  }
  
  m <- length(mod_list)
  if (m == 0) stop("没有可绘制的调节作用。")
  
  # 2) 扩展邻接矩阵
  W <- matrix(0, nrow = p + m, ncol = p + m)
  W[1:p, 1:p] <- pairwise_wadj * pairwise_signs
  diag(W) <- 0
  for (k in seq_len(m)) {
    fk <- p + k
    i  <- mod_list[[k]]$i
    j  <- mod_list[[k]]$j
    w  <- abs(mod_list[[k]]$w)
    W[fk, i] <- w; W[i, fk] <- w
    W[fk, j] <- w; W[j, fk] <- w
  }
  
  
  # 3) 节点标签/形状/颜色
  node_labels <- c(var_names, rep(ifelse(is.null(triangle_label), var_names[moderator_index], triangle_label), m))
  shapes      <- c(rep("circle", p), rep("triangle", m))
  vsize_vec   <- c(rep(vsize_vars, p), rep(vsize_mods, m))
  node_colors <- c(rep(var_node_color, p), rep(mod_node_color, m))
  
  # 4) 固定边色 + 标签
  adj_col <- function(col) grDevices::adjustcolor(col, alpha.f = edge_alpha)
  Cmat <- matrix(adj_col(mod_edge_color), nrow = p + m, ncol = p + m)
  base_signed <- pairwise_wadj * pairwise_signs
  Cmat[1:p, 1:p][ base_signed > 0 ] <- adj_col(pos_edge_color)
  Cmat[1:p, 1:p][ base_signed < 0 ] <- adj_col(neg_edge_color)
  
  Elabels <- NULL
  if (show_edge_labels) {
    Elabels <- matrix("", nrow = p + m, ncol = p + m)
    for (k in seq_len(m)) {
      fk <- p + k
      i  <- mod_list[[k]]$i
      j  <- mod_list[[k]]$j
      lab <- formatC(mod_list[[k]]$w, digits = edge_label_digits, format = "fg")
      Elabels[fk, i] <- lab; Elabels[i, fk] <- lab
      Elabels[fk, j] <- lab; Elabels[j, fk] <- lab
    }
  }
  
  # 5) 绘图
  qg <- qgraph::qgraph(
    W,
    layout      = layout,
    labels      = node_labels,
    shape       = shapes,
    color       = node_colors,
    edge.color  = Cmat,
    edge.labels = Elabels,
    edge.labels.cex = edge_labels_cex,
    edge.labels = TRUE,   
    label.cex   = 1.0,
    vsize       = vsize_vec,
    title       = main_title
  )
  
  pdf(file.path("plot", "network_with_mod.pdf"))
  qgraph(W,layout  = layout,labels = node_labels,shape = shapes,color = node_colors,edge.color = Cmat,edge.labels = Elabels,edge.labels.cex = edge_labels_cex,edge.labels = TRUE,label.cex = 1.0,vsize = vsize_vec,title = main_title)
  dev.off()
  
  invisible(list(
    W            = W,
    colors       = Cmat,
    edge_labels  = Elabels,
    node_labels  = node_labels,
    node_colors  = node_colors,
    shapes       = shapes,
    mod_list     = mod_list,
    qgraph       = qg
  ))
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#子功能函数：从mgm_fit中提取moderation_df——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# 从 mgm_fit$interactions 构建 moderation_df
# - mgm_fit: mgm() 的拟合结果
# - var_names: 可选，字符向量，长度 = p，用作 Var1/Var2 的标签（不传则给出索引）
# - moderator_index: 整数，调节变量在数据中的列号（默认 1）
# - use = c("weightsAgg","weights") 选择用聚合权重还是原参数（默认 weightsAgg）
# - drop_duplicates: 是否对同一 (Var1,Var2) 多条记录去重并取平均（默认 TRUE，稳妥）
# 返回：data.frame(Var1, Var2, Weight, Sign)
make_moderation_df <- function(mgm_fit,
                               var_names = NULL,
                               moderator_index = 1,
                               use = c("weightsAgg", "weights"),
                               drop_duplicates = TRUE) {
  use <- match.arg(use)
  
  # 1) 取出三阶交互（[[2]] 对应 order=3）
  ind_all  <- tryCatch(mgm_fit$interactions$indicator[[2]], error = function(e) NULL)
  w_all    <- tryCatch(mgm_fit$interactions[[use]][[2]],   error = function(e) NULL)
  s_all    <- tryCatch(mgm_fit$interactions$signs[[2]],    error = function(e) NULL)
  
  if (is.null(ind_all) || is.null(w_all)) {
    stop("未检测到三阶交互（interactions[[2]] 为空）。请确认模型包含 k=3 或使用 moderators 参数拟合。")
  }
  
  # indicator 可能在只有一行时被降维成向量，这里强制成 n x 3 矩阵
  if (is.null(dim(ind_all))) ind_all <- matrix(ind_all, ncol = 3, byrow = TRUE)
  
  # weightsAgg/weights 结构是 list，每个元素对应 indicator 的一行；转成数值向量
  w_vec <- unlist(w_all)
  if (length(w_vec) != nrow(ind_all)) {
    stop("weights 与 indicator 行数不匹配：请检查 mgm_fit$interactions 结构。")
  }
  
  # 同理处理 signs（可能缺失）
  s_vec <- if (!is.null(s_all)) {
    as.numeric(unlist(s_all))
  } else {
    rep(NA_real_, length(w_vec))
  }
  
  # 2) 仅保留包含指定调节变量的三元组
  keep <- apply(ind_all, 1, function(x) moderator_index %in% x)
  if (!any(keep)) stop("未发现包含指定调节变量的三阶交互。请检查 moderator_index 是否正确。")
  
  tri <- ind_all[keep, , drop = FALSE]
  w   <- w_vec[keep]
  s   <- s_vec[keep]
  
  # 3) 对每个三元组，去掉调节变量，留下被调节的两个变量 (i, j)
  ij_mat <- t(apply(tri, 1, function(x) setdiff(x, moderator_index)))
  if (is.null(dim(ij_mat))) ij_mat <- matrix(ij_mat, ncol = 2, byrow = TRUE)
  
  # 4) 组装数据框（可用索引或变量名）
  to_label <- function(idx) {
    if (is.null(var_names)) idx else var_names[idx]
  }
  df <- data.frame(
    Var1   = to_label(ij_mat[, 1]),
    Var2   = to_label(ij_mat[, 2]),
    Weight = as.numeric(w),
    Sign   = as.numeric(s),
    stringsAsFactors = FALSE
  )
  
  # 若 Sign 缺失或为 0，则用 Weight 的正负来补
  df$Sign[is.na(df$Sign) | df$Sign == 0] <- sign(df$Weight)
  df$Sign[df$Sign == 0] <- 1  # 权重恰好为0时给个默认正号，主要用于显示
  
  # 5) 可选：同一 (Var1, Var2) 如重复，取平均（有时不同节点回归会留下重复）
  if (drop_duplicates) {
    df <- aggregate(cbind(Weight, Sign) ~ Var1 + Var2, data = df, FUN = mean)
  }
  
  # 6) 排序：按 |Weight| 从大到小，方便查看
  df <- df[order(-abs(df$Weight)), ]
  rownames(df) <- NULL
  df
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：边权差异统计-两两condition进行置信区间做差——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

edge_difference_test <- function(
    bootstrap_results_list,   # list: 每个条件的 resample() 返回对象
    cond_list,                # list: 每个条件的 mgm 拟合对象（与 bootstrap 对应）
    var_names = NULL,         # 可选：变量名；若为 NULL，则从 cond_list 推断
    condition_labels = NULL,  # 可选：每个条件的标签；默认 "Cond1" 等
    conf_level = 0.95,        # 置信水平
    top_k = NULL,             # 可选：仅返回 |diff| 最大的前K条（对每个条件对）
    signed = FALSE            # 是否比较“有符号强度”（启发式：用拟合网络 signs 乘以 bootstrap）
) {
  # --- 基本一致性检查 ---
  stopifnot(length(bootstrap_results_list) == length(cond_list))
  G <- length(cond_list)
  if (G < 2) stop("至少需要两个条件才能比较边权差异。")
  
  # --- 维度与变量名 ---
  # 从第一个条件确定 p 与默认变量名
  p <- ncol(cond_list[[1]]$pairwise$wadj)
  if (is.null(var_names)) {
    # 优先从 cond_list 的数据结构里取
    cn <- colnames(cond_list[[1]]$data)
    var_names <- if (!is.null(cn)) cn else paste0("V", seq_len(p))
  } else {
    if (length(var_names) != p) {
      stop("var_names 长度应等于每个条件网络的节点数（去掉调节变量后的 p）。")
    }
  }
  
  # 条件标签
  if (is.null(condition_labels)) {
    condition_labels <- paste0("Cond", seq_len(G))
  }
  stopifnot(length(condition_labels) == G)
  
  # --- 取各条件点估计（可选带符号） ---
  hat_list <- vector("list", G)
  sign_list <- vector("list", G)
  for (g in seq_len(G)) {
    wadj_g  <- cond_list[[g]]$pairwise$wadj
    signs_g <- cond_list[[g]]$pairwise$signs
    signs_g[is.na(signs_g)] <- 0
    sign_list[[g]] <- signs_g
    hat_list[[g]] <- if (signed) wadj_g * signs_g else wadj_g
  }
  
  # --- 取各条件 bootstrap 数组 p×p×nB，并按需加符号（启发式） ---
  boot_arr_list <- vector("list", G)
  nB_vec <- integer(G)
  for (g in seq_len(G)) {
    arr <- bootstrap_results_list[[g]]$bootParameters  # 注意：没有 $bootstrap 这层
    if (length(dim(arr)) != 3 || dim(arr)[1] != p || dim(arr)[2] != p) {
      stop(sprintf("第 %d 个条件的 bootParameters 不是 p×p×nB 的三维数组。", g))
    }
    # 如需有符号比较，用拟合网络的 signs 近似对齐方向
    if (signed) {
      arr <- sweep(arr, 1:2, sign_list[[g]], `*`)
    }
    boot_arr_list[[g]] <- arr
    nB_vec[g] <- dim(arr)[3]
  }
  nB_use <- min(nB_vec)
  alpha <- (1 - conf_level) / 2
  
  # --- 遍历所有条件对 ---
  combs <- t(combn(G, 2))
  out_list <- vector("list", nrow(combs))
  
  for (cc in seq_len(nrow(combs))) {
    a <- combs[cc, 1]
    b <- combs[cc, 2]
    lab_a <- condition_labels[a]
    lab_b <- condition_labels[b]
    
    rows <- vector("list", p * (p - 1) / 2)
    idx <- 0
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        # 点估计差
        w_a_hat <- hat_list[[a]][i, j]
        w_b_hat <- hat_list[[b]][i, j]
        diff_hat <- w_a_hat - w_b_hat
        
        # bootstrap 差异分布（对齐 nB_use）
        boot_a <- boot_arr_list[[a]][i, j, 1:nB_use]
        boot_b <- boot_arr_list[[b]][i, j, 1:nB_use]
        # 容错：NA -> 0
        boot_a[is.na(boot_a)] <- 0
        boot_b[is.na(boot_b)] <- 0
        diff_boot <- boot_a - boot_b
        
        ci_low  <- as.numeric(quantile(diff_boot, probs = alpha,    na.rm = TRUE))
        ci_high <- as.numeric(quantile(diff_boot, probs = 1 - alpha, na.rm = TRUE))
        sig <- !(ci_low <= 0 & ci_high >= 0)
        
        idx <- idx + 1
        rows[[idx]] <- data.frame(
          Pair        = paste0(lab_a, " - ", lab_b),
          Var1        = var_names[i],
          Var2        = var_names[j],
          Est_A       = w_a_hat,
          Est_B       = w_b_hat,
          Diff_AB     = diff_hat,
          CI_low      = ci_low,
          CI_high     = ci_high,
          Significant = sig,
          stringsAsFactors = FALSE
        )
      }
    }
    df_pair <- do.call(rbind, rows)
    
    # 可选：按 |Diff_AB| 排序并保留前K
    if (!is.null(top_k) && top_k > 0 && nrow(df_pair) > top_k) {
      ord <- order(abs(df_pair$Diff_AB), decreasing = TRUE)
      df_pair <- df_pair[ord, , drop = FALSE]
      df_pair <- head(df_pair, top_k)
    } else {
      df_pair <- df_pair[order(-abs(df_pair$Diff_AB)), , drop = FALSE]
    }
    out_list[[cc]] <- df_pair
  }
  
  out_df <- do.call(rbind, out_list)
  rownames(out_df) <- NULL
  return(out_df)
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：边权差异统计-两两condition进行独立样本t检验——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

t_test_edge_diff <- function(
    cond_list,           # list: 每个条件下的网络模型（通过mgm::condition创建）
    var_names,           # 变量名称向量，长度=p
    top_n = NULL,        # 可选：显示显著性前n条
    conf_level = 0.95    # 可选：置信水平（默认95%）
) {
  stopifnot(length(cond_list) > 1)  # 至少需要两个条件进行比较
  G <- length(cond_list)
  p <- length(var_names)
  
  # 初始化空的结果列表
  result_list <- list()
  
  # 计算每对条件下边的差异并进行t检验
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      A <- var_names[i]
      B <- var_names[j]
      
      # 初始化每对条件的边权差和t检验结果
      diff_list <- numeric(G)
      t_values <- numeric(G)
      p_values <- numeric(G)
      
      # 获取每个条件下的边权
      for (g in 1:G) {
        wadj  <- cond_list[[g]]$pairwise$wadj
        signs <- cond_list[[g]]$pairwise$signs
        signed_w <- wadj[i, j] * signs[i, j]
        diff_list[g] <- signed_w
      }
      
      # 计算差异：两个条件之间的边权差
      diff <- diff_list[1] - diff_list[2]
      
      # 计算t值和p值
      # 使用t检验
      t_test_result <- t.test(diff_list)
      t_values <- t_test_result$statistic
      p_values <- t_test_result$p.value
      
      # 判定显著性
      significance <- ifelse(p_values < (1 - conf_level), 1, 0)
      
      # 组合结果
      result_list[[length(result_list) + 1]] <- data.frame(
        Edge = paste(A, "-", B),
        Condition_1 = diff_list[1],
        Condition_2 = diff_list[2],
        Condition_3 = diff_list[3],
        Diff = diff,
        t_value = t_values,
        Significance = significance
      )
    }
  }
  
  # 将结果转换为数据框
  result_df <- do.call(rbind, result_list)
  
  # 按t值排序（降序），并显示显著性最强的前top_n条边
  result_df <- result_df[order(-result_df$t_value), ]
  if (!is.null(top_n)) {
    result_df <- head(result_df, top_n)
  }
  
  # 返回结果
  return(result_df)
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#子函数1.0：按调节变量分条件提取——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# data_mat       : 原始数据集 (matrix/data.frame)
# moderator_index: 调节变量所在列号
# var_names      : （可选）变量名向量；若缺省则自动取 colnames(data_mat)

split_by_moderator <- function(data_mat, moderator_index = 1, var_names = NULL) {
  stopifnot(is.matrix(data_mat) || is.data.frame(data_mat))
  if (is.null(var_names)) var_names <- colnames(data_mat)
  
  moderator_values <- sort(unique(data_mat[, moderator_index]))
  n_levels <- length(moderator_values)
  
  data_subsets <- vector("list", n_levels)
  
  for (i in seq_len(n_levels)) {
    sub <- data_mat[data_mat[, moderator_index] == moderator_values[i],
                    -moderator_index, drop = FALSE]
    colnames(sub) <- var_names[-moderator_index]
    data_subsets[[i]] <- sub
  }
  
  list(
    data_subsets = data_subsets,          # 各条件下的子数据集
    moderator_values = moderator_values   # 调节变量的所有水平
  )
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#子函数1.1：分条件进行mgm——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# data_subsets : 由拆分函数得到的子数据集列表
# 其余参数与原先一致；若未提供 type_vec/level_vec，则自动推断

run_mgm_for_conditions <- function(
    data_subsets,
    maxit      = 300000,
    lambdaSel  = "EBIC",
    lambdaGam  = 0.8,
    ruleReg    = "OR",
    scale      = TRUE,
    type_vec   = NULL,
    level_vec  = NULL,
    pbar       = FALSE
) {
  n_levels  <- length(data_subsets)
  cond_list <- vector("list", n_levels)
  
  # 若未传入 type_vec，则从首个子数据集推断一次
  if (is.null(type_vec)) {
    df0 <- data_subsets[[1]]
    type_vec <- sapply(seq_len(ncol(df0)), function(j) {
      col <- df0[, j]; col <- col[!is.na(col)]
      if (is.factor(col) || all(col == round(col))) "c" else "g"
    })
  }
  
  for (i in seq_len(n_levels)) {
    df <- data_subsets[[i]]
    
    # level_vec 为空时，按当前子数据集计算
    level_current <- if (is.null(level_vec)) {
      sapply(seq_len(ncol(df)), function(j) {
        col <- df[, j]; col <- col[!is.na(col)]
        if (type_vec[j] == "c") length(unique(col)) else 1
      })
      cat(sprintf("\ntype_vec 和 level_vec 已根据条件 %d 自动推断。\n", i))
    } else level_vec
    
    cat(sprintf("正在为条件 %d 拟合 MGM 模型...\n", i))
    cond_list[[i]] <- mgm(
      data  = df,
      type  = type_vec,
      level = level_current,
      k     = 2,
      lambdaSel = lambdaSel,
      lambdaGam = lambdaGam,
      ruleReg   = ruleReg,
      scale     = scale,
      glmnet.control = list(maxit = maxit),
      pbar      = pbar
    )
  }
  
  cond_list
}


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————



#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#子函数2：根据分条件mgm结果进行bootstrap，并存储显著边————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

run_bootstrap_for_conditions <- function(
    cond_list,               # 每个条件下的 mgm 模型列表
    data_subsets,            # 每个条件下的子数据集列表
    nB = 100,                # bootstrap次数
    maxit = 300000,          # glmnet最大迭代次数
    lambdaSel = "EBIC",      # 正则化方法
    lambdaGam = 0.8,         # 正则化参数
    ruleReg = "OR",          # 正则化规则
    top_k = NULL,            # 可选：仅返回前top_k显著边
    pbar = FALSE
) {
  # 检查输入的条件列表长度与数据集一致
  stopifnot(length(cond_list) == length(data_subsets))
  
  # 提示正在进行的工作内容
  cat("\n正在对每个条件的网络进行Bootstrap检验...\n")
  
  # 用于存储每个条件的bootstrap结果
  bootstrap_results_list <- vector("list", length(cond_list))
  
  # 用于存储显著边打印结果
  significant_edges_df <- data.frame(
    Edge = character(0),
    Condition = integer(0),
    Weight = numeric(0),
    CI_low = numeric(0),
    CI_high = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # 用于存储每个条件的显著边矩阵
  sig_list <- vector("list", length(cond_list))
  
  # 循环每个条件
  for (i in seq_along(cond_list)) {
    cat(sprintf("\n正在对条件 %d 进行Bootstrap检验...\n", i))
    
    # 获取当前条件的mgm模型
    mgm_fit <- cond_list[[i]]
    wadj <- mgm_fit$pairwise$wadj
    signs <- mgm_fit$pairwise$signs
    
    # 获取对应的子数据集
    subset_data <- data_subsets[[i]]
    
    # 使用resample函数进行bootstrap
    res_obj <- resample(
      object = mgm_fit,
      data = subset_data,  # 使用子数据集
      nB = nB,
      glmnet.control = list(maxit = maxit),
      pbar = pbar
    )
    
    # 存储bootstrap结果
    bootstrap_results_list[[i]] <- res_obj
    
    # 获取bootstrap结果的置信区间
    ci_low <- apply(res_obj$bootParameters, c(1, 2), quantile, probs = 0.025)
    ci_high <- apply(res_obj$bootParameters, c(1, 2), quantile, probs = 0.975)
    
    # 找到显著边
    significant_edges <- (ci_low > 0) | (ci_high < 0)
    
    sig_list[[i]] <- significant_edges
    
    # 获取显著边信息并存储
    for (k in 1:(ncol(subset_data)-1)) {
      for (j in (k+1):ncol(subset_data)) {
        if (significant_edges[k, j]) {
          significant_edges_df <- rbind(significant_edges_df, data.frame(
            Edge = paste(colnames(subset_data)[k], "-", colnames(subset_data)[j]),
            Condition = i,
            Weight = wadj[k, j],
            CI_low = ci_low[k, j],
            CI_high = ci_high[k, j],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
  }
  
  # 排序并返回Top_k显著边
  significant_edges_df <- significant_edges_df[order(abs(significant_edges_df$Weight), decreasing = TRUE), ]
  if (!is.null(top_k)) {
    significant_edges_df <- head(significant_edges_df, top_k)
  }
  
  # 返回结果
  return(list(
    bootstrap_results_list = bootstrap_results_list,  # 每个条件下的bootstrap检验结果
    significant_edges_df = significant_edges_df,  # 显著边打印信息数据框
    sig_list = sig_list  # 显著边矩阵 
  ))
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# 自适应拼图：多个 condition 的 TopN CI 图（调用 plot_topN_bootstrap_edges）
# 依赖：ggplot2, patchwork, readr（若保存CSV），
#       plot_topN_bootstrap_edges()（吃 resample 对象 + 可传 mgm_fit）
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

plot_conditions_topN <- function(
    bootstrap_results_list,     # list: 每个条件的 resample() 对象
    cond_list = NULL,           # list: 每个条件的 mgm 对象（建议提供以取 wadj/signs/colnames）
    var_names = NULL,           # 去掉调节变量后的变量名（若 mgm$data 没列名则需手动传）
    condition_labels = NULL,    # 每个条件的标签；NULL 则 "Cond1"... 
    top_n = 20,                 # 想画的 Top N
    mode = c("split","abs", "pos", "neg"), # 选边策略
    unify_y = TRUE,             # 是否统一 y 轴
    y_limits = NULL,            # 自定义 y 轴范围（优先级最高），例如 c(-1, 1)
    main_prefix = "TopN边的95%CI - ", # 每个小图标题前缀
    subtitle   = "绝对边权TopN的Bootstrap 95%置信区间",
    font_family = "myfont",
    save_pdf = NULL,            # e.g. "plot/Top20_CI_conditions.pdf"
    width_per_col = 16,         # 每列宽度（英寸）
    height_per_row = 8          # 每行高度（英寸）
) {
  mode <- match.arg(mode)
  G <- length(bootstrap_results_list)
  if (G == 0) stop("bootstrap_results_list 为空。")
  
  # 条件标签
  if (is.null(condition_labels)) condition_labels <- paste0("Cond", seq_len(G))
  
  # 逐条件生成图与原始统计，用于后续统一坐标
  plot_list <- vector("list", G)
  edge_tables <- vector("list", G)
  # 预计算各条件的全局 CI 范围，给统一 y 用
  ci_min_vec <- numeric(G)
  ci_max_vec <- numeric(G)
  
  for (i in seq_len(G)) {
    res_obj  <- bootstrap_results_list[[i]]
    mgm_fit_i <- if (!is.null(cond_list)) cond_list[[i]] else NULL
    title_i   <- paste0(main_prefix, condition_labels[i])
    
    # 先估一个全局 y 范围（若未显式传入 y_limits）
    if (!is.null(res_obj$bootQuantiles)) {
      ci_min_vec[i] <- min(res_obj$bootQuantiles, na.rm = TRUE)
      ci_max_vec[i] <- max(res_obj$bootQuantiles, na.rm = TRUE)
    } else {
      # 没有 bootQuantiles 时，粗略从 bootParameters 估一个范围
      ci_min_vec[i] <- quantile(res_obj$bootParameters, probs = 0.025, na.rm = TRUE)
      ci_max_vec[i] <- quantile(res_obj$bootParameters, probs = 0.975, na.rm = TRUE)
    }
    
    # 调用你的新绘图函数（吃 resample 对象）
    res_i <- plot_topN_bootstrap_edges(
      res_obj    = res_obj,
      mgm_fit    = mgm_fit_i,
      var_names  = var_names,
      top_n      = top_n,
      mode       = mode,
      main_title = title_i,
      subtitle   = subtitle,
      font_family = font_family
    )
    plot_list[[i]]    <- res_i$plot
    edge_tables[[i]]  <- res_i$edge_table_top
  }
  
  # 统一 y 轴
  if (unify_y) {
    if (is.null(y_limits)) {
      # 取所有条件的全局范围，再稍微加点边际
      y_min <- min(ci_min_vec, na.rm = TRUE)
      y_max <- max(ci_max_vec, na.rm = TRUE)
      pad <- 0.05 * (y_max - y_min + 1e-8)
      y_limits <- c(y_min - pad, y_max + pad)
    }
    plot_list <- lapply(plot_list, function(p) p + scale_y_continuous(limits = y_limits))
  }
  
  # 自适应拼图布局：
  # ≤3：横排；>3：尽量方阵（ncol = ceiling(sqrt(G)))
  if (G <= 3) {
    ncol <- G; nrow <- 1
  } else {
    ncol <- ceiling(sqrt(G))
    nrow <- ceiling(G / ncol)
  }
  
  # 用 patchwork 拼接
  wrap_list <- wrap_plots(plot_list, ncol = ncol, nrow = nrow)
  # 加总标题（可按需外部再包一层）
  combined_plot <- wrap_list + plot_layout(ncol = ncol, nrow = nrow) +
    plot_annotation(
      title = paste0("各条件网络的Top", top_n, "边 Bootstrap 置信区间对比"),
      theme = theme(
        plot.title = element_text(family = font_family, face = "bold", hjust = 0.5, size = 14)
      )
    )
  
  print(combined_plot)
  
  # 保存 PDF
  if (!is.null(save_pdf)) {
    # 确保目录存在
    dir.create(dirname(save_pdf), showWarnings = FALSE, recursive = TRUE)
    total_width  <- width_per_col * ncol
    total_height <- height_per_row * nrow
    ggsave(save_pdf, combined_plot, width = total_width, height = total_height, dpi = 300)
    message("已保存：", save_pdf)
  }
  
  invisible(list(
    plots = plot_list,
    tables = edge_tables,
    combined_plot = combined_plot,
    layout = list(nrow = nrow, ncol = ncol, y_limits = y_limits)
  ))
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# 函数构建：一键两两NCT————————————————————————————————————————————————————————
# 输出结果：整体网络结构差异、总体连通性差异、单边显著性检验
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
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

run_nct_pairs <- function(data_subsets,
                          vars_for_NCT,
                          gamma = 0.5,
                          it = 100,
                          alpha = 0.05,
                          test.centrality = FALSE,
                          paired = FALSE,
                          progressbar = FALSE
) {
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
      paired = paired,
      progressbar = progressbar   # 关闭进度条显示
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

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：一个数据集过采样函数（简单复制）
#受不了了bootstrap总是报错，原来是数据集里面有一些分类变量的水平过少，采样采不到
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

# data_list : 由多个数据框组成的列表
# cat_vars  : 需要检查/过采样的分类变量名向量
# min_count : 每个水平至少保留的行数

oversample_rare_levels_list <- function(data_list, cat_vars, min_count = 5) {
  lapply(data_list, function(mat) {
    res <- mat
    for (v in cat_vars) {
      col_vals <- res[, v]
      counts <- table(col_vals)                        # 统计各水平数
      rare_lvls <- as.numeric(names(counts[counts < min_count]))
      if (length(rare_lvls) > 0) {
        dup_rows <- lapply(rare_lvls, function(lvl) {
          idx <- which(res[, v] == lvl)
          need <- min_count - length(idx)              # 还需补多少
          res[sample(idx, need, replace = TRUE), , drop = FALSE]
        })
        res <- rbind(res, do.call(rbind, dup_rows))    # 添加补充行
      }
    }
    rownames(res) <- NULL
    res
  })
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：对nct结果进行绘图（绘制边权差异网络）————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# nct_results: run_nct_pairs(...) 返回的 $nct_results 列表
# edge_differences: run_nct_pairs(...) 返回的 $edge_differences 列表
# output_dir: 输出 PDF 所在目录

plot_nct_pairs <- function(
    nct_results, 
    edge_differences, 
    vars_name,
    output_dir = "plot"
    ) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (pair in names(nct_results)) {
    res <- nct_results[[pair]]
    diff_mat <- res$nw1 - res$nw2
    
    edge_df <- edge_differences[[pair]]
    edge_colors <- matrix("grey80", nrow(diff_mat), ncol(diff_mat))
    if (!is.null(edge_df) && nrow(edge_df) > 0) {
      for (i in seq_len(nrow(edge_df))) {
        v1 <- edge_df$Var1[i]
        v2 <- edge_df$Var2[i]
        edge_colors[v1, v2] <- edge_colors[v2, v1] <- "red"
      }
    }
    
    pdf(file.path(output_dir, paste0(pair, "_diff_network.pdf")))
    qgraph(diff_mat,
           layout      = "spring",
           labels      = vars_name,
           edge.color  = edge_colors,
           edge.labels = TRUE,
           label.cex   = 1.1,
           title       = paste("Difference Network:", pair))
    dev.off()
    cat (paste0(pair,"的差异网络以存储为./plot/", pair,"_diff_network.pdf\n"))
    qgraph(diff_mat,
           layout      = "spring",
           labels      = vars_name,
           edge.color  = edge_colors,
           edge.labels = TRUE,
           label.cex   = 1.1,
           title       = paste("Difference Network:", pair))
  }
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#函数构建：对不同条件下节点BEI差异的显著性检验————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

pairwise_BEI_ttest <- function(BEI_for_conditions,
                               adjust_method = "bonferroni",
                               paired        = TRUE,
                               var.equal     = TRUE,
                               min_n         = 2) {
  
  BEI_for_conditions %>%
    mutate(
      Node      = factor(Node),
      Group     = factor(Group),
      Condition = factor(Condition)
    ) %>%
    group_by(Node, Condition) %>%
    mutate(n_obs = n()) %>%                 # 统计样本量
    ungroup() %>%
    group_by(Node) %>%
    group_modify(~{
      if (any(.x$n_obs < min_n)) {          # 若存在样本数不足的条件，跳过
        return(tibble(
          Level1 = character(),
          Level2 = character(),
          p_adj  = NA_real_,
          note   = "样本数不足，未执行 t 检验"
        ))
      }
      
      # 所有条件水平组合
      cond_levels <- levels(.x$Condition)
      combs <- combn(cond_levels, 2, simplify = FALSE)
      
      # 对每一对条件运行 bruceR::TTEST
      res <- map_dfr(combs, function(cc) {
        dat_pair <- filter(.x, Condition %in% cc)
        t_res <- TTEST(dat_pair,
                       y         = "BEI",
                       x         = "Condition",
                       paired    = paired,
                       var.equal = var.equal,
                       factor.rev = FALSE)
        tibble(
          Level1 = cc[1],
          Level2 = cc[2],
          p      = t_res$p
        )
      })
      
      # 多重比较校正
      res %>%
        mutate(p_adj = p.adjust(p, method = adjust_method)) %>%
        select(Level1, Level2, p_adj)
    }) %>%
    ungroup()
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


#函数：删无限值


clean_xlsx <- function(path, sheet = 1) {
  # 读取指定工作表
  df <- read_xlsx(path, sheet = sheet)
  
  # 选出数值型列，用于检查无限值
  num_cols <- sapply(df, is.numeric)
  
  # 删除包含 NA 或 ±Inf 的行
  df_clean <- df[
    complete.cases(df) &              # 去掉含 NA 的行
      apply(df[, num_cols, drop = FALSE], 1, function(r) all(is.finite(r))), # 去掉含 ±Inf 的行
    , drop = FALSE
  ]
  
  # 覆写原文件（仅保留清理后的数据；如需保留多表格，可先备份）
  write_xlsx(df_clean, path)
}

#————

#—————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#已遗弃
#函数构建：文本输出三阶交互大小（调节作用）及各condition下对应边权——————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

summarize_moderation_text <- function(
    mgm_fit,
    data_mat,
    moderation_df,         # data.frame: Var1, Var2, Weight, (可选) Sign
    var_names,             # 含所有变量中文名/标签
    moderator_index = 1,   # 调节变量列号（默认第1列）
    moderator_name  = NULL,# 若不传，则用 var_names[moderator_index]
    condition_values = NULL,       # 如 c(1,2,3)；缺省则从 data_mat[, moderator_index] 唯一值取
    condition_labels = NULL,       # 如 c("Level 1","Level 2","Level 3")
    cond_list = NULL,              # 可选：已预先计算好的 condition() 列表；传了就不再计算
    digits = 3,                    # 小数位
    top_k = NULL                   # 可选：只汇报 |Weight| 前K条调节作用
) {
  stopifnot(is.matrix(data_mat) || is.data.frame(data_mat))
  if (is.null(moderator_name)) moderator_name <- var_names[moderator_index]
  
  # —— 0) 整理 moderation_df（允许 Var1/Var2 是名或索引） ——
  idx_of <- function(v) if (is.numeric(v)) as.integer(v) else match(v, var_names)
  if (!all(c("Var1","Var2","Weight") %in% names(moderation_df))) {
    stop("moderation_df 需包含列: Var1, Var2, Weight，（可选）Sign")
  }
  moderation_df$Var1_idx <- idx_of(moderation_df$Var1)
  moderation_df$Var2_idx <- idx_of(moderation_df$Var2)
  if (anyNA(moderation_df$Var1_idx) || anyNA(moderation_df$Var2_idx)) {
    stop("moderation_df$Var1 / Var2 无法在 var_names 中匹配。")
  }
  # 可选：只保留|Weight|前K条
  if (!is.null(top_k)) {
    ord <- order(abs(moderation_df$Weight), decreasing = TRUE)
    moderation_df <- moderation_df[ord, , drop = FALSE]
    moderation_df <- head(moderation_df, top_k)
  }
  
  # —— 1) 准备条件水平与标签 ——
  if (is.null(condition_values)) {
    condition_values <- sort(unique(as.integer(data_mat[, moderator_index])))
  }
  if (is.null(condition_labels)) {
    condition_labels <- paste0("Level ", condition_values)
  }
  if (length(condition_labels) != length(condition_values)) {
    stop("condition_labels 与 condition_values 长度不一致。")
  }
  
  # —— 2) condition()：如未提供 cond_list，则现算 ——
  if (is.null(cond_list)) {
    cond_list <- vector("list", length(condition_values))
    for (i in seq_along(condition_values)) {
      val <- condition_values[i]
      cond_list[[i]] <- mgm::condition(
        object = mgm_fit,
        values = setNames(list(val), as.character(moderator_index))
      )
    }
  }
  
  # —— 3) 提取每个条件下 A–B 的带符号边权，并逐条打印 ——
  cat("\n================ 调节作用文字描述 ================\n")
  res_rows <- list()
  
  for (r in seq_len(nrow(moderation_df))) {
    i <- moderation_df$Var1_idx[r]
    j <- moderation_df$Var2_idx[r]
    A <- var_names[i]
    B <- var_names[j]
    mod_w <- moderation_df$Weight[r]
    
    # 每个条件的带符号边权
    cond_weights <- numeric(length(cond_list))
    for (cidx in seq_along(cond_list)) {
      wadj  <- cond_list[[cidx]]$pairwise$wadj
      signs <- cond_list[[cidx]]$pairwise$signs
      w_ij  <- wadj[i, j]
      s_ij  <- signs[i, j]
      # 如果 sign 缺失或为0，按无符号处理；你也可以改成 NA
      signed_w <- ifelse(is.na(s_ij) || s_ij == 0, NA_real_, w_ij * s_ij)
      # 若 signs 缺失但 w 非零，你也可以直接用 w_ij（按需替换）
      if (is.na(signed_w) && !is.na(w_ij) && w_ij != 0) signed_w <- w_ij
      cond_weights[cidx] <- signed_w
    }
    
    # —— 4) 文字框输出 ——
    cat(sprintf("\n【%s】调节了：%s 与 %s 的关系；调节效应 = %.*f\n",
                moderator_name, A, B, digits, mod_w))
    for (cidx in seq_along(condition_values)) {
      val_lab <- condition_labels[cidx]
      wlab    <- ifelse(is.na(cond_weights[cidx]), "NA", sprintf("%.*f", digits, cond_weights[cidx]))
      cat(sprintf(" - 在 %s 中：%s 与 %s 的边权 = %s\n", val_lab, A, B, wlab))
    }
    
    # 存到结果表
    row_list <- list(
      Moderator      = moderator_name,
      Var1           = A,
      Var2           = B,
      ModEffect      = round(mod_w, digits)
    )
    # 展开每个条件的边权
    for (cidx in seq_along(condition_labels)) {
      nm <- paste0("Edge_", condition_labels[cidx])
      row_list[[nm]] <- ifelse(is.na(cond_weights[cidx]),
                               NA_real_, round(cond_weights[cidx], digits))
    }
    res_rows[[r]] <- row_list
  }
  
  # —— 5) 返回整洁结果表 ——
  res_df <- do.call(rbind, lapply(res_rows, as.data.frame))
  rownames(res_df) <- NULL
  
  cat("=================================================\n\n")
  return(invisible(list(
    table = res_df,
    condition_values = condition_values,
    condition_labels = condition_labels
  )))
}

#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

