# University_Network_Analysis_R

its a R project on the moderation in learning habits and mental health among university students analysis

## Supporting Functions / 支持函数

The repository includes `Supporting_Function.R` with custom functions for network analysis. Below is a bilingual summary of these helpers.

### `plot_topN_bootstrap_edges`
- **EN**: Visualize the top N edges from mgm resample results with confidence intervals; supports filtering by absolute value or sign.
- **中文**：对 mgm 重采样结果的边按绝对值或符号筛选 Top N，并以置信区间形式展示。

### `community_detection_and_plot`
- **EN**: Perform Louvain community detection on weighted graphs and plot the network with an optional node–variable lookup table.
- **中文**：使用 Louvain 算法在加权图中发现社群并绘图，可输出节点与变量对照表。

### `compute_BEI_for_network` / `plot_BEI_results` / `compute_and_plot_BEI`
- **EN**: Chain of functions to compute Bridge Expected Influence (BEI) for networks and visualize results across conditions.
- **中文**：计算网络中每个节点的桥接预期影响力（BEI），并在多个条件下统一绘图。

### `plot_moderation_qgraph`
- **EN**: Create qgraph plots highlighting three-way interactions by adding moderator nodes; supports top-K filtering and custom styling.
- **中文**：在传统 pairwise 图上添加调节节点以展示三阶交互，可筛选权重最大的前 K 个调节作用并自定义样式。

### `make_moderation_df`
- **EN**: Extract specified three-way interactions from mgm fits into a tidy data frame for downstream visualization or summary.
- **中文**：从 mgm 拟合对象中提取含指定调节变量的三阶交互，生成整洁数据框以便后续使用。

### `summarize_moderation_text`
- **EN**: Produce textual summaries of moderation effects, listing effect sizes and condition-specific edge weights.
- **中文**：以文字方式汇总调节效应，列出效应值及不同条件下对应的边权。

### `edge_difference_test`
- **EN**: Compare bootstrap edge weights between conditions, returning differences and confidence intervals with optional Top-K filtering.
- **中文**：比较不同条件的 bootstrap 边权，提供差值及置信区间，可按 Top K 进行筛选。

### `t_test_edge_diff`
- **EN**: Conduct independent-sample t-tests on edge weights between two conditions and rank by significance.
- **中文**：对两条件的边权做独立样本 t 检验，并按显著性排序。

### `run_mgm_per_condition`
- **EN**: Fit mgm models separately for each level of a moderator, removing the moderator variable and handling type/level inference.
- **中文**：按调节变量水平拆分数据并分别拟合 mgm 模型，可自动推断变量类型和水平。

### `run_bootstrap_for_conditions`
- **EN**: Execute bootstrap for each condition’s mgm network, extracting significant edges and confidence intervals.
- **中文**：对每个条件的 mgm 网络进行 bootstrap，提取显著边及其置信区间。

### `run_mgm_and_bootstrap`
- **EN**: Wrapper that combines condition-wise mgm fitting and bootstrap into a single workflow.
- **中文**：封装 mgm 拟合与 bootstrap，以一键完成分条件网络估计和重采样。

### `plot_conditions_topN`
- **EN**: Generate unified plots of top N bootstrap edges for multiple conditions with optional shared y-axis.
- **中文**：批量绘制多条件的 Top N CI 图，可选择统一 y 轴。

### `multi_NCT_compare`
- **EN**: Perform pairwise Network Comparison Tests across multiple conditions and summarize global and edge-wise differences.
- **中文**：对多个条件执行两两网络比较检验，汇总全局和单边差异并可输出 Top K 摘要。函数会自动剔除在任一条件下方差为 0 或有效观测不足的变量，以避免比较时的数值问题。
