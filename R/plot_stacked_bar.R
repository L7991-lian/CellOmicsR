#' plot_stacked_bar
#' @description plot stacked bar
#' @param data a dataframe
#' @param x_col charcater, column name for x group
#' @param stack_col charcater, column name for stack group
#' @param fill_col charcater, column name for fill group
#'
#' @return a ggplot object
#' @export
#'
#' @examples cell_data <- data.frame( Sample = rep(c("Sample1", "Sample2", "Sample3"), each = 4),Type = rep(c("Type1", "Type2", "Type3", "Type4"), 3),Count = c(10, 15, 8, 6, 5, 7, 12, 9, 3, 6, 4, 11))
# plot_stacked_bar(data = cell_data, x_col = Sample, stack_col = Count, fill_col = Type)

plot_stacked_bar <- function(data, x_col, stack_col, fill_col) {
  # 对数据进行整理和计算比例
  formatted_data <- data %>%
    group_by({{ x_col }}) %>%
    mutate(Total = sum({{ stack_col }})) %>%
    mutate(Percentage = {{ stack_col }} / Total * 100)

  # 配色方案
  colors <- c("#0072B2", "#009E73", "#F0E442", "#D55E00")

  # 绘制堆积柱形图
  p <- ggplot(formatted_data, aes({{ x_col }}, {{ stack_col }}, fill = {{ fill_col }}, group = {{ fill_col }})) +
    geom_bar(stat = "identity", position = "stack", color = "white") +
    scale_fill_manual(values = colors, guide = "none") +
    labs(x = as_label(enquo(x_col)), y = as_label(enquo(stack_col)), fill = "Cell Type") +
    theme_minimal()+
    theme(panel.grid = element_blank(),
          text = element_text(size = 18),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

  return(p)
}



