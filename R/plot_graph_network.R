#' Plot Graph Network
#'
#' @param graph_df The dataframe containing the edge information and related attributes of the graph.
#' @param sendercolumn A character specifying the column name in the dataframe representing the sender.
#' @param receivercolumn A character specifying the column name in the dataframe representing the receiver.
#' @param widthcolumn A character specifying the column name in the dataframe representing the edge width.
#' @param threshold A numeric value specifying the threshold for filtering edge width.
#' @param colors A vector specifying the node colors and edge colors (optional).
#' @param useLabels A logical value indicating whether to display node labels (default is TRUE).
#' @param nodeSize A numeric value specifying the size of the nodes (default is 5).
#' @param vertex.label.color A character specifying the color of node labels.
#' @param edge.label.color A character specifying the color of edge labels.
#' @param edge.label.cex A numeric value specifying the font size multiplier for edge labels.
#' @param ... Other parameters passed to the plot() function.
#'
#' @return Returns the plotted graph object.
#' @export
#'
#' @examples
#' dataframe <- data.frame(
#' source = c("A", "A", "B", "B", "C", "C", "B", "C", "D", "E", "F", "D", "D", "F"),
#' target = c("B", "C", "D", "C", "E", "B", "A", "F", "A", "D", "E", "F", "B", "A"),
#' value = c(5, 10, 0.9, 9, 7, 6, 1, 3, 8, 3, 8, 14, 2, 5)
#' )
#' colors <- c(A = "#EE4000", B = "#0000CD", C = "#66CC00", D = rgb(102/255,46/255,115/255), E = rgb(31/255,153/255,139/255), F = "#003366")
#' png("plot_graph_network.png", width = 7, height = 7)
#' plot_graph_network(dataframe, sendercolumn = "source", receivercolumn = "target", widthcolumn = "value", useLabels = T, threshold = 0.01, colors = colors, nodeSize = 10, vertex.label.color = "white", edge.label.color = "black", edge.label.cex = 1.2)
#' dev.off()
plot_graph_network <- function(graph_df, sendercolumn, receivercolumn, widthcolumn, threshold = 0.01, colors = NULL, useLabels = TRUE, nodeSize = 5, vertex.label.color, edge.label.color, edge.label.cex, ...) {
  graph_df$sender <- as.character(graph_df[[sendercolumn]])
  graph_df$receiver <- as.character(graph_df[[receivercolumn]])
  graph_df$width <- graph_df[[widthcolumn]]

  vertices <- data.frame(id = unique(graph_df$sender), size = nodeSize,
                         stringsAsFactors = FALSE, label = unique(graph_df$sender))

  if (!is.null(colors)) {
    vertices$label.color <- colors[vertices$id]
    vertices$color <- colors[vertices$id]
    graph_df$color <- colors[graph_df$sender]
  }

  plot_graph <- igraph::graph_from_data_frame(subset(graph_df, !is.na(width) & width > threshold), vertices = vertices)

  z_order <- igraph::E(plot_graph)$width + 2.5  # 计算边的z轴顺序

  if (useLabels) {
    plot(plot_graph, vertex.frame.color = NA, edge.arrow.mode = "-", edge.curved = 0.3,
         edge.label = round(graph_df$width[!is.na(graph_df$width)], 2),
         edge.label.color = edge.label.color,   # 设置边标签颜色为黑色
         edge.label.cex = edge.label.cex,         # 设置边标签字体大小为1.2倍
         vertex.label = vertices$label,   # 显示节点标签
         vertex.label.color = vertex.label.color,       # 设置节点标签颜色为红色
         z = z_order, ...)
  } else {
    plot(plot_graph, vertex.frame.color = NA, edge.arrow.mode = "-", vertex.label = NA,
         edge.curved = 0.3, edge.label = round(graph_df$width[!is.na(graph_df$width)], 2),
         edge.label.color = edge.label.color,   # 设置边标签颜色为黑色
         edge.label.cex = edge.label.cex,         # 设置边标签字体大小为1.2倍
         vertex.label = vertices$label,   # 显示节点标签
         vertex.label.color = vertex.label.color,       # 设置节点标签颜色为红色
         z = z_order, ...)
  }
  invisible(plot_graph)
}
