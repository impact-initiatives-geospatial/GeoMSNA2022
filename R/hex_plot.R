#' hex_plot_mean_rs_mean_msna_ind
#' Creates a scaterplot of rs indicators added to the data and other numeric indicators
#' @param input_dataset 
#' @param group_by_col 
#' @param input_x_col_rs 
#' @param input_y_col 
#'
#' @return
#' @export
#'
#' @examples
hex_plot_mean_rs_mean_msna_ind <- function(input_dataset, group_by_col, 
                                           input_x_col_rs, input_y_col) {
  df_data_process <- input_dataset |> 
    group_by(!!sym(group_by_col)) |> 
    summarise({{input_x_col_rs}} := mean(!!sym(input_x_col_rs), na.rm = TRUE),
              {{input_y_col}} := mean(!!sym(input_y_col), na.rm = TRUE))
  
  ggplot2::ggplot(data = df_data_process, aes(x = !!sym(input_x_col_rs), y = !!sym(input_y_col))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method=lm) +
    ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 1) +
    ggplot2::labs(title = paste("Plot of", input_x_col_rs ,"and", input_y_col))
}

#' hex_plot_mean_rs_count_msna_ind
#' Creates a scaterplot of rs indicators added to the data and other categorical indicators
#' @param input_dataset 
#' @param group_by_col 
#' @param input_x_col_rs 
#' @param input_y_col 
#' @param search_y_col 
#'
#' @return
#' @export
#'
#' @examples
hex_plot_mean_rs_count_msna_ind <- function(input_dataset, group_by_col, 
                                            input_x_col_rs, input_y_col, 
                                            search_y_col = c("yes")) {
  df_data_process <- input_dataset |> 
    group_by(!!sym(group_by_col)) |> 
    summarise({{input_x_col_rs}} := mean(!!sym(input_x_col_rs), na.rm = TRUE),
              {{input_y_col}} := sum(!!sym(input_y_col) %in% search_y_col, na.rm = TRUE))
  
  ggplot2::ggplot(data = df_data_process, aes(x = !!sym(input_x_col_rs), y = !!sym(input_y_col))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method=lm) +
    ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 1) +
    ggplot2::labs(title = paste("Plot of", input_x_col_rs ,"and", input_y_col))
}