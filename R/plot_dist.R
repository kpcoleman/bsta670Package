#' Plot Histogram
#'
#' Plot the histogram of a numeric variable within a data frame
#'
#' @param dat a data.frame containing the data to plot.
#' @param var_name a character vector of the column name in the data.
#'
#' @return a histogram plot.
#' @import ggplot2
#'
#' @examples
#' plot_dist(dat = iris, var_name = "Petal.Length")
#'
#' @export


plot_dist <- function(dat, var_name){
  stopifnot(var_name %in% colnames(dat))
  stopifnot(class(dat[,var_name]) == "numeric")
  
  p <- ggplot2::ggplot(dat) +
    ggplot2::geom_histogram(ggplot2::aes_string(x = var_name), bins = 30) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Count", x = var_name)
  
  return(p)
}
