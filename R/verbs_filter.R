#' Filter rows using pandas
#'
#' @description
#' Filters a data frame using an R expression translated to pandas.
#' @param .data An R data.frame or tibble.
#' @param filter_expression The filtering expression, written in R syntax.
#' @return A `data.frame` containing the filtered rows.
#' @export
#' @examples
#' # Only run the example when the reticulate package is installed
#' if (requireNamespace("reticulate", quietly = TRUE) &&
#'     reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   rp_filter(ggplot2::diamonds, carat > 1 & price < 4000)
#' }

rp_filter <- function(.data, filter_expression) {
  filter_expr <- substitute(filter_expression)

  filter_str <- translate_filter(!!filter_expr)
  print(filter_str)
  command_str <- create_pandas_statement("df", filter_str = filter_str)
  

  execute_pandas_statement(r_df = .data, py_command = command_str, return.as = "result")
}