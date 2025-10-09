#' Create a chained pandas command string
#'
#' @description
#' This internal function assembles a Python command string for pandas by
#' chaining together different data manipulation methods. It serves as the

#' central command generator for the package.
#'
#' @param df_name A character string for the name of the pandas DataFrame object
#'   in the Python session (e.g., "py_df").
#' @param filter_str A character string containing a valid pandas query
#'   (e.g., from `translate_filter()`).
#' @param ... Placeholder for future arguments like select, group_by, etc.
#' @return A character string of the complete, chained pandas command.
#' @keywords internal
create_pandas_statement <- function(df_name, filter_str) {
  escaped_filter_str <- gsub("\"", "\\\"", filter_str, fixed = TRUE)
  sprintf("%s.query(\"%s\")", df_name, escaped_filter_str)
}
