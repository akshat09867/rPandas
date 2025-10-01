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
create_pandas_statement <- function(df_name, filter_str = NULL, ...) {
  
  # Start with the base DataFrame name
  command <- df_name
  
  # Add a filter/query step if provided
  if (!is.null(filter_str) && nchar(filter_str) > 0) {
    # CORRECTED: Use double quotes to wrap the query. This prevents
    # conflicts with single quotes used for strings inside the query.
    query_step <- sprintf('.query("%s")', filter_str)
    command <- paste0(command, query_step)
  }
  
  # Future enhancements for select, groupby, etc. will go here.
  
  return(command)
}