#' Create a chained pandas command string
#'
#' @description
#' This internal function assembles a Python command string for pandas by
#' chaining together different data manipulation methods. It serves as the
#' central command generator for the package.
#'
#' @param df_name A character string for the name of the pandas DataFrame.
#' @param filter_str A string for the `.query()` method.
#' @param select_str A string for column selection (e.g., "['col1', 'col2']").
#' @param sort_by_str A string for the `by` argument of `.sort_values()`.
#' @param sort_asc_str A string for the `ascending` argument of `.sort_values()`.
#' @return A character string of the complete, chained pandas command.
#' @keywords internal
create_pandas_statement <- function(df_name, filter_str= NULL, select_str= NULL, sort_by_str=NULL,sort_asc_str=NULL, assing_str=NULL) {
  escaped_filter_str <- gsub("\"", "\\\"", filter_str, fixed = TRUE)
  if (!is.null(filter_str) && nchar(filter_str) > 0) {
  command <- sprintf("%s.query(\"%s\")", df_name, escaped_filter_str)
  }
  if(!is.null(select_str) && nchar(select_str)>0){
    command <- paste0(df_name, sprintf("[%s]",select_str))
  }
  if (!is.null(sort_by_str) && nchar(sort_by_str) > 0) {
    command <- paste0(df_name, sprintf('.sort_values(by=%s, ascending=%s)', sort_by_str, sort_asc_str))
  }
  if(!is.null(assing_str) && nchar(assing_str)>0){
    command <- paste0(df_name, sprintf(".assign(%s)", assing_str))
  }
  return(command)
}
