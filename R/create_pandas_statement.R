#' Create a chained pandas command string
#'
#' @description
#' This internal function assembles a Python command string for pandas by
#' chaining together different data manipulation methods. It serves as the
#' central command generator for the package.
#'
#' @param df_name A character string for the name of the pandas DataFrame.
#' @param filter_str A string for the `.query()` method.
#' @param select_str A string for column selection (e.g., \code{['col1', 'col2']}).
#' @param sort_by_str A string for the `by` argument of `.sort_values()`.
#' @param sort_asc_str A string for the `ascending` argument of `.sort_values()`.
#' @param assign_str A string for the `.assign()` method.
#' @param drop_str A string for the `.drop()` method.
#' @param groupby_str A string for the `.groupby()` method.
#' @param agg_str A string for the `.agg()` method.
#' @param head_k Integer for `.head(k)`.
#' @param tail_k Integer for `.tail(k)`.
#' @return A character string of the complete, chained pandas command.
#' @keywords internal
create_pandas_statement <- function(df_name, filter_str= NULL, select_str= NULL, sort_by_str=NULL,sort_asc_str=NULL, assign_str=NULL,drop_str=NULL,groupby_str = NULL, agg_str = NULL, head_k=NULL, tail_k=NULL) {
  command <- df_name
  escaped_filter_str <- gsub("'", "\\\"", filter_str, fixed = TRUE)
  if (!is.null(filter_str) && nchar(filter_str) > 0) {
  command <- sprintf("%s.query('%s')", command, escaped_filter_str)
  }
  if(!is.null(select_str) && nchar(select_str)>0){
    command <- paste0(command, sprintf("[%s]",select_str))
  }
  if (!is.null(sort_by_str) && nchar(sort_by_str) > 0) {
    command <- paste0(command, sprintf('.sort_values(by=%s, ascending=%s)', sort_by_str, sort_asc_str))
  }
   
  if(!is.null(assign_str) && nchar(assign_str)>0){
    command <- paste0(command, sprintf(".assign(%s)", assign_str))
  }
 if(!is.null(drop_str) && nchar(drop_str)>0){
    command <- paste0(command, drop_str)
  }
  if (!is.null(groupby_str) && nchar(groupby_str) > 0) {
    command <- paste0(command, groupby_str)
  }

    if (!is.null(agg_str) && nchar(agg_str) > 0) {
    command <- paste0(command, agg_str)
  }

  if (!is.null(head_k) && !is.null(tail_k)) {
    stop("Only one of head_k or tail_k can be provided.")
  }
  if(!is.null(head_k) && is.numeric(head_k) && head_k>=0){
    command <- paste0(command, sprintf(".head(%s)", head_k))
  }
  if(!is.null(tail_k) && is.numeric(tail_k) && tail_k >=0){
    command <- paste0(command, sprintf(".tail(%s)", tail_k))
  }
  return(command)
}
