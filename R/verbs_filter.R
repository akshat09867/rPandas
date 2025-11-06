#' Filter rows using pandas
#'
#' @description
#' Filters a data frame using an R expression translated to pandas.
#' @param .data An R data.frame or tibble.
#' @param filter_expression The filtering expression, written in R syntax.
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` containing the filtered rows.
#' @export
#' @examples
#' # Only run the example when the reticulate package is installed
#' if (requireNamespace("reticulate", quietly = TRUE) &&
#'     reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   rp_filter(ggplot2::diamonds, carat > 1 & price < 4000)
#' }

rp_filter <- function(.data, filter_expression, return.as = "result") {
  filter_expr <- substitute(filter_expression)

  filter_str <- translate_filter(!!filter_expr)
  print(filter_str)
  command_str <- create_pandas_statement("df", filter_str = filter_str)
  

  execute_pandas_statement(r_df = .data, py_command = command_str, return.as)
}

#'Filtering columns
#' @description
#' Selects specific columns from a data frame. It captures the bare column
#' names and translates the operation into a pandas selection command.
#'
#' @param .data An R data.frame or tibble.
#' @param ... The bare column names to select (e.g., `carat, cut, price`).
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` containing only the selected columns.
#'
#' @export
#' @examples
#' if (reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   rp_select(ggplot2::diamonds, carat, cut, price)
#' }
rp_select <- function(.data, ..., return.as='result'){
  rp_check_env()
 select_c <- translate_select(...)
 command <- create_pandas_statement("df", select_str= select_c)
 execute_pandas_statement(.data, command,return.as)
}


#' Sort rows of a data frame using pandas
#'
#' @description
#' Sorts a data frame by one or more columns. It translates the R expressions
#' into a pandas `.sort_values()` command and executes it.
#'
#' @param .data An R data.frame or tibble.
#' @param ... Bare column names to sort by. Use `desc(colname)` to sort
#'   in descending order (e.g., `cut, desc(price)`).
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` sorted by the specified columns.
#'
#' @export
#' @examples
#' if (reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   
#'   # Sort by cut (ascending) and price (descending)
#'   rp_sort(ggplot2::diamonds, cut, desc(price))
#' }
rp_sort <- function(.data, ...,return.as='result') {
  rp_check_env()
  
  sort_params <- translate_sort(...)
  
  command_str <- create_pandas_statement(
    "df", 
    sort_by_str = sort_params$by, 
    sort_asc_str = sort_params$ascending
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str,return.as)
}


#' Create or modify columns using pandas
#'
#' @description
#' Creates new columns or modifies existing ones by translating R expressions
#' into a pandas `.assign()` command.
#'
#' @param .data An R data.frame or tibble.
#' @param ... Named R expressions (e.g., `price_per_carat = price / carat`).
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` with the new or modified columns.
#' @export
rp_mutate <- function(.data, ...,return.as='result') {
  rp_check_env()
  
  assign_str <- translate_mutate(...)
  
  command_str <- create_pandas_statement(
    "df", 
    assing_str = assign_str
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str,return.as)
}


#' Summarize data using pandas
#'
#' @description
#' Aggregates a data frame by one or more groups, applying summary functions.
#' It translates R's `dplyr::summarise` syntax into a pandas `.groupby().agg()`
#' command.
#'
#' @param .data An R data.frame or tibble.
#' @param ... Named summary expressions (e.g., `avg_price = mean(price)`).
#'   Supports `mean`, `median`, `sd`, `var`, `min`, `max`, `sum`, and `n()`.
#' @param .by A bare column name or `c(col1, col2)` to group by.
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` with the summarized and grouped data.
#'
#' @export
#' @examples
#' if (reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   
#'   # Summarize by one group
#'   rp_summarize(ggplot2::diamonds, 
#'                avg_price = mean(price), 
#'                .by = cut)
#'   
#'   # Summarize by multiple groups and multiple functions
#'   rp_summarize(ggplot2::diamonds, 
#'                avg_price = mean(price), 
#'                count = n(),
#'                .by = c(cut, color))
#' }
rp_summarize <- function(.data, ..., .by = NULL,return.as='result') {
  rp_check_env()
  
  groupby_str <- translate_groupby(rlang::enquo(.by))
  
  agg_str <- translate_summarize(...)
  
  command_str <- create_pandas_statement(
    "df", 
    groupby_str = groupby_str,
    agg_str = agg_str
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str,return.as)
}



#' Apply multiple summary functions to multiple columns
#'
#' @description
#' Applies a list of summary functions to a list of columns, after
#' optionally grouping the data.
#'
#' @param .data An R data.frame.
#' @param ... Bare column names to summarize (e.g., `price, carat`).
#' @param the.functions A character vector of R function names
#'   (e.g., `c("mean", "sd")`). Supports "mean", "median", "sd",
#'   "var", "min", "max", "sum".
#' @param .by A bare column name or `c(col1, col2)` to group by.
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` with the summarized and grouped data.
#' @export
#' @examples
#' if (reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'
#'   rp_calculate(
#'     ggplot2::diamonds,
#'     price, carat,
#'     the.functions = c("mean", "sd"),
#'     .by = cut
#'   )
#' }
rp_calculate <- function(.data, ..., the.functions, .by = NULL,return.as='result') {
  rp_check_env()
  
  groupby_str <- translate_groupby(rlang::enquo(.by))
  
  variable_exprs <- rlang::enquos(...)
  if (rlang::is_empty(variable_exprs)) {
    stop("At least one variable (...) must be provided.", call. = FALSE)
  }
  if (missing(the.functions) || !is.character(the.functions)) {
    stop("`the.functions` must be a character vector.", call. = FALSE)
  }
  
  agg_str <- translate_calculate(variable_exprs, the.functions)
  
  command_str <- create_pandas_statement(
    "df", 
    groupby_str = groupby_str,
    agg_str = agg_str
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str,return.as)
}