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
#'
#' @return A `data.frame` containing only the selected columns.
#'
#' @export
#' @examples
#' if (reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   rp_select(ggplot2::diamonds, carat, cut, price)
#' }
rp_select <- function(.data, ...){
  rp_check_env()
 select_c <- translate_select(...)
 command <- create_pandas_statement("df", select_str= select_c)
 execute_pandas_statement(.data, command)
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
#'
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
rp_sort <- function(.data, ...) {
  rp_check_env()
  
  sort_params <- translate_sort(...)
  
  command_str <- create_pandas_statement(
    "df", 
    sort_by_str = sort_params$by, 
    sort_asc_str = sort_params$ascending
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str)
}


#' Create or modify columns using pandas
#'
#' @description
#' Creates new columns or modifies existing ones by translating R expressions
#' into a pandas `.assign()` command.
#'
#' @param .data An R data.frame or tibble.
#' @param ... Named R expressions (e.g., `price_per_carat = price / carat`).
#'
#' @return A `data.frame` with the new or modified columns.
#' @export
rp_mutate <- function(.data, ...) {
  rp_check_env()
  
  assign_str <- translate_mutate(...)
  
  command_str <- create_pandas_statement(
    "df", 
    assing_str = assign_str
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str)
}