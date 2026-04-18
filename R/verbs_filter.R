#' Filter rows using pandas
#'
#' @description
#' Filters a data frame using an R expression translated to pandas.
#' @param .data An R data.frame or tibble.
#' @param filter_expression The filtering expression, written in R syntax.
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
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

rp_filter <- function(.data, filter_expression, table_name = NULL,return.as = "result") {
  filter_expr <- substitute(filter_expression)

  filter_str <- translate_filter(!!filter_expr)
  command_str <- create_pandas_statement("df", filter_str = filter_str)
  

  execute_pandas_statement(r_df = .data, py_command = command_str, table_name = table_name,return.as)
}

#'Filtering columns
#' @description
#' Selects specific columns from a data frame. It captures the bare column
#' names and translates the operation into a pandas selection command.
#'
#' @param .data An R data.frame or tibble.
#' @param ... The bare column names to select (e.g., `carat, cut, price`).
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
#' @param return.as What to return: "result", "code", or "all".
#' @return A `data.frame` containing only the selected columns.
#'
#' @export
#' @examples
#' if (reticulate::py_available(initialize = TRUE) &&
#'     reticulate::py_module_available("pandas")) {
#'   rp_select(ggplot2::diamonds, carat, cut, price)
#' }
rp_select <- function(.data, ..., table_name=NULL,return.as='result'){
  rp_check_env()
 select_c <- translate_select(...)
 command <- create_pandas_statement("df", select_str= select_c)
 execute_pandas_statement(.data, command, table_name = table_name,return.as)
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
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
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
rp_sort <- function(.data, ...,table_name=NULL,return.as='result') {
  rp_check_env()
  
  sort_params <- translate_sort(...)
  
  command_str <- create_pandas_statement(
    "df", 
    sort_by_str = sort_params$by, 
    sort_asc_str = sort_params$ascending
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str, table_name = table_name,return.as)
}


#' Mutate (add/modify/remove) columns using pandas
#'
#' @param .data An R data frame.
#' @param to_remove A character vector of column names to remove.
#' @param ... Named expressions for new/modified columns.
#' @param return.as Either "result", "code", or "all".
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
#' @return A data frame or list depending on return.as.
#' @export
rp_mutate <- function(.data, to_remove = NULL, ...,table_name=NULL, return.as = "result") {
  rp_check_env()
  
  trans <- translate_mutate(to_remove, ...)
    command_str <- create_pandas_statement(
    df_name = "df",
    assign_str = if (nzchar(trans$assign_str)) trans$assign_str else NULL,
    drop_str = trans$drop_str
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str, table_name = table_name, return.as = return.as)
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
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
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
rp_summarize <- function(.data, ..., .by = NULL,table_name=NULL,return.as='result') {
  rp_check_env()
  
  groupby_str <- translate_groupby(rlang::enquo(.by))
  
  agg_str <- translate_summarize(...)
  
  command_str <- create_pandas_statement(
    "df", 
    groupby_str = groupby_str,
    agg_str = agg_str
  )
  
  execute_pandas_statement(r_df = .data, py_command = command_str, table_name = table_name,return.as)
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
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
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
rp_calculate <- function(.data, ..., the.functions, .by = NULL,table_name=NULL,return.as='result') {
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
  
  execute_pandas_statement(r_df = .data, py_command = command_str, table_name = table_name, return.as)
}


#' Extract the first k rows of a data frame
#'
#' This function returns the first k rows of the data frame. If grouping variables
#' are provided via `.by`, it returns the first k rows within each group.
#'
#' @param .data An R data frame (or tibble) to be processed.
#' @param k An integer specifying the number of rows to return. If `.by` is used,
#'   returns up to k rows per group.
#' @param .by Optional grouping variables. Can be one or more unquoted column names.
#'   When provided, the operation is performed on each group separately.
#' @param return.as One of `"result"`, `"code"`, or `"all"`.
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
#' @return Depending on `return.as`: a data frame, a character string, or a list.
#' @export
rp_first_k_rows <- function(.data, k, .by=NULL,table_name=NULL, return.as= "result"){
    groupby_str <- translate_groupby(rlang::enquo(.by))
    f_expr <- create_pandas_statement("df", groupby_str = groupby_str, head_k = k)
  execute_pandas_statement(r_df = .data, py_command = f_expr, table_name = table_name, return.as)
}


#' Extract the last k rows of a data frame
#'
#' This function returns the last k rows of the data frame. If grouping variables
#' are provided via `.by`, it returns the last k rows within each group.
#'
#' @param .data An R data frame (or tibble) to be processed.
#' @param k An integer specifying the number of rows to return. If `.by` is used,
#'   returns up to k rows per group.
#' @param .by Optional grouping variables. Can be one or more unquoted column names.
#'   When provided, the operation is performed on each group separately.
#' @param return.as One of `"result"`, `"code"`, or `"all"`.
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
#' @return Depending on `return.as`: a data frame, a character string, or a list.
#' @export
rp_last_k_rows <- function(.data, k, .by=NULL,table_name=NULL, return.as="result"){
  groupby_str <- translate_groupby(rlang::enquo(.by))
  f_expr <- create_pandas_statement("df", groupby_str = groupby_str, tail_k = k)
  execute_pandas_statement(r_df = .data, py_command = f_expr, table_name = table_name, return.as)
}

#' Count rows in a data frame, optionally by groups
#'
#' This function returns the number of rows in a data frame. When grouping
#' variables are provided via `.by`, it returns the row counts for each group.
#'
#' @param .data An R data frame (or tibble) to be processed.
#' @param .by Optional grouping variables. Can be one or more unquoted column names
#'   (e.g., `cut` or `c(cut, color)`). When provided, counts are computed per group.
#' @param return.as One of `"result"`, `"code"`, or `"all"`.
#' @param table_name An optional character string. If provided, the generated Python code will replace the internal dataframe name with this string (e.g., \code{"diamonds.query(...)"}). This is useful for seeing the exact, copy-pasteable Python code. Defaults to \code{NULL} (uses \code{"df"}).
#' @return A data frame with one column `"n"` (total row count) if `.by = NULL`,
#'     or a data frame with the grouping columns and a column `"n"` (per‑group counts).
#' @export
rp_count <- function(.data, .by = NULL,table_name=NULL, return.as = "result"){
  by_quo <- rlang::enquo(.by)
  
  if (rlang::quo_is_missing(by_quo) || rlang::quo_is_null(by_quo)) {
    py_cmd <- "pd.DataFrame({'n': [len(df)]})"
    
  } else {
   by_expr <- rlang::quo_get_expr(by_quo)
        groupby_str <- translate_groupby(by_expr)
        py_cmd <- paste0("df", groupby_str, ".size().rename(columns={'size': 'n'})")
  }
  
  execute_pandas_statement(r_df = .data, py_command = py_cmd, table_name = table_name, return.as = return.as)
}