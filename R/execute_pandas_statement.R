#' Executes a pandas command string on an R data frame.
#'
#' @description
#' This internal function serves as the execution engine for the rPandas package.
#' It uses the reticulate package to pass an R data frame to a Python session,
#' execute a command on it using pandas, and then retrieve the result back into R.
#'
#' @param r_df An R data.frame or data.table to be used in the Python command.
#' @param py_command A character string containing the Python code to execute.
#'        This string should use the placeholder 'df' to refer to the data frame.
#'        For example: "df.head(5)" or "df.groupby('Species').size()".
#' @param return.as A character string specifying what to return.
#'        - "result": (Default) Returns the resulting R object (e.g., a data frame).
#'        - "code": Returns the generated Python code string.
#'        - "all": Returns a list containing both the result and the code.
#' @return The object specified by the return.as parameter.
#' @keywords internal
execute_pandas_statement <- function(r_df, py_command, return.as = "result") {
  
  if (!return.as %in% c("result", "code", "all")) {
    stop("`return.as` must be one of 'result', 'code', or 'all'.", call. = FALSE)
  }
  
  py_df_name <- "r.r_df"
  full_py_command <- gsub("\\bdf\\b", py_df_name, py_command, perl = TRUE)
  
  if (return.as == "code") {
    return(full_py_command)
  }
  

  if (!reticulate::py_module_available("pandas")) {
    stop("The 'pandas' Python module is required. Please install it.", call. = FALSE)
  }
  
  py_script <- sprintf("
import pandas as pd
py_result = %s
", full_py_command)
  
  reticulate::py_run_string(py_script, local = FALSE)
  
  result_from_py <- reticulate::py$py_result
  
  if (return.as == "result") {
    return(result_from_py)
  }
  
  if (return.as == "all") {
    return(list(
      result = result_from_py,
      code = full_py_command
    ))
  }
}