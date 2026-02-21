#' Executes a pandas command string on an R data frame.
#'
#' This is the core execution engine. It explicitly injects an R data frame
#' into the Python session, runs a command, retrieves the result, and cleans up.
#'
#' @param r_df An R data.frame.
#' @param py_command A character string of Python code using 'df' as a placeholder.
#' @param return.as What to return: "result", "code", or "all".
#' @return The result of the execution.
#' @keywords internal
execute_pandas_statement <- function(r_df, py_command, return.as = "result") {
  rp_check_env()
  if (!return.as %in% c("result", "code", "all")) {
    stop("return.as must be 'result', 'code', or 'all'.", call. = FALSE)
  }
  
  reticulate::py_run_string("import pandas as pd")
  
  py_df_name <- "rpandas_df_in"
  py[[py_df_name]] <- r_df
  
  full_py_command <- gsub("\\bdf\\b", py_df_name, py_command, perl = TRUE)
  
  if (return.as == "code") {
    reticulate::py_run_string(paste("del", py_df_name))
    return(full_py_command)
  }
  
  py_script <- sprintf("
import pandas as pd
import numpy as np

rpandas_df_out = %s

if isinstance(rpandas_df_out, pd.DataFrame) and isinstance(rpandas_df_out.columns, pd.MultiIndex):
    rpandas_df_out.columns = ['.'.join(col).strip('.') for col in rpandas_df_out.columns.values]

if isinstance(rpandas_df_out.index, pd.MultiIndex):
    rpandas_df_out = rpandas_df_out.reset_index()
", full_py_command)

  reticulate::py_run_string(py_script)
  reticulate::py_run_string(paste("del", py_df_name))
  
  result_from_py <- reticulate::py$rpandas_df_out
  
  result_in_r <- tryCatch(
    reticulate::py_to_r(result_from_py),
    error = function(e) result_from_py  
  )
  
  if (inherits(result_in_r, "python.builtin.object")) {
    reticulate::py_run_string("rpandas_dict = rpandas_df_out.to_dict(orient='list')")
    dict_from_py <- reticulate::py$rpandas_dict
    result_in_r <- as.data.frame(dict_from_py, stringsAsFactors = FALSE)
    reticulate::py_run_string("del rpandas_dict")
  }
  
  reticulate::py_run_string("del rpandas_df_out")
  
  if (return.as == "result") {
    return(result_in_r)
  }
  
  list(result = result_in_r, code = full_py_command)
}