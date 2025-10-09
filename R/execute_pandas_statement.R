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
    stop("`return.as` must be 'result', 'code', or 'all'.", call. = FALSE)
  }

  py_df_name <- "rpandas_df_in"

 py[[py_df_name]] <- r_df


  full_py_command <- gsub("\\bdf\\b", py_df_name, py_command, perl = TRUE)

  if (return.as == "code") {
    reticulate::py_run_string(paste("del", py_df_name))
    return(full_py_command)
  }


  py_script <- sprintf("
import pandas as pd
rpandas_df_out = %s
", full_py_command)

  reticulate::py_run_string(py_script)
  
  reticulate::py_run_string(paste("del", py_df_name))

  result_from_py <- reticulate::py$rpandas_df_out
  
  reticulate::py_run_string("del rpandas_df_out")

  if (return.as == "result") {
    return(result_from_py)
  }
  if (return.as == "all") {
    return(list(result = result_from_py, code = full_py_command))
  }
}