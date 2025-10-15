#' Check for rPandas dependencies and provide diagnostics
#'
#' @description
#' This function checks if the user's system is correctly configured with Python
#' and the pandas library. If dependencies are missing, it stops with a
#' detailed diagnostic report and actionable instructions.
#'
#' @export
#' @return Invisibly returns `TRUE` if all checks pass.
rp_check_env <- function() {

  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python could not be found by reticulate.\n\n",
      "--- How to Fix ---\n",
      "1. Install Python (e.g., from https://www.python.org/downloads/).\n",
      "2. Or, for a self-contained environment, run this in your R console:\n",
      '   reticulate::install_miniconda()',
      call. = FALSE
    )
  }

  if (!reticulate::py_module_available("pandas")) {
    config <- reticulate::py_config()
    python_path <- config$python

    error_message <- paste0(
      "The 'pandas' Python library was not found in the current environment.\n\n",
      "--- rPandas System Diagnosis ---\n",
      "rPandas is currently configured to use this Python installation:\n",
      "> ", python_path, "\n\n",
      "This environment does NOT contain the 'pandas' library.\n\n",
      "--- How to Fix ---\n",
      "1. To install pandas into this specific environment, run this in your R console:\n",
      '   reticulate::py_install("pandas")\n\n',
      "2. If the path above is incorrect, restart your R session and tell R the correct path *before* loading any libraries:\n",
      '   reticulate::use_python("/path/to/your/correct/python")\n',
      "------------------------------"
    )
    stop(error_message, call. = FALSE)
  }

  invisible(TRUE)
}