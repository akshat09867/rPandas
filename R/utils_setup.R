#' Check for rPandas dependencies
#'
#' @description
#' This function checks if the user's system is correctly configured with Python
#' and the pandas library. It provides helpful messages and instructions if
#' dependencies are missing.
#'
#' All user-facing `rPandas` functions will call this function internally to
#' ensure the environment is ready.
#'
#' @export
#' @return Invisibly returns `TRUE` if all checks pass, otherwise it stops
#'   with an informative error message.
rp_check_env <- function() {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required for this function. Please install it, or call this function only when reticulate is available.", call. = FALSE)
  }
  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python could not be found.\n",
      "Please install Python (e.g., from https://www.python.org/downloads/)\n",
      "or install a Conda environment (e.g., via reticulate::install_miniconda()).",
      call. = FALSE
    )
  }
  
  if (!reticulate::py_module_available("pandas")) {
    stop(
      "The 'pandas' Python library was not found.\n",
      "Please install it by running the following command in your R console:\n",
      'reticulate::py_install("pandas")',
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}