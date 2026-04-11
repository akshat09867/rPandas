#' Check for rPandas dependencies and provide diagnostics
#'
#' @description
#' This function checks if the user's system is correctly configured with Python
#' and the pandas library. If dependencies are missing, it stops with a
#' detailed diagnostic report and actionable instructions, including listing
#' available conda environments if applicable.
#'
#' @export
#' @return Invisibly returns `TRUE` if all checks pass.
rp_check_env <- function() {

  if (!reticulate::py_available(initialize = TRUE)) {
    stop(
      "Python could not be found by reticulate.\n\n",
      "--- How to Fix ---\n",
      "Option 1 (recommended): Install Miniconda (self-contained Python)\n",
      "  Run in R: reticulate::install_miniconda()\n\n",
      "Option 2: Use an existing Python installation\n",
      "  Run in R before loading rPandas:\n",
      '  reticulate::use_python("/path/to/your/python", required = TRUE)\n',
      "  Find your Python path with: Sys.which('python')\n\n",
      "After fixing, restart R and try again.\n",
      "------------------------------",
      call. = FALSE
    )
  }

  if (!reticulate::py_module_available("pandas")) {
    config <- reticulate::py_config()
    python_path <- config$python

    msg <- paste0(
      "The 'pandas' Python library was not found in the current environment.\n\n",
      "--- rPandas System Diagnosis ---\n",
      "rPandas is currently configured to use this Python installation:\n",
      "> ", python_path, "\n\n",
      "This environment does NOT contain the 'pandas' library.\n"
    )

    conda_envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)
    if (!is.null(conda_envs) && nrow(conda_envs) > 0) {
      msg <- paste0(msg,
        "\n--- Available Conda Environments ---\n",
        paste0(conda_envs$name, " (", conda_envs$python, ")", collapse = "\n"),
        "\n\n"
      )
      msg <- paste0(msg,
        "Try switching to the first environment:\n",
        '  reticulate::use_python(python = conda_list()$python[1], required = TRUE)\n',
        "Then restart R and reload rPandas.\n\n"
      )
    } else {
      msg <- paste0(msg,
        "\nNo conda environments found. You can create one with pandas:\n",
        "  reticulate::conda_create('r-reticulate')\n",
        "  reticulate::conda_install('r-reticulate', 'pandas')\n",
        "  reticulate::use_condaenv('r-reticulate', required = TRUE)\n\n"
      )
    }

    msg <- paste0(msg,
      "--- How to Fix (quick) ---\n",
      "To install pandas into the current Python environment, run:\n",
      '  reticulate::py_install("pandas")\n\n',
      "If the path above is incorrect, restart R and set the correct Python path *before* loading rPandas:\n",
      '  reticulate::use_python("/correct/path", required = TRUE)\n',
      "------------------------------"
    )
    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}