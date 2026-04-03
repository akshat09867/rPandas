#' Translate an R filter expression into a Python query string
#'
#' @description
#' Capture a bare R expression and translate it to a Python-compatible string
#' suitable for use with `pandas.DataFrame.query()`.
#' @param expr A bare R expression (e.g., `carat > 2 & cut == "Ideal"`).
#' @return A character string of the translated Python query.
#' @export
#' @examples
#' translate_filter(carat > 2 & cut == "Ideal")
#' # -> "(carat > 1) and (cut == 'Ideal')"
translate_filter <- function(expr) {
  expr_quo <- rlang::enquo(expr)
  expr_env <- rlang::get_env(expr_quo)
  expr_body <- rlang::get_expr(expr_quo)

  out <- translate_filter_recursive(expr_body, env = expr_env)
  
  out <- sub("^\\((.*)\\)$", "\\1", out)
  return(out)
}


#' Recursive helper to translate R expressions
#' @param expr_body A language object (call, symbol, or atomic).
#' @return A character string.
#' @keywords internal
translate_filter_recursive <- function(expr_body, env) {
  if (is.symbol(expr_body)) {
    return(as.character(expr_body))
  }
  if (is.atomic(expr_body)) {
    if (is.logical(expr_body) && length(expr_body) == 1) {
      if (is.na(expr_body)) return("None")
      return(ifelse(expr_body, "True", "False"))
    }
    if (is.character(expr_body)) {
      esc <- gsub("'", "\\\\'", expr_body, fixed = TRUE)
      if (length(esc) > 1) {
        return(sprintf("['%s']", paste(esc, collapse = "', '")))
      }
      return(sprintf("'%s'", esc))
    }
    if (is.numeric(expr_body)) {
      if (length(expr_body) > 1) {
        return(sprintf("[%s]", paste(expr_body, collapse = ", ")))
      }
      return(as.character(expr_body))
    }
    return(as.character(expr_body))
  }

  if (is.call(expr_body)) {
    op <- as.character(expr_body[[1]])
    args <- as.list(expr_body)[-1]
    
    tr <- function(x) translate_filter_recursive(x, env = env)

    if (op == "(") {
      return(tr(args[[1]]))
    }
    
    if (op == "c") {
      pieces <- vapply(args, tr, FUN.VALUE = character(1))
      return(paste0("[", paste(pieces, collapse = ", "), "]"))
    }
    
    if (op %in% c("&", "&&")) return(paste0("(", tr(args[[1]]), " and ", tr(args[[2]]), ")"))
    if (op %in% c("|", "||")) return(paste0("(", tr(args[[1]]), " or ", tr(args[[2]]), ")"))
    if (op == "!") return(paste0("(not ", tr(args[[1]]), ")"))
    if (op == "is.na") return(paste0("(", tr(args[[1]]), ".isna())"))

    if (op %in% c("%in%", "%notin%")) {
      py_op <- ifelse(op == "%in%", "in", "not in")
      lhs <- tr(args[[1]])
      
      rhs_val <- eval(args[[2]], envir = env)
      
      rhs_str <- tr(rhs_val)
      
      return(paste0("(", lhs, " ", py_op, " ", rhs_str, ")"))
    }

    py_op <- switch(op, "%/%" = "//", "%%"  = "%", op) 
    
    if (py_op %in% c("==", "!=", ">", ">=", "<", "<=", "+", "-", "*", "/", "//", "%")) {
      return(paste0("(", tr(args[[1]]), " ", py_op, " ", tr(args[[2]]), ")"))
    }

    translated_args <- vapply(args, tr, character(1))
    return(paste0(op, "(", paste(translated_args, collapse = ", "), ")"))
  }

  stop("Unrecognized expression type in translate_filter_recursive()")
}

#' "Not In" Operator
#'
#' @description
#' Provides the opposite of the standard R `%in%` operator.
#' @param x Vector of values to be matched.
#' @param y Vector of values to be matched against.
#' @return A logical vector.
#' @export
#' @examples
#' "a" %notin% c("b", "c")
`%notin%` <- function(x, y) {
  !(x %in% y)
}


#' Translate captured column names into a Python list string
#'
#' @param ... Bare column names (captured by `rlang::enquos`).
#' @return A character string formatted as a Python list.
#' @keywords internal
translate_select <- function(...) {
  exprs <- rlang::enquos(...)

  col_names <- vapply(exprs, function(expr) {
    text <- rlang::expr_text(expr)
      gsub("^~", "", text)  
  }, character(1))

  py_list_str <- sprintf("['%s']", paste(col_names, collapse = "', '"))
  
  return(py_list_str)
}

#' Translate captured sort expressions into Python .sort_values() arguments
#'
#' @param ... Bare column names or `desc(colname)` (captured by `rlang::enquos`).
#' @return A list with two elements:
#'   - `$by`: A string for the `by` argument (e.g., \code{['cut', 'price']})
#'   - `$ascending`: A string for the `ascending` argument (e.g., \code{[True, False]})
#' @keywords internal
translate_sort <- function(...) {
  exprs <- rlang::enquos(...)

  cols <- character()
  asc  <- logical()

  for (q in exprs) {
    e <- rlang::get_expr(q)

    if (is.call(e) && rlang::call_name(e) == "desc") {
      cols <- c(cols, rlang::as_string(e[[2]]))
      asc  <- c(asc, FALSE)
    } else {
      cols <- c(cols, rlang::as_string(e))
      asc  <- c(asc, TRUE)
    }
  }

  by_str  <- sprintf("['%s']", paste(cols, collapse = "', '"))
  asc_py  <- ifelse(asc, "True", "False")
  asc_str <- sprintf("[%s]", paste(asc_py, collapse = ", "))

  return(list(by = by_str, ascending = asc_str))
}

#' Translate R mutate expressions to pandas assign/drop strings
#'
#' @param to_remove Character vector of column names to drop.
#' @param ... Named R expressions.
#' @return A list with components assign_str and drop_str.
#' @keywords internal
translate_mutate <- function(to_remove = NULL, ...) {
  exprs <- rlang::enquos(...)
    if (length(exprs) > 0) {
    nm <- names(exprs)
    if (is.null(nm) || any(nm == "")) {
      stop("All arguments to rp_mutate() must be named (e.g., new_col = expression).",
           call. = FALSE)
    }
  }
  
  assign_pieces <- character()
  if (length(exprs) > 0) {
   assign_pieces <- vapply(seq_along(exprs), function(i) {
      q <- exprs[[i]]
      col_name <- names(exprs)[i]          # Get the argument name
      translated <- translate_assign_recursive(rlang::get_expr(q))
      paste0(col_name, " = lambda x: ", translated)
    }, FUN.VALUE = character(1))
  }
  
  drop_str <- NULL
  if (length(to_remove) > 0) {
    if (!is.character(to_remove)) {
      stop("`to_remove` must be a character vector of column names.", call. = FALSE)
    }
    quoted <- paste0('"', to_remove, '"')
    drop_str <- sprintf(".drop(columns = [%s])", paste(quoted, collapse = ", "))
  }
  
  list(
    assign_str = paste(assign_pieces, collapse = ", "),
    drop_str = drop_str
  )
}

#' Recursively translate an R expression for a pandas .assign() lambda
#'
#' @param expr_body A language object (call, symbol, or atomic).
#' @return A character string of the translated Python expression.
#' @keywords internal
translate_assign_recursive <- function(expr_body) {
  if (is.symbol(expr_body)) {
    return(sprintf("x['%s']", as.character(expr_body)))
  }

  if (is.atomic(expr_body)) {
    if (is.logical(expr_body) && length(expr_body) == 1) {
      if (is.na(expr_body)) return("None")
      return(ifelse(expr_body, "True", "False"))
    }
    if (is.character(expr_body)) {
      esc <- gsub("'", "\\\\'", expr_body, fixed = TRUE)
      return(sprintf("'%s'", esc))
    }
    return(as.character(expr_body))
  }

  if (is.call(expr_body)) {
    op <- as.character(expr_body[[1]])
    args <- as.list(expr_body)[-1]
    
    tr <- function(x) translate_assign_recursive(x)

    
    if (op %in% c("&", "&&")) return(paste0("(", tr(args[[1]]), " and ", tr(args[[2]]), ")"))
    if (op %in% c("|", "||")) return(paste0("(", tr(args[[1]]), " or ", tr(args[[2]]), ")"))
    if (op == "!") return(paste0("not (", tr(args[[1]]), ")"))

    if (op == "is.na") return(paste0(tr(args[[1]]), ".isna()"))
    
    py_op <- switch(op,
                    "%/%" = "//",
                    "%%"  = "%",
                    op)
    
    if (py_op %in% c("==", "!=", ">", ">=", "<", "<=", "+", "-", "*", "/", "//", "%")) {
      return(paste0("(", tr(args[[1]]), " ", py_op, " ", tr(args[[2]]), ")"))
    }

    translated_args <- vapply(args, tr, character(1))
    return(paste0(op, "(", paste(translated_args, collapse = ", "), ")"))
  }

  stop("Unrecognized expression type in translate_assign_recursive()")
}




#' Translate a .by argument into a pandas .groupby() string
#'
#' @param by_expr An enquosured `.by` argument.
#' @return A string for the `.groupby(..., as_index=False)` method,
#'   or NULL if the argument is empty.
#' @keywords internal
translate_groupby <- function(by_expr) {
  expr_body <- rlang::get_expr(by_expr)
  
  if (rlang::is_null(expr_body)) {
    return(NULL)
  }
  
  col_names <- character()
  
  if (is.call(expr_body) && rlang::call_name(expr_body) == "c") {
    args <- rlang::call_args(expr_body)
    col_names <- vapply(args, rlang::expr_text, character(1))
  } else {
    col_names <- rlang::expr_text(expr_body)
  }
  
  py_list_str <- sprintf("['%s']", paste(col_names, collapse = "', '"))
  
  sprintf(".groupby(%s, as_index=False)", py_list_str)
}


#' Translate named R expressions for .agg()
#'
#' @description
#' Translates R's `new = func(old)` syntax into pandas' named aggregation
#' syntax `new = ('old', 'func')`.
#'
#' @param ... Named R expressions (e.g., `avg_price = mean(price)`).
#' @return A string for the `.agg()` method.
#' @keywords internal
translate_summarize <- function(...) {
  exprs <- rlang::enquos(...)
  
  if (rlang::is_empty(exprs)) {
    stop("No summary functions provided.", call. = FALSE)
  }
  if (is.null(names(exprs)) || any(names(exprs) == "")) {
    stop("All arguments to rp_summarize() must be named.", call. = FALSE)
  }
  
  new_col_names <- names(exprs)
  agg_pieces <- character(length(exprs))
  
  for (i in seq_along(exprs)) {
    new_name <- new_col_names[i]
    expr_body <- rlang::get_expr(exprs[[i]])
    
    if (!is.call(expr_body)) {
      stop("Summarize expressions must be function calls (e.g., mean(price)).", call. = FALSE)
    }
    
    r_func_name <- rlang::call_name(expr_body)

    
    if (r_func_name == "n") {
      py_func_name <- "size"

      r_col_name <- "price" 
    } else {
      call_args <- rlang::call_args(expr_body)
      
      if (rlang::is_empty(call_args)) {
        stop(paste("Function", r_func_name, "needs a column argument."), call. = FALSE)
      }
      
      r_col_name <- rlang::expr_text(call_args[[1]])
      
      py_func_name <- switch(r_func_name,
        "mean" = "mean",
        "median" = "median",
        "sd" = "std",
        "var" = "var",
        "min" = "min",
        "max" = "max",
        "sum" = "sum",
        stop(paste("Unknown summary function:", r_func_name), call. = FALSE)
      )
    }
    agg_pieces[i] <- sprintf("%s = ('%s', '%s')", new_name, r_col_name, py_func_name)
  }
  
  sprintf(".agg(%s)", paste(agg_pieces, collapse = ", "))
}



#' Translate R function/column names into a pandas agg dictionary
#'
#' @description
#' Translates R's `the.variables` and `the.functions` into pandas'
#' dictionary-based `.agg()` syntax.
#'
#' @param variable_exprs A list of enquosured variable names.
#' @param function_names A character vector of R function names.
#' @return A string for the `.agg()` method (e.g., \code{.agg({'col1': ['mean', 'std']})}).
#' @keywords internal
translate_calculate <- function(variable_exprs, function_names) {
  
  map_r_to_py_func <- function(r_func_name) {
    py_func_name <- switch(r_func_name,
      "mean" = "mean",
      "median" = "median",
      "sd" = "std",
      "var" = "var",
      "min" = "min",
      "max" = "max",
      "sum" = "sum",
      stop(paste("Unknown summary function:", r_func_name), call. = FALSE)
    )
    return(py_func_name)
  }
    cols <- vapply(variable_exprs, function(expr) {
    text <- rlang::expr_text(expr)
      gsub("^~", "", text)  
  }, character(1))
  
  py_funcs <- vapply(function_names, map_r_to_py_func, character(1))
  py_funcs_str <- sprintf("['%s']", paste(py_funcs, collapse = "', '"))
  
  agg_pieces <- vapply(cols, function(col) {
    sprintf("'%s': %s", col, py_funcs_str)
  }, character(1))
  
  dict_str <- paste(agg_pieces, collapse = ", ")
  sprintf(".agg({%s})", dict_str)
}