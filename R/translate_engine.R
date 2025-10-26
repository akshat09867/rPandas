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

#' Translate named R expressions for .assign()
#'
#' @description
#' Captures one or more named R expressions and translates them into a
#' single, comma-separated string of keyword arguments for pandas `.assign()`.
#'
#' @param ... Named R expressions (e.g., `new_col = old_col * 2`).
#' @return A single string (e.g., "new_col = (old_col * 2), other = 'value'").
#' @keywords internal
translate_mutate <- function(...) {
  exprs <- rlang::enquos(...)
  
  if (rlang::is_empty(exprs)) {
    return(NULL)
  }
  if (is.null(names(exprs)) || any(names(exprs) == "")) {
    stop("All arguments to rp_mutate() must be named.", call. = FALSE)
  }
  
  new_col_names <- names(exprs)
  
  translated_exprs <- vapply(exprs, function(expr) {
    translate_assign_recursive(rlang::get_expr(expr))
  }, FUN.VALUE = character(1))

  assign_pieces <- paste0(new_col_names, " = lambda x: ", translated_exprs)
  print(paste(assign_pieces, collapse = ", "))
  return(paste(assign_pieces, collapse = ", "))
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