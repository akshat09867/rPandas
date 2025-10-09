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