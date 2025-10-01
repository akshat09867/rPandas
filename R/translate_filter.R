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
#' # -> "carat > 2 and cut == 'Ideal'"
translate_filter <- function(expr) {
  expr_quo <- rlang::enquo(expr)
  expr_body <- rlang::get_expr(expr_quo)
  out <- translate_filter_recursive(expr_body)
  out <- sub("^\\((.*)\\)$", "\\1", out)
  return(out)
}

translate_filter_recursive <- function(expr_body) {
  if (is.symbol(expr_body)) {
    return(as.character(expr_body))
  }

  if (is.atomic(expr_body)) {
    if (is.logical(expr_body) && length(expr_body) == 1) {
      if (is.na(expr_body)) return("None")
      return(ifelse(expr_body, "True", "False"))
    }

    if (is.character(expr_body)) {
      esc <- vapply(expr_body, function(x) gsub("'", "\\\\'", x, fixed = TRUE),
                    FUN.VALUE = character(1), USE.NAMES = FALSE)
      if (length(esc) > 1) {
        return(sprintf("['%s']", paste(esc, collapse = "', '")))
      } else {
        return(sprintf("'%s'", esc))
      }
    }

    if (is.numeric(expr_body)) {
      if (length(expr_body) > 1) {
        return(sprintf("[%s]", paste(expr_body, collapse = ", ")))
      } else {
        return(as.character(expr_body))
      }
    }

    return(as.character(expr_body))
  }

  if (is.call(expr_body)) {
    op <- as.character(expr_body[[1]])
    args <- as.list(expr_body)[-1]

    tr <- function(x) translate_filter_recursive(x)

    if (op == "c") {
      pieces <- vapply(args, function(a) {
        translate_filter_recursive(a)
      }, FUN.VALUE = character(1), USE.NAMES = FALSE)
      return(paste0("[", paste(pieces, collapse = ", "), "]"))
    }

    if (op %in% c("&", "&&")) {
      return(paste0("(", tr(args[[1]]), " and ", tr(args[[2]]), ")"))
    }
    if (op %in% c("|", "||")) {
      return(paste0("(", tr(args[[1]]), " or ", tr(args[[2]]), ")"))
    }
    if (op == "!") {
      return(paste0("(not ", tr(args[[1]]), ")"))
    }

    if (op == "%in%") {
      lhs <- tr(args[[1]])
      rhs_expr <- args[[2]]
      if (is.symbol(rhs_expr)) {
        return(paste0("(", lhs, " in @", as.character(rhs_expr), ")"))
      } else {
        rhs <- tr(rhs_expr)
        return(paste0("(", lhs, " in ", rhs, ")"))
      }
    }

    if (op %in% c("is.na", "is.na")) {
      return(paste0("(", tr(args[[1]]), ".isna())"))
    }

    if (op %in% c("==", "!=", ">", ">=", "<", "<=",
                  "+", "-", "*", "/", "%/%", "%%")) {
      py_op <- switch(op,
                      "==" = "==",
                      "!=" = "!=",
                      ">"  = ">",
                      ">=" = ">=",
                      "<"  = "<",
                      "<=" = "<=",
                      "+"  = "+",
                      "-"  = "-",
                      "*"  = "*",
                      "/"  = "/",
                      "%/%" = "//",   
                      "%%"  = "%",    
                      op)
      return(paste0("(", tr(args[[1]]), " ", py_op, " ", tr(args[[2]]), ")"))
    }

    translated_args <- vapply(args, function(a) tr(a), character(1), USE.NAMES = FALSE)
    return(paste0(op, "(", paste(translated_args, collapse = ", "), ")"))
  }

  stop("Unrecognized expression type in translate_filter_recursive()")
}
