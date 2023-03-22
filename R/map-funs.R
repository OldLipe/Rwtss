.map_chr <- function(x, fn, ...) {
    vapply(X = x, FUN = fn, ..., FUN.VALUE = character(1))
}

.map_lgl <- function(x, fn, ...) {
    vapply(X = x, FUN = fn, ..., FUN.VALUE = logical(1))
}

.map_dfr <- function(x, fn, ...) {
    result_lst <- lapply(X = x, FUN = fn, ...)
    is_elem_tbl <- .map_lgl(result_lst, .is_tbl)
    if (!any(is_elem_tbl)) {
        stop("'fn' should return a tibble in .map_dfr")
    }
    return(do.call(rbind, result_lst))
}
