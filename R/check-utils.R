.check_valid_url <- function(URL) {
    .is_chr(URL)    
    if (is.null(.get_request(URL))) {
        stop("URL is not ...")
    }
}

.check_chr <- function(x, len_min = 1, len_max = Inf) {
    if (!.is_chr(x)) {
        stop("Invalid argument. Expected a character")
    }
    if (length(x) > len_min && length(x) < len_max) {
        stop("invalid ...")
    }
}

.is_chr <- function(x) {
    is.character(x)
}

.is_error <- function(x) {
    inherits(x, "error")
}

.is_tbl <- function(x) {
    inherits(x, c("tbl_df", "tbl", "data.frame"))
}

`set_class<-` <- function(x, value, append = TRUE) {
    .is_chr(value)
    if (append) {
        value <- c(value, class(x))
    }
    class(x) <- value
    return(x)
}