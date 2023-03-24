.check_valid_url <- function(URL) {
    .is_chr(URL)    
    if (is.null(.get_request(URL))) {
        stop("URL is not ...")
    }
}

.check_chr <- function(x, len_min = 1, len_max = Inf) {
    if (!.is_chr(x)) {
        stop("Invalid argument. Expected a character parameter.")
    }
    if (length(x) < len_min && length(x) > len_max) {
        stop("invalid ...")
    }
}

.check_num <- function(x, len_min = 1, len_max = Inf) {
    if (!.is_num(x)) {
        stop("Invalid argument. Expected a numeric parameter.")
    }
    if (length(x) > len_min && length(x) < len_max) {
        stop("invalid ...")
    }
}

.check_date <- function(date, min, max) {
    if (is.null(date)) {
        return(NULL)
    }
    
    if (!.is_date(date) || !.is_chr(date)) {
        stop(paste("Invalid date parameter.", 
                   "Must be a character or date type."),
             call. = FALSE
        )
    }
    if (!.is_date(min) || !.is_chr(min) || !is.null(min) &&
        !.is_date(max) || !.is_chr(max) || is.null(max)) {
        stop(paste("Invalid min or max parameters.", 
                   "Must be a character or date type."),
             call. = FALSE
        )
    }
    if (date < min || date > max ) {
        stop("'date' should be greater than 'min' and less than 'max'.")
    }
}

.check_locations <- function(longitude, latitude) {
    .check_num(longitude, len_min = 1, len_max = 1)
    .check_num(latitude, len_min = 1, len_max = 1)
    # TODO: add long and lat check in degress
}

.is_chr <- function(x) {
    is.character(x)
}

.is_num <- function(x) {
    is.numeric(x)
}

.is_error <- function(x) {
    inherits(x, "error")
}

.is_tbl <- function(x) {
    inherits(x, c("tbl_df", "tbl", "data.frame"))
}

.is_date <- function(x) {
    inherits(x, "Date")
}

`set_class<-` <- function(x, value, append = TRUE) {
    .is_chr(value)
    if (append) {
        value <- c(value, class(x))
    }
    class(x) <- value
    return(x)
}

.default <- function(x, value) {
    if (!is.null(x)) {
        return(x)
    }
    return(value)
}

.check_bands <- function(x, y) {
    if (!all(x %in% y)) {
        stop("Rwtss - attributes not available.", call. = FALSE)
    }
}

.check_bbox <- function(longitude, latitude, bbox) {
    if (longitude < bbox[["xmin"]] || longitude > bbox[["xmax"]]) {
        message("Rwtss - invalid longitude value")
        return(NULL)
    }
    if (latitude < bbox[["ymin"]] || latitude > bbox[["ymax"]]) {
        message("Rwtss - invalid latitude value")
        return(NULL)
    }
}

.check_s3_class <- function(x, classes) {
    if (inherits(x, classes)) {
        stop("Invalid object")
    }
}


