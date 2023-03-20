.is_valid_url <- function(URL) {
    .is_chr(URL)    
    if (is.null(.get_request(URL))) {
        stop("URL is not ...")
    }
}

.get_request <- function(url, ...) {
    response <- tryCatch({
        httr::stop_for_status(httr::GET(request, ...))
    }, 
    error = function(e) {
        return(NULL)
    })
    
    return(response)
}

.is_chr <- function(x) {
    if (!is.character(x)) {
        stop("Rwtss - parameter should be a character")
    }
}

.check_length <- function(x, min = 1, max = Inf) {
    if (length(x) >= min && length(x) <= max) {
        stop("invalid ...")
    }
}

.rm_trailing_dash <- function(url) {
    gsub(pattern = "/*$", replacement = "", x = url)
}

.build_url <- function(url, path = NULL, query = NULL, endpoint = NULL) {
    url <- .rm_trailing_dash(url)
    parsed <- httr::parse_url(url)
    parsed[["path"]] <- c(parsed[["path"]], path)
    parsed[["query"]] <- c(parsed[["query"]], query)
    parsed[["endpoint"]] <- c(parsed[["endpoint"]], endpoint)
    
    return(httr::build_url(parsed))
}