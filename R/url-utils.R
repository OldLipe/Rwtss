#' @title Parse a JSON response from the WTSS server
#' @name .parse_json
#'
#' @description Parse a JSON response from the WTSS service
#'
#' @param response a response httr object
#' @return  parsed JSON document
.parse_json <- function(response) {
    if (httr::http_type(response) != "application/json") {
        stop("invalid response type. Expect JSON type...")
    }
    parsed_response <- httr::content(
        x = response, 
        as = "parse", 
        type = "application/json", 
        encoding = "UTF-8", 
    )
    return(parsed_response)
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