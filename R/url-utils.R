.parse_json <- function(response) {
    if (httr::http_type(response) != "application/json") {
        .error("Invalid response type.",
               "x" = "Expect JSON document output.")
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

.querystrings_encode <- function(params) {
  return(lapply(params, paste0, collapse = ","))
}

.build_url <- function(url, path = NULL, query = NULL, endpoint = NULL) {
    url <- .rm_trailing_dash(url)
    parsed <- httr::parse_url(url)
    parsed[["path"]] <- c(parsed[["path"]], path)
    parsed[["query"]] <- c(parsed[["query"]], .querystrings_encode(query))
    parsed[["endpoint"]] <- c(parsed[["endpoint"]], endpoint)
    
    return(httr::build_url(parsed))
}
