#' @title Post a response to the WTSS server
#' @name .wtss_get_response 
#'
#' @description Sends a request to the WTSS server and gets a response
#'
#' @param request   valid request according to the WTSS protocol
#' @param ...       additional parameters that can be added in httr.
#' @return  response from the server
.get_request <- function(request, ..., n_tries = 3) {
    # Sends 3x requests to the WTSS server
    response <- tryCatch({
        httr::RETRY(
            verb = "GET",
            url = request,
            times = n_tries,
            pause_base = 3,
            pause_cap = 20,
            pause_min = 1,
            terminate_on_success = FALSE,
            quiet = TRUE, ...
        )}, 
        error = function(e) {
            simpleError("Invalid requisition...")
        })
    
    .check_response(request, response)
    return(response)
}

.post_request <- function(request, body, encode = "json", ..., n_tries = 3) {
    # Sends 3x requests to the WTSS server
    response <- tryCatch({
        httr::RETRY(
            verb = "POST",
            body = body,
            encode = encode,
            url = request,
            times = n_tries,
            pause_base = 3,
            pause_cap = 20,
            pause_min = 1,
            terminate_on_success = FALSE,
            quiet = TRUE, ...
        )}, 
        error = function(e) {
            simpleError("Invalid requisition...")
        })
    
    .check_response(request, response)
    return(response)
}
