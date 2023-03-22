#' @title Send a request to WTSS server
#' @name .wtss_send_request
#'
#' @description Sends a request to the WTSS server and times out after 10 tries
#'
#' @param request   valid request according to the WTSS protocol
#' @param ...       additional parameters that can be added in httr.
#' @return  response from the server
.wtss_send_request <- function(request, ...) {
    response <- NULL 
    ce <- 0
    # try 5 times (avoid time out connection)
    while (purrr::is_null(response) & ce < 5) {
        response <- .wtss_get_response(request, ...)
        ce <- ce + 1
    }
    
    return(response)
}

#' @title Get a response to the WTSS server
#' @name .wtss_get_response 
#'
#' @description Sends a request to the WTSS server and gets a response
#'
#' @param request   valid request according to the WTSS protocol
#' @param ...       additional parameters that can be added in httr.
#' @return  response from the server
.get_request <- function(request, n_tries = 3, ...) {
    # Sends 3x requests to the WTSS server and gets a response
    response <- tryCatch({
        httr::RETRY(
            verb = "GET",
            url = request,
            times = n_tries,
            pause_base = 3,
            pause_cap = 20,
            pause_min = 1,
            terminate_on_success = FALSE, ...
        )}, 
        error = function(e) {
            simpleError("Invalid requisition...")
        })
    
    if (.is_error(response)) {
        stop(paste("The requested URL was not possible to reach:", request),
             call. = FALSE
        )
    }
    
    httr::stop_for_status(
        response, task = "Invalid URL. please, ..."
    )
    return(response)
}
