.get_timeseries <- function(URL,
                            name,      
                            attributes,
                            longitude,
                            latitude, 
                            start_date,
                            end_date,
                            token) {
    # Build url to send request
    req_url <- .build_url(
        url = URL, 
        path = "time_series", 
        query = list("coverage"   = name,
                     "attributes" = paste(attributes, collapse = ","),
                     "longitude"  = longitude, 
                     "latitude"   = latitude,
                     "start_date" = start_date, 
                     "end_date"   = end_date,
                     "access_token" = token
        )
    )
    
    # Send a request to the WTSS server
    response <- .get_request(req_url)
    # Parse response object
    parsed_response <- .parse_json(response)
    # Check ...
    if (length(parsed_response$result$attributes) == 0) {
        stop(
            paste("The requisition returns zero attributes as result.",
                  "Please check your request or contact the server",
                  "maintenance."))
    }
    # Post-processing
    result <- list(.wtss_time_series_processing(parsed_response))
    names(result) <- name
    # Convert to tibble 
    ts_tbl <- .wtss_to_tibble(ts = result, name = name, bands = attributes, 
                              longitude = longitude, latitude = latitude, 
                              start_date = start_date, end_date = end_date, 
                              cov_desc = wtss.env$desc)
    
    # Set class         
    set_class(ts_tbl) <- "wtss"
    # Return ...
    return(ts_tbl)
}