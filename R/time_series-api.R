.get_timeseries <- function(URL,
                            name,      
                            longitude,
                            latitude, 
                            attributes,
                            start_date,
                            end_date,
                            token) {
    
    # Have we described the coverage before?
    if (is.null(wtss.env$desc) || .get_name(wtss.env$desc) != name) {
        wtss.env$desc <- describe_coverage(URL, name)
        # if describe_coverage fails, return a default time series
        if (is.null(wtss.env$desc)) {
            stop(paste("Could not retrieve description of coverage",
                       name, "from WTSS server."), call. = FALSE)
        }
    }
    
    # check if the selected attributes are available
    coverage_bands <- .get_bands(wtss.env$desc)
    attributes <- .default(attributes, coverage_bands)
    .check_bands(attributes, coverage_bands)
    
    # check bounds for latitude and longitude
    coverage_bbox <- .get_bbox(wtss.env$desc)
    .check_bbox(longitude, latitude, coverage_bbox)
    
    # Check start and end date
    timeline <- .get_timeline(wtss.env$desc)
    start_date <- .default(start_date, min(timeline))
    end_date <- .default(end_date, max(timeline))
    
    # Check start_date and end_date range is valid
    .check_date(start_date, min = min(timeline), max = max(timeline))
    .check_date(end_date, min = min(timeline), max = max(timeline))
    
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
    set_class(ts_tbl) <- c("wtss", "sits")
    # Return ...
    return(ts_tbl)
}