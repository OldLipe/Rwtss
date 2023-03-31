.summarize_timeseries <- function(url,
                                  name,
                                  geom,
                                  attributes,
                                  aggregations,
                                  start_date,
                                  end_date,
                                  scale,
                                  pixel_strategy,
                                  qa_values,
                                  masked,
                                  token, ...) {
    # Have we described the coverage before?
    .get_coverage_details(URL = url, name = name)
    
    # Check if the selected attributes are available
    coverage_bands <- .get_bands(wtss.env$desc)
    attributes <- .default(attributes, coverage_bands)
    .check_bands(attributes, coverage_bands)
    
    # Check bounds for latitude and longitude
    coverage_bbox <- .get_bbox(wtss.env$desc)
    # Create a geom list object
    geom <- .create_geom_lst(geom = geom)
    # TODO: transform to geom first
    .check_bbox(geom, coverage_bbox)
    
    # Check start and end date
    # TODO: verify if date is datetime, otherwise transforms it
    timeline <- .get_timeline(wtss.env$desc)
    start_date <- .default(start_date, min(timeline))
    end_date <- .default(end_date, max(timeline))
    
    # Check start_date and end_date range is valid
    .check_date(start_date, min_date = min(timeline), max_date = max(timeline))
    .check_date(end_date, min_date = min(timeline), max_date = max(timeline))
    
    # ...
    if (length(attributes) == 1) {
        attributes <- list(attributes)
    }
    # Build url to send request
    req_url <- .build_url(
        url = url, 
        path = c(name, "summarize"),
        query = c("access_token" = token)
    )
    # Send a request to the WTSS server
    response <- .post_request(
        request = req_url,
        body = list(
            "geom" = geom,
            "attributes" = attributes,
            "aggregations" = aggregations,
            "start_datetime" = start_date,
            "end_datetime" = end_date,
            "applyAttributeScale" = scale,
            "pixelCollisionType" = pixel_strategy,
            "qa" = qa_values,
            "masked" = masked
        ),
        encode = "json",
        httr::verbose()
    )
    # Parse response object
    parsed_response <- .parse_json(response)
    # Check ...
    .check_results(parsed_response)
    # ...
    summarize_tbl <- .sum_format_tbl(parsed_response)
    set_class(summarize_tbl) <- c("timeseries", "wtss")
    
    return(summarize_tbl)
}

.sum_format_tbl <- function(response, url) {
    results <- .res_get_results(response)
    ts_tbl <- .map_dfr(seq_len(length(results)), function(idx) {
        # Get each pixel results
        ts <- results[[idx]]
        # Create a tibble with time series values
        ts_values <- .map_dfc(ts[["time_series"]][["values"]], function(x) {
            tibble::tibble(unlist(x))
        })
        # Put names in each band column
        colnames(ts_values) <- names(ts[["time_series"]][["values"]])
        # Add metadata columns into time series
        tibble::add_column(
            ts_values,
            longitude = ts[["pixel_center"]][["coordinates"]][[1]],
            latitude  = ts[["pixel_center"]][["coordinates"]][[2]],
            Index     = unlist(ts[["time_series"]][["timeline"]]),
            .before   = 1
        )
    })
    # Creating a time series column with time series values
    ts_tbl <- tidyr::nest(ts_tbl, "time_series" = -c("longitude", "latitude"))
    ts_tbl <- tibble::add_column(
        ts_tbl, 
        start_date = .res_get_start_date(response),
        end_date   = .res_get_end_date(response),
        label      = "NoLabel",
        cube       = .res_get_col_id(response),
        .after = 2
    )
    attr(ts_tbl, "response") <- response
    attr(ts_tbl, "req_url") <- url
    return(ts_tbl)
}