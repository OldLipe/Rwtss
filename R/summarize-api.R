.sum_retrieve_summary <- function(url,
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
    cov <- .get_cov_details(url = url, name = name)
    
    # Check if the selected attributes are available
    cov_bands <- .get_bands(cov)
    attributes <- .default(attributes, value = cov_bands)
    .check_bands(attributes, cov_bands)
    
    # Create a geom list object
    geom <- .create_geom_lst(geom = geom)
    .check_bbox(geom, cov = cov)
    
    # Check start_date and end_date range is valid
    timeline <- .get_timeline(cov)
    start_date <- .default(start_date, value = min(timeline))
    end_date <- .default(end_date, value = max(timeline))
    # TODO: format date to datetime
    .check_interval(start_date = start_date, end_date = end_date, cov = cov)
    
    # Convert to list to provide to query string    
    attributes <- .to_list(attributes)
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
    # Format summarized into a tibble 
    summarize_tbl <- .sum_format_tbl(parsed_response)
    # Set classes in summarize object
    set_class(summarize_tbl) <- c("summarize")
    # Return summarize
    return(summarize_tbl)
}

.sum_format_tbl <- function(response) {
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
    return(ts_tbl)
}