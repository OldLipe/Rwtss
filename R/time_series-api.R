next_timeseries <- function(timeseries) {
    .check_timeseries(timeseries)
    if (!.ts_needs_paginate(timeseries)) {
        return(timeseries)
    }
    response <- .ts_get_response(timeseries)
    
    body <- response[["query"]]
    next_page <- response[["pagination"]][["next"]]
    body[["page"]] <- next_page
    req_url <- .ts_get_url(timeseries)
    # Append page to the query url
    page_url <- .build_url(
        url = req_url, query = c("page" = next_page)
    )
    # Send a request to the WTSS server
    response <- .post_request(
        request = page_url,
        body = body, 
        encode = "json",
        httr::verbose()
    )
    # Parse response object
    page_response <- .parse_json(response)
    # Format time series into a tibble
    results <- .ts_format_tbl(page_response, req_url)
    # Merge time series
    results <- .ts_merge_timeseries(timeseries, results)
    # create time series obj
    attr(results, "response") <- page_response
    attr(results, "req_url") <- req_url 
    set_class(results) <- c("timeseries", "wtss")
    results
}

fetch_timeseries <- function(timeseries) {
    .check_timeseries(timeseries)
    # Is it necessary paginate?
    do_paginate <- .ts_needs_paginate(timeseries)
    # Paginate all time series
    while (do_paginate) {
        # Retrieve time series from next page
        timeseries <- next_timeseries(timeseries)        
        # paginate again?
        do_paginate <- .ts_needs_paginate(timeseries)
    }
    return(timeseries)
}

.retrieve_timeseries <- function(url,
                                 name,      
                                 geom,
                                 longitude,
                                 latitude,
                                 attributes,
                                 start_date,
                                 end_date,
                                 pagination,
                                 scale,
                                 pixel_strategy,
                                 token, ...) {
    # Have we described the coverage before?
    cov <- .get_cov_details(url = url, name = name)
    
    # Check if the selected attributes are available
    cov_bands <- .get_bands(cov)
    attributes <- .default(attributes, value = cov_bands)
    .check_bands(attributes, cov_bands)
    
    # Create a geom list object
    geom <- .create_geom_lst(
        longitude = longitude, 
        latitude = latitude,
        geom = geom
    )
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
        path = c(name, "timeseries"),
        query = c("access_token" = token)
    )
    # Send a request to the WTSS server
    response <- .post_request(
        request = req_url,
        body = list(
            "attributes" = attributes,
            "geom" = geom, 
            "start_datetime" = start_date, 
            "end_datetime" = end_date,
            "pagination" = pagination,
            "applyAttributeScale" = scale,
            "pixelCollisionType" = pixel_strategy
        ),
        httr::verbose(),
        encode = "json"
    )
    # Parse response object
    parsed_response <- .parse_json(response)
    # Format time series into a tibble 
    timeseries <- .ts_format_tbl(parsed_response, req_url)
    # Set classes in time series object
    set_class(timeseries) <- c("timeseries", "wtss")
    # Return time series
    return(timeseries)
}

# ---- ts api ----

.ts_get_response <- function(timeseries) {
    attr(timeseries, "response")
}

.ts_get_url <- function(timeseries) {
    attr(timeseries, "req_url")
}

.ts_merge_timeseries <- function(timeseries, results) {
    ts_expanded <- tidyr::unnest(timeseries, "time_series")
    results_expanded <- tidyr::unnest(results, "time_series")
    ts_combined <- .rbind(list(ts_expanded, results_expanded))
    tidyr::nest(
        ts_combined, "time_series" = -c("longitude", "latitude", 
                                        "label", "cube")
    )
}

.ts_get_timeseries <- function(timeseries) {
    timeseries[["time_series"]]
}

.ts_needs_paginate <- function(timeseries) {
    response <- .ts_get_response(timeseries)
    if (!"pagination" %in% names(response) ||
        !"next" %in% names(response[["pagination"]])) {
        return(FALSE)
    }
    return(TRUE)
}

.ts_format_tbl <- function(response, url) {
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

# ---- res api ----

.res_get_results <- function(response) {
    response[["results"]]
}

.res_get_start_date <- function(response) {
    response[["query"]][["start_date"]]    
}

.res_get_col_id <- function(response) {
    response[["query"]][["collectionId"]]    
}

.res_get_end_date <- function(response) {
    response[["query"]][["end_date"]]    
}

# ---- spatial api ----

bbox_headers <- c("xmin", "ymin", "xmax", "ymax")
spatial_types <- c("Point", "Polygon", "MultiPoint", "MultiPolygon")

get_spatial <- function(x) {
    UseMethod("get_spatial", x)
}

#' @export
get_spatial.point <- function(x) {
    list(type = "Point", coordinates = unname(x))
}

#' @export
get_spatial.bbox <- function(x) {
    list(
        type = "Polygon",
        coordinates = list(
            rbind(
                c(x[[1]], x[[2]]),
                c(x[[3]], x[[2]]),
                c(x[[3]], x[[4]]),
                c(x[[1]], x[[4]]),
                c(x[[1]], x[[2]])
            )
        )
    )
}

#' @export
get_spatial.character <- function(x) {
    x <- tryCatch({
        jsonlite::fromJSON(
            txt = x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
            simplifyMatrix = FALSE
        )
    }, error = function(e) {
        .error("Invalid GeoJSON string.")
    })
    get_spatial(x)
}

#' @export
get_spatial.list <- function(x) {
    if (!all(c("type", "coordinates") %in% names(x))) {
        .error("Not a valid GeoJSON geometry.")
    }
    if (!x[["type"]] %in% spatial_types) {
        .error("GeoJSON type '%s' is not supported.", x[["type"]])
    }
    return(x)
}

#' @export
get_spatial.sf <- function(x) {
    get_spatial.sfc(sf::st_geometry(x))
}

#' @export
get_spatial.sfc <- function(x) {
    if (length(x) > 1) {
        x <- x[[1]]
        .warning("Informed geometry has multiple features. Using the first one.")
    }
    get_spatial.sfg(x[[1]])
}

#' @export
get_spatial.sfg <- function(x) {
    names(spatial_types) <- toupper(spatial_types)
    geom_type <- as.character(sf::st_geometry_type(x))
    if (!geom_type %in% names(spatial_types)) {
        .error("Geometry type '%s' is not supported.", geom_type)
    }
    geom_type <- spatial_types[[geom_type]]
    
    coords <- unclass(x)
    # unclass do not operates in recursive
    if (geom_type == "MultiPolygon") {
        coords <- .map(unclass(x), unclass)
    }
    
    return(
        list(type = geom_type, coordinates = coords)
    )
}

#' @export
get_spatial.default <- function(x) {
    .error("Invalid 'geom' parameter.")
}

.create_geom_lst <- function(longitude = NULL, latitude = NULL, geom) {
    if (!is.null(longitude) && !is.null(latitude)) {
        geom <- c("longitude" = longitude, "latitude" = latitude)
        set_class(geom) <- "point"
    }
    geom_name <- .default(names(geom), "")
    if (is.numeric(geom) && all(geom_name %in% bbox_headers)) {
        set_class(geom) <- "bbox"
    }
    return(get_spatial(geom))
}

.error <- function(..., .envir = parent.frame(1)) {
    stop(cli::format_error(paste(...), .envir = .envir), call. = FALSE)
}

.warning <- function(..., .envir = parent.frame(1)) {
    warning(
        cli::format_error(paste(...), .envir = .envir),
        call. = FALSE, 
        immediate. = TRUE
    )
}

.message <- function(..., .envir = parent.frame(1)) {
    message(cli::format_error(paste(...), .envir = .envir))
}
