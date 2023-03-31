.check_valid_url <- function(URL) {
    .is_chr(URL)    
    .get_request(URL, n_tries = 1)
}

.check_chr <- function(x, 
                       within = NULL, 
                       within_type = "all", 
                       len_min = 1, 
                       len_max = Inf,
                       is_null = FALSE) {
    if (is.null(x) && is_null) {
        return(.check_null(x, is_null = is_null))
    }
    if (!.is_chr(x)) {
        .error("Invalid {.var {x}} value.",
               "Expected a {.cls character} parameter.",
               .envir = environment())
    }
    if (length(x) < len_min || length(x) > len_max) {
        .error(
            "invalid length size of {.var {x}} values.", .envir = environment()
        )
    }
    if (!is.null(within)) {
        switch(
            within_type, 
            all = .all_elem_within(x, within),
            any = .any_elem_within(x, within),
            none = .none_elem_within(x, within),
            NULL
        )
    }
}

.all_elem_within <- function(x, within) {
    if (!all(x %in% within)) {
        .error("Value {.var {x}} is not in",
               "{.var {within}}", .envir = environment())
    }
}

.any_elem_within <- function(x, within) {
    if (!any(x %in% within)) {
        .error("No value {.var {x}} in {.var {within}}", 
               .envir = environment())
    }
}

.none_elem_within <- function(x, within) {
    if (any(x %in% within)) {
        .error("Some values {.var {x}} in {.var {within}}", 
               .envir = environment())
    }
}

.check_null <- function(x, is_null = FALSE) {
    if (is.null(x)) {
        if (!is_null) {
            .error("Invalid NULL parameter.")
        }
    }
}

.check_num <- function(x, len_min = 1, len_max = Inf, is_null = FALSE) {
    if (is.null(x) && is_null) {
        return(.check_null(x = x, is_null = is_null))
    }
    if (!.is_num(x)) {
        .error("Invalid parameter. Expected a {.cls numeric} parameter.")
    }
    if (length(x) < len_min || length(x) > len_max) {
        .error("Invalid length size of value(s). Length {.var {length(x)}}",
               "is greater than {.var {len_min}} or less than",
               "{.var {len_max}}.", .envir = environment())
    }
}

.check_temporal <- function(start_date, end_date) {
    if (is.null(start_date) && is.null(end_date)) {
        return(NULL)
    }
    if (!is.null(start_date) &&
        !(.is_date(start_date) || .is_chr(start_date))) {
        .error("Invalid 'start_date' parameter.",
               "Must be a 'character' or 'date' type.")
    }
    if (!is.null(end_date) &&
        !(.is_date(end_date) || .is_chr(end_date))) {
        .error("Invalid 'end_date' parameter.",
               "Must be a 'character' or 'date' type.")
    }
}

.check_interval <- function(start_date, end_date, cov) {
    timeline <- .get_timeline(cov)
    if (start_date < min(timeline) || end_date > max(timeline)) {
        .error(
            "The provided {.var start_date} or {.var end_date} is not",
            "within the time interval of the collection.",
            "i" = "Collection interval: {.cls {min(timeline)}/{max(timeline)}}"
        )
    }
}

.check_spatial <- function(lon = NULL, lat = NULL, geom = NULL) {
    if (!is.null(lon) && !is.null(lon) && !is.null(geom)) {
        .error("Invalid parameters. Just provide {.var longitude} and",
               "{.var latitude} or {.var geom}, separately.")
    }
    .check_num(lon, len_min = 1, len_max = 1, is_null = TRUE)
    .check_num(lat, len_min = 1, len_max = 1, is_null = TRUE)
    
    if (!is.null(lon) && !is.null(lat)) {
        if (lat < -90.0 || lat > 90.0) {
            .error("{.var latitude} is out-of range {.cls [-90,90]}")
        }
        if (lon < -180.0 || lon > 180.0) {
            .error("{.var longitude} is out-of range {.cls [-180,180]}")
        }    
    }
}

.check_lgl <- function(x, is_null = FALSE) {
    if (is.null(x) && is_null) {
        return(.check_null(x, is_null = is_null))
    }
    if (!is.logical(x)) {
        .error("Value {.var {x}} is not logical.",
               "Expect a {.cls logical} value.", .envir = environment())
    }
}

.check_period <- function(pagination) {
    .check_chr(pagination, len_max = 1, is_null = TRUE)
    if (is.null(pagination)) {
        return(NULL)
    }
    if (!grepl("^P[0-9]+[DMY]$", pagination)) {
        .error("Value {.var {pagination}} is not a valid period.",
               "Use {.cls P} for period with time, e.g. {.cls P15D}",
               "for 15 days.", .envir = environment())
    }
}

.check_geom <- function(x, is_null = FALSE) {
    if (is.null(x) && is_null) {
        return(.check_null(x, is_null = is_null))
    }
    if (is.list(x) && !all(names(x) %in% c("type", "coordinates"))) {
        .error("Invalid geom object")
    }
    if (!inherits(x, c("sf", "sfc", "sfg", "character", "list"))) {
        .error("Invalid {.var geom} object")
    }
}

.is_chr <- function(x) {
    is.character(x)
}

.is_num <- function(x) {
    is.numeric(x)
}

.is_error <- function(x) {
    inherits(x, "error")
}

.is_tbl <- function(x) {
    inherits(x, c("tbl_df", "tbl", "data.frame"))
}

.is_date <- function(x) {
    inherits(x, "Date")
}

`set_class<-` <- function(x, value, append = TRUE) {
    .is_chr(value)
    if (append) {
        value <- c(value, class(x))
    }
    class(x) <- value
    return(x)
}

.default <- function(x, value) {
    if (!is.null(x)) {
        return(x)
    }
    return(value)
}

.check_bands <- function(x, y) {
    if (!all(x %in% y)) {
        .error("Attributes not available.")
    }
}

.check_bbox <- function(geom, cov) {
    cov_bbox <- .geom_as_sf(get_spatial.bbox(.get_bbox(cov)))
    geom_bbox <- .geom_as_sf(geom)
    is_within <- apply(
        sf::st_within(geom_bbox, cov_bbox, sparse = FALSE), 1, any
    )
    
    if (!is_within) {
        .error("The provided geometry does not in coverage extent.", 
               "Verify your query")
    }
}

.geom_as_sf <- function(geom) {
    geo_json <- jsonlite::toJSON(geom, auto_unbox = TRUE)
    sf::read_sf(geo_json, crs = 4326)
}

.check_s3_class <- function(x, classes) {
    if (inherits(x, classes)) {
        .error("Invalid object")
    }
}

.check_response <- function(request, response) {
    if (.is_error(response)) {
        stop(paste("The requested URL was not possible to reach:", request),
             call. = FALSE
        )
    }
    httr::stop_for_status(response)    
}

.check_timeseries <- function(timeseries) {
    if (!inherits(timeseries, "timeseries")) {
        .error("Invalid {.var timeseries} object.")
    }
    if (is.null(attr(timeseries, "response"))) {
        .error("Not a valid {.var timeseries} object.")
    }
}

.to_list <- function(x) {
    if (length(x) == 1) {
        return(list(x))
    }
    return(x)
}
