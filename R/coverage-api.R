.get_coverages <- function(URL) {
    # Build url to send request
    req_url <- .build_url(url = URL, path = "list_coverages")
    # Send a request to the WTSS server
    response <- .get_request(req_url)
    # Parse response content
    parsed_response <- .parse_json(response)
    # Set coverages class
    set_class(parsed_response) <- "coverages"
    # Return the coverages 
    return(parsed_response)
}

.get_coverage_details <- function(URL, name) {
    # Build url to send request
    req_url <- .build_url(
        url = URL, path = "describe_coverage", query = list("name" = name)
    )
    # Send a request to the WTSS server
    response <- .get_request(req_url)
    # Parse response object
    parsed_response <- .parse_json(response)
    # Post-processing
    cov_tbl <- .wtss_coverage_description(URL, parsed_response)
    # Set class
    set_class(cov_tbl) <- c("describe_coverage", "sits_cube") 
    # Return ...
    return(cov_tbl)
}

#' @title Decodes the description from a WTSS coverage
#' @name  .wtss_coverage_description
#' 
#' @description creates a tibble to store the description of the WTSS coverage
#' @param URL       URL of the coverage
#' @param cov       coverage response provided by WTSS service
.wtss_coverage_description <- function(URL, parsed_response){
    name <- parsed_response$name
    timeline <- lubridate::as_date(unlist(parsed_response$timeline))
    bands <- .map_chr(parsed_response$attributes, `[[`, "name")
    
    attr_tbl <- .map_dfr(parsed_response$attributes, function(attr) {
        tibble::tibble(
            name = attr[["name"]],
            scale_factors  = attr[["scale_factor"]],
            missing_values = attr[["missing_value"]],
            minimum_values = attr[["valid_range"]][["min"]],
            maximum_values = attr[["valid_range"]][["max"]]
        )  
    })
    # Spatial extent
    xmin <- parsed_response$spatial_extent$xmin
    ymin <- parsed_response$spatial_extent$ymin
    xmax <- parsed_response$spatial_extent$xmax
    ymax <- parsed_response$spatial_extent$ymax
    
    # Spatial resolution
    xres <- parsed_response$spatial_resolution$x
    yres <- parsed_response$spatial_resolution$y
    
    # Size (rows and cols)
    nrows <- parsed_response$dimension$y$max_idx - parsed_response$dimensions$y$min_idx + 1
    ncols <- parsed_response$dimension$x$max_idx - parsed_response$dimensions$x$min_idx + 1
    
    # Projection CRS
    crs <- parsed_response$crs$proj4
    
    # retrieve the satellite associated to the cube
    sat_sensor <- .wtss_guess_satellite(xres)
    satellite  <- sat_sensor["satellite"]
    # retrieve the sensor associated to the cube
    sensor <- sat_sensor["sensor"]
    
    # create a tibble to store the metadata
    cov_tbl <- tibble::tibble(URL            = URL,
                              satellite      = satellite,
                              sensor         = sensor,
                              bands          = list(bands),
                              scale_factors  = list(attr_tbl[["scale_factors"]]),
                              missing_values = list(attr_tbl[["missing_values"]]),
                              minimum_values = list(attr_tbl[["minimum_values"]]),
                              maximum_values = list(attr_tbl[["maximum_values"]]),
                              timeline       = list(timeline),
                              nrows          = nrows,
                              ncols          = ncols,
                              xmin           = xmin,
                              xmax           = xmax,
                              ymin           = ymin,
                              ymax           = ymax,
                              xres           = xres,
                              yres           = yres,
                              crs            = crs)
    return(cov_tbl)
}

#' @title Try a best guess for the type of sensor/satellite
#' @name .wtss_guess_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description    Based on resolution, tries to guess what is the satellite.
#'
#' @param xres      xres of the coverage
#' @return          Satellite sensor pair
.wtss_guess_satellite <- function(xres) {
    if (xres < 1.0 ) {
        # assume that xres is given in decimal degrees
        # approximate resolution of the coverage 
        res_m <- geosphere::distGeo(p1 = c(0.0, 0.0), p2 = c(xres, 0.00))
    }
    else
        res_m <- xres
    
    #try to guess the satellite
    if (res_m > 200.0 && res_m < 2000.0) {
        sat_sensor <- c("TERRA", "MODIS")
    }
    else if (res_m > 60.00 && res_m < 80.0)
        sat_sensor <- c("CBERS", "AWFI")
    else if (res_m > 25.00 && res_m < 35.0)
        sat_sensor <- c("LANDSAT", "OLI")
    else if (res_m < 25.00 && res_m > 5.0)
        sat_sensor <- c("SENTINEL-2", "MSI")
    else 
        sat_sensor <- c("UNKNOWN", "UNKNOWN")
    
    names(sat_sensor) <-  c("satellite", "sensor")
    return(sat_sensor)
}
