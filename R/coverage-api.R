.get_covs <- function(url) {
    # Build url to send request
    req_url <- .build_url(url = url, path = "/")
    # Send a request to the WTSS server
    response <- .get_request(req_url)
    # Parse response content
    parsed_response <- .parse_json(response)
    # Set coverages class
    set_class(parsed_response) <- "coverages"
    # Return the coverages 
    return(parsed_response)
}

.get_cov_details <- function(url, name) {
    # Have we described the coverage before?
    if (!is.null(wtss.env$desc) && .get_name(wtss.env$desc) == name) {
        return(wtss.env$desc)
    }
    # Build url to send request
    req_url <- .build_url(url = url, path = name)
    # Send a request to the WTSS server
    response <- .get_request(req_url)
    # Parse response object
    parsed_response <- .parse_json(response)
    # Post-processing
    cov_tbl <- .get_cov_metadata(url, parsed_response)
    # Set class
    set_class(cov_tbl) <- c("describe_coverage", "sits_cube") 
    # Save ...
    wtss.env$desc <- cov_tbl
    # Return ...
    return(cov_tbl)
}

.get_cov_metadata <- function(URL, parsed_response){
    timeline <- lubridate::as_date(unlist(parsed_response$timeline))
    bands <- .map_chr(parsed_response$bands, `[[`, "name")
    
    bds_tbl <- .map_dfr(parsed_response$bands, function(band) {
        tibble::tibble(
            name       = band[["name"]],
            scale      = band[["scale"]],
            missing    = band[["nodata"]],
            min_values = band[["min_value"]],
            max_values = band[["max_value"]]
        )  
    })
    
    # Spatial extent
    bbox <- .format_bbox(parsed_response)
    
    # Spatial resolution
    xres <- unique(.map_dbl(parsed_response$bands, `[[`, "resolution_x"))
    yres <- unique(.map_dbl(parsed_response$bands, `[[`, "resolution_y"))
    
    # Size (rows and cols)
    size <- c(
        nrows = parsed_response$raster_size$y, 
        ncols = parsed_response$raster_size$x
    ) 
    
    # Projection CRS
    crs <- parsed_response[["bdc:crs"]]
    
    # retrieve the satellite associated to the cube
    sat_sensor <- .cov_guess_sat(xres)
    satellite  <- sat_sensor["satellite"]
    # retrieve the sensor associated to the cube
    sensor <- sat_sensor["sensor"]
    
    # create a tibble to store the metadata
    cov_tbl <- tibble::tibble(URL            = URL,
                              satellite      = satellite,
                              sensor         = sensor,
                              bands          = list(bands),
                              scale_factors  = list(bds_tbl[["scale"]]),
                              missing_values = list(bds_tbl[["missing"]]),
                              minimum_values = list(bds_tbl[["min_values"]]),
                              maximum_values = list(bds_tbl[["max_values"]]),
                              timeline       = list(timeline),
                              nrows          = size[["nrows"]],
                              ncols          = size[["ncols"]],
                              xmin           = bbox[["xmin"]],
                              xmax           = bbox[["xmax"]],
                              ymin           = bbox[["ymin"]],
                              ymax           = bbox[["ymax"]],
                              xres           = xres,
                              yres           = yres,
                              crs            = crs)
    return(cov_tbl)
}

.cov_guess_sat <- function(xres) {
    #try to guess the satellite
    if (xres > 200.0 && xres < 2000.0) {
        sat_sensor <- c("TERRA", "MODIS")
    }
    else if (xres > 60.00 && xres < 80.0)
        sat_sensor <- c("CBERS", "AWFI")
    else if (xres > 25.00 && xres < 35.0)
        sat_sensor <- c("LANDSAT", "OLI")
    else if (xres < 25.00 && xres > 5.0)
        sat_sensor <- c("SENTINEL-2", "MSI")
    else 
        sat_sensor <- c("UNKNOWN", "UNKNOWN")
    
    names(sat_sensor) <-  c("satellite", "sensor")
    return(sat_sensor)
}
