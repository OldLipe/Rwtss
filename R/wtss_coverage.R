#' @title Decodes the description from a WTSS coverage
#' @name  .wtss_coverage_description
#' 
#' @description creates a tibble to store the description of the WTSS coverage
#' @param URL       URL of the coverage
#' @param cov       coverage response provided by WTSS service
.wtss_coverage_description <- function(URL, cov){
    # retrieve the name of the coverage
    if (is.null(cov$name)) {
        stop("The WTSS not provide the name item.")
    }
    
    name <- cov$name

    # temporal extent
    if (is.null(cov$timeline)) {
        stop("The WTSS not provide the timeline item.")    
    }
    
    timeline <- lubridate::as_date(cov$timeline)
    
    # retrieve information about the bands
    if (is.null(attributes)) {
        stop("The WTSS not provide the attributes item.")
    }
    
    attr_tbl <- tibble::as_tibble(cov$attributes)
    bands <- attr_tbl$name
    
    t <- dplyr::select(attr_tbl, name, missing_value, scale_factor, valid_range)
    missing_values        <- t$missing_value
    names(missing_values) <- t$name
    
    scale_factors         <- t$scale_factor
    names(scale_factors)  <- t$name
    
    minimum_values        <- t$valid_range$min
    names(minimum_values) <- t$name
    
    maximum_values        <- t$valid_range$max
    names(maximum_values) <- t$name
    
    # correct errors in scale factors
    if (all(scale_factors == 1))
        scale_factors <- 1/maximum_values
    
    # Spatial extent
    xmin <- cov$spatial_extent$xmin
    ymin <- cov$spatial_extent$ymin
    xmax <- cov$spatial_extent$xmax
    ymax <- cov$spatial_extent$ymax
    
    # Spatial resolution
    xres <- cov$spatial_resolution$x
    yres <- cov$spatial_resolution$y
    
    # Size (rows and cols)
    nrows <- cov$dimension$y$max_idx - cov$dimensions$y$min_idx + 1
    ncols <- cov$dimension$x$max_idx - cov$dimensions$x$min_idx + 1
    
    # Projection CRS
    crs <- cov$crs$proj4
    
    # retrieve the satellite associated to the cube
    sat_sensor <- .wtss_guess_satellite(xres)
    satellite  <- sat_sensor["satellite"]
    # retrieve the sensor associated to the cube
    sensor     <- sat_sensor["sensor"]
    
    # create a tibble to store the metadata
    cov.tb <- tibble::tibble(URL            = URL,
                             satellite      = satellite,
                             sensor         = sensor,
                             bands          = list(bands),
                             scale_factors  = list(scale_factors),
                             missing_values = list(missing_values),
                             minimum_values = list(minimum_values),
                             maximum_values = list(maximum_values),
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
    
    class(cov.tb) <- c("describe_cov", "sits_cube", class(cov.tb))
    
    return(cov.tb)
}

.wtss_print_coverage <- function(cov.tb){
    cat("---------------------------------------------------------------------")
    cat(paste0("\nWTSS server URL = ", cov.tb$URL, "\n"))
    cat(paste0("Cube (coverage) = ", cov.tb$name))
    
    print(knitr::kable(dplyr::select(cov.tb, satellite, sensor, bands), 
                       padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, scale_factors), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, minimum_values), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, maximum_values), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, nrows, ncols, xmin, xmax, ymin, 
                                     ymax, xres, yres, crs), padding = 0))
    # print the timeline
    timeline <- lubridate::as_date(cov.tb$timeline[[1]])
    n_time_steps <- length(timeline)
    cat(paste0("\nTimeline - ",n_time_steps," time steps\n"))
    cat(paste0("start_date: ", timeline[1], 
               " end_date: ", timeline[n_time_steps],"\n"))
    cat("-------------------------------------------------------------------\n")
}

#' @export 
print.describe_coverage <- function(cov.tb){
    cat("---------------------------------------------------------------------")
    cat(paste0("\nWTSS server URL = ", cov.tb$URL, "\n"))
    cat(paste0("Cube (coverage) = ", cov.tb$name))
    
    print(knitr::kable(dplyr::select(cov.tb, satellite, sensor, bands), 
                       padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, scale_factors), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, minimum_values), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, maximum_values), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, nrows, ncols, xmin, xmax, ymin, 
                                     ymax, xres, yres, crs), padding = 0))
    # print the timeline
    timeline <- lubridate::as_date(cov.tb$timeline[[1]])
    n_time_steps <- length(timeline)
    cat(paste0("\nTimeline - ",n_time_steps," time steps\n"))
    cat(paste0("start_date: ", timeline[1], 
               " end_date: ", timeline[n_time_steps],"\n"))
    cat("-------------------------------------------------------------------\n")
}

#' @title Retrieves the list of cubes from the URL server
#' @name .wtss_list_coverages
#'
#' @description Use the WTSS protocol to find out available coverages
#'
#' @param URL      URL of the WTSS service
#' @return              updated WTSS object.
.wtss_list_coverages <- function(URL) {
    # Build url to send request
    req_url <- .build_url(url = URL, path = "list_coverages")
    # Send a request to the WTSS server
    req_obj <- .wtss_process_request(req_url)
    # Was the response correct?
    if (is.null(req_obj)) {
        return(NULL)
    }
    return(req_obj$coverages)
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
