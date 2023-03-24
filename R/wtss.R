#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Lists coverages available in the WTSS service 
#'
#' @param URL       URL of the server
#' @return          vector with coverage name
#' @examples
#' \dontrun{
#' # Using external server 
#' list_coverages("https://brazildatacube.dpi.inpe.br/wtss/")
#' }
#' @export
list_coverages <- function(URL) {
    # Pre-condition
    .check_valid_url(URL)
    # Try to retrieve the coverage list
    coverages <- .get_coverages(URL)
    # Return coverages
    return(coverages)
}

#' @title Retrieves the list of cubes from the URL server
#' @name  describe_coverage
#'
#' @description Contacts the WTSS server to describe one coverage
#' @param URL         URL of the server
#' @param name        name of coverage
#' @param .print      Print the coverage description
#' @return            tibble with coverage description
#' 
#' @examples
#' \dontrun{
#' # Using external server 
#' describe_coverage("https://brazildatacube.dpi.inpe.br/wtss/", 
#'                   "LC8_30_16D_STK-1")
#' }
#' @export
describe_coverage <- function(URL, name) {
    # Pre-conditions
    .check_valid_url(URL)
    .check_chr(x = name, len_min = 1, len_max = 1)
    # Try to retrieve the details of a coverage
    result <- .get_coverage_details(URL = URL, name = name)
    # Return ...
    return(result)
}

#' @title Get time series
#' @name time_series
#' @author  Gilberto Camara
#' @description Retrieves the time series for a pair of coordinates 
#' 
#' @param URL           URL of the server
#' @param name          Coverage name.
#' @param attributes    Vector of band names.
#' @param longitude     Longitude in WGS84 coordinate system.
#' @param latitude      Latitude in WGS84 coordinate system.
#' @param start_date    Start date in the format yyyy-mm-dd or yyyy-mm 
#'                      depending on the coverage.
#' @param end_date      End date in the format yyyy-mm-dd or yyyy-mm 
#'                      depending on the coverage.
#' @param token         A character with token to be add in URL.
#' @param ...           Additional parameters that can be added in httr.
#' @return              time series in a tibble format (NULL)
#' @examples
#' \dontrun{
#' # connect to a WTSS server
#' wtss_server <- "https://brazildatacube.dpi.inpe.br/wtss/"
#' # retrieve a time series
#' ndvi_ts <- Rwtss::time_series(
#'                URL = wtss_server, 
#'                name = "LC8_30_16D_STK-1", 
#'                attributes = "NDVI", 
#'                latitude = -14.31, 
#'                longitude = -51.16,
#'                token = "6SzGipAIMwhkepwVE3fRH7n77nwvZW7ttu18mK3ZiS"
#' )
#' # plot the time series
#' plot(ndvi_ts)
#' }
#'@export
time_series <- function(URL,
                        name,
                        attributes = NULL,
                        longitude,
                        latitude,
                        start_date = NULL,
                        end_date   = NULL,
                        token = NULL,
                        ...) {
    # Pre-conditions
    .check_valid_url(URL)
    .check_chr(x = name, len_min = 1, len_max = 1)
    .check_chr(x = attributes)
    .check_locations(longitude = longitude, latitude = latitude)
    .check_date(date = start_date, min = start_date, max = end_date)
    .check_chr(x = token, len_min = 1, len_max = 1)
    # Have we described the coverage before?
    if (is.null(wtss.env$desc) || .get_name(wtss.env$desc) != name) {
        wtss.env$desc <- describe_coverage(URL, name)
        # if describe_coverage fails, return a default time series
        # TODO: verificar se chega nulo aqui
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
    # .check_date(start_date, min = min(timeline), max = max(timeline))
    # .check_date(end_date, min = min(timeline), max = max(timeline))
    
    # Get time series
    items <- .get_timeseries(
        URL = URL,
        name = name, 
        attributes = attributes,
        longitude = longitude,
        latitude = latitude,
        start_date = start_date,
        end_date = end_date,
        token = token
    )
    # Return ...
    return(items)
}

#' Get .... from wtss objects
#' 
#' General function ...
#' 
#' @param x ...
#' 
#' @return ...
#'   
#' @export
wtss_name <- function(x) {
    # verify objects
    .check_s3_class(x, c("describe_coverage", "sits"))
    return(.get_name(x))
}

.get_name <- function(x) {
    UseMethod(".get_name", x)
}

.get_name.describe_coverage <- function(x) {
    x[["name"]]
}

.get_name.sits <- function(x) {
    return(invisible(x))
}

#' Get .... from wtss objects
#' 
#' General function ...
#' 
#' @param x ...
#' 
#' @return ...
#'   
#' @export
wtss_bands <- function(x) {
    .check_s3_class(x, c("describe_coverage", "sits"))
    return(.get_bands(x))
}

.get_bands <- function(x) {
    UseMethod(".get_bands", x)
}

.get_bands.describe_coverage <- function(x) {
    x[["bands"]][[1]]
}

.get_bands.sits <- function(x) {
    return(invisible(x))
}

#' Get .... from wtss objects
#' 
#' General function ...
#' 
#' @param x ...
#' 
#' @return ...
#' 
#' @export
wtss_timeline <- function(x) {
    .check_s3_class(x, c("describe_coverage", "sits"))
    return(.get_timeline(x))
}

.get_timeline <- function(x) {
    UseMethod(".get_timeline", x)
}

.get_timeline.describe_coverage <- function(x) {
    lubridate::as_date(x[["timeline"]][[1]])
}

.get_bands.sits <- function(x) {
    return(invisible(x))
}

#' Get .... from wtss objects
#' 
#' General function ...
#' 
#' @param x ...
#' 
#' @return ...
#' 
#' @export
wtss_bbox <- function(x) {
    .check_s3_class(x, c("describe_coverage", "sits"))
    return(.get_bbox(x))
}

.get_bbox <- function(x) {
    UseMethod(".get_bbox", x)
}

.get_bbox.describe_coverage <- function(x) {
    c(xmin = x[["xmin"]], xmax = x[["xmax"]], 
      ymin = x[["ymin"]], ymax = x[["ymax"]])
}

.get_bbox.sits <- function(x) {
    return(invisible(x))
}
