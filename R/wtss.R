#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Lists coverages available in the WTSS service.
#'
#' @param URL URL of the server
#' @return a \code{list} with the links and names of each coverage.
#' @examples
#' \dontrun{
#' list_coverages("https://brazildatacube.dpi.inpe.br/wtss/")
#' }
#' @export
list_coverages <- function(URL) {
    # Pre-condition
    .check_valid_url(URL)
    # Try to retrieve the coverage list
    coverages <- .get_covs(URL)
    # Return coverage list
    return(coverages)
}

#' @title Retrieves the list of cubes from the URL server
#' @name  describe_coverage
#'
#' @description Requests the WTSS server to describe one coverage.
#' @param URL   URL of the server.
#' @param name  name of coverage.
#' @return a \code{tibble} with coverage description.
#' 
#' @examples
#' \dontrun{
#' describe_coverage("https://brazildatacube.dpi.inpe.br/wtss/",
#'                   "LC8_30_16D_STK-1")
#' }
#' @export
describe_coverage <- function(URL, name) {
    # Pre-conditions
    .check_valid_url(URL)
    .check_chr(x = name, len_min = 1, len_max = 1)
    # Try to retrieve the details of a coverage
    result <- .get_cov_details(url = URL, name = name)
    # Return coverage metadata
    return(result)
}

pixel_strategies <- c(
    "center", "upperLeft", "upperRight", "lowerLeft", "lowerRight"
)

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
#'                token = "change-me"
#' )
#' # plot the time series
#' plot(ndvi_ts)
#' }
#'@export
time_series <- function(URL,
                        name,
                        geom = NULL, 
                        longitude = NULL,
                        latitude = NULL,
                        attributes = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        pagination = NULL,
                        scale = FALSE,
                        pixel_strategy = "center",
                        token,  ...) {
    # Pre-conditions
    .check_valid_url(URL)
    .check_chr(name, len_max = 1)
    .check_spatial(lon = longitude, lat = latitude, geom = geom)
    .check_temporal(start_date, end_date)
    .check_chr(attributes, is_null = TRUE)
    .check_chr(pixel_strategy, within = pixel_strategies, len_max = 1)
    .check_lgl(scale, is_null = TRUE)
    .check_period(pagination)
    .check_chr(token, len_max = 1)
    # Get time series
    timeseries <- .retrieve_timeseries(
        url = URL,
        name = name, 
        geom = geom,
        longitude = longitude,
        latitude = latitude,
        attributes = attributes,
        start_date = start_date,
        end_date = end_date,
        pagination = pagination,
        scale = scale,
        pixel_strategy = pixel_strategy,
        token = token, ...
    )
    # Return extracted time series
    return(timeseries)
}

#' @title Gets the aggregated time series for an geometry.
#' @name summarize
#' @description ... 
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
#'                token = "change-me"
#' )
#' # plot the time series
#' plot(ndvi_ts)
#' }
#'@export
summarize <- function(URL,
                      name,
                      geom = NULL,
                      attributes = NULL,
                      aggregations = NULL,
                      start_date = NULL,
                      end_date = NULL,
                      scale = FALSE,
                      pixel_strategy = "center",
                      qa_values = NULL,
                      masked = NULL,
                      token = NULL, ...) {
    
    # Pre-conditions
    .check_valid_url(URL)
    .check_chr(x = name, len_min = 1, len_max = 1)
    .check_chr(x = attributes)
    .check_chr(
        x = aggregations, 
        within = c("min", "max", "mean", "median", "std"),
        len_min = 1,
        len_max  = 5
    )
    .check_lgl(x = scale)
    .check_lgl(x = masked, is_null = TRUE)
    .check_chr(
        x = pixel_strategy, 
        within = c("center", "upperLeft", "upperRight", 
                   "lowerLeft", "lowerRight"),
        len_min = 1,
        len_max  = 1
    )
    .check_locations(geom = geom)
    .check_date(
        x = start_date, 
        min_date = start_date, 
        max_date = end_date, 
        is_null = TRUE
    )
    .check_chr(x = token, len_min = 1, len_max = 1)
    # Get time series
    summarized_ts <- .summarize_timeseries(
        url = URL,
        name = name, 
        geom = geom,
        attributes = attributes,
        aggregations = aggregations,
        start_date = start_date,
        end_date = end_date,
        scale = scale,
        pixel_strategy = pixel_strategy,
        qa_values = qa_values,
        mask = masked,
        token = token, ...
    )
    # Return the extracted time series
    return(summarized_ts)
}

#' Get name attribute from wtss objects
#' 
#' @param x a `describe_coverage` and object 
#' 
#' @return ...
#'   
#' @export
wtss_name <- function(x) {
    # verify objects
    .check_s3_class(x, c("describe_coverage", "sits"))
    return(.get_name(x))
}

#' @export
.get_name <- function(x) {
    UseMethod(".get_name", x)
}

#' @export
.get_name.describe_coverage <- function(x) {
    return(.default(x[["name"]], ""))
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

#' @export
.get_bands <- function(x) {
    UseMethod(".get_bands", x)
}

#' @export
.get_bands.describe_coverage <- function(x) {
    return(x[["bands"]][[1]])
}

#' @export
.get_bands.sits <- function(x) {
    col_names <- unlist(.map(x[["time_series"]], colnames), recursive = FALSE)
    return(setdiff(col_names, "Index"))
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

#' @export
.get_timeline <- function(x) {
    UseMethod(".get_timeline", x)
}

#' @export
.get_timeline.describe_coverage <- function(x) {
    lubridate::as_date(x[["timeline"]][[1]])
}

#' @export
.get_bands.sits <- function(x) {
    timeline <- unlist(unique(.map(x[["time_series"]], `[[`, "Index")))
    return(lubridate::as_date(timeline))
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

#' @export
.get_bbox <- function(x) {
    UseMethod(".get_bbox", x)
}

#' @export
.get_bbox.describe_coverage <- function(x) {
    bbox <- c(xmin = x[["xmin"]], xmax = x[["xmax"]], 
              ymin = x[["ymin"]], ymax = x[["ymax"]]
    )
    return(bbox)
}

#' @export
.get_bbox.sits <- function(x) {
    bbox <- c(xmin = min(x[["longitude"]]), xmax = max(x[["longitude"]]),
      ymin = min(x[["latitude"]]), ymax = max(x[["latitude"]])
    )
    return(bbox)
}

.format_bbox <- function(x) {
    pts <- unlist(x$extent$coordinates, recursive = FALSE)
    c(xmin = pts[[1]][[1]], ymin = pts[[1]][[2]],
      xmax = pts[[3]][[1]], ymax = pts[[3]][[2]])
}
