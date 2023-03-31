#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Lists the data products made available by the 
#'  WTSS service for extracting and summarizing time series.
#'
#' @param URL a \code{character} with URL of the server.
#' 
#' @return a \code{list} with the links and names of each coverage.
#' @examples
#' \dontrun{
#' list_coverages("https://brazildatacube.dpi.inpe.br/dev/wtss/v2/")
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

#' @title Retrieves the metadata from one coverage
#' @name  describe_coverage
#'
#' @description Requests the WTSS server to get the metadata of one coverage.
#' The metadata includes minimum, maximum and missing values. 
#' Among other information from the coverage.
#' 
#' @param URL  a \code{character} with URL of the server.
#' @param name a \code{character} with the coverage name.
#' 
#' @return a \code{tibble} with coverage description.
#' 
#' @examples
#' \dontrun{
#' describe_coverage("https://brazildatacube.dpi.inpe.br/dev/wtss/v2/",
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

#' @title Extract time series from WTSS service
#' @name time_series
#' @author  Gilberto Camara
#' @description Retrieves the time series for a pair of coordinates 
#' 
#' @param URL        a \code{character} with URL of the server.
#' @param name       a \code{character} with the coverage name.
#' @param geom       a \code{list}, \code{character}, \code{bbox}, 
#' \code{sf}, \code{sfc}, \code{sfg}, representing the geometry to be request.
#' It is supported the following geometries type: \code{Point}, 
#' \code{MultiPoint}, \code{Polygon}, \code{MultiPolygon}. 
#' All geometries should be provided in WGS 84 coordinate system. 
#' See details below.
#' @param longitude  a \code{numeric} with longitude in WGS84 coordinate system.
#' @param latitude   a \code{numeric} with latitude in WGS84 coordinate system.
#' @param attributes a \code{character} with the bands names.
#' @param start_date a \code{character} with the start date in 
#'  format yyyy-mm-dd.
#' @param end_date   a \code{character} with the end date in format yyyy-mm-dd.
#' @param pagination a \code{character} with the period of pagination follwing
#' ISO8601-compliant time period, with number and unit, where "D", "M" and "Y"
#' stand for days, month and year; e.g., "P16D" for 16 days.
#' @param scale      a \code{logical} to apply the attribute scale factor and
#' offset along the time series values. Default is FALSE.
#' @param pixel_strategy a \code{character} with the geospatial method to 
#' retrieve pixel intersection. Options: "center", "upperLeft", "upperRight", 
#' "lowerLeft", "lowerRight".
#' @param token      a \code{character} with token to be add in URL.
#' @param ...        additional parameters that can be added in httr.
#' 
#' @details The parameter \code{geom} can be provided in several ways:
#' \itemize{ 
#' \item In \code{list} type, geometry should be represented as a GeoJSON, 
#'  having \code{type} and \code{coordinates} names. 
#' \item In \code{character} type, geometry should be provided as a GeoJSON.
#' \item In \code{bbox} type, geometry should be represented as a numeric 
#'  vector with the following names: \code{xmin, ymin, xmax, ymin}. 
#' \item Any object of \code{sf} package, such as \code{sf}, \code{sfc}, and
#' \code{sfg}, is supported.
#' }
#' 
#' @return time series in a tibble format.
#' 
#' @examples
#' \dontrun{
#' # connect to a WTSS server
#' wtss_server <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
#' # retrieve a time series for a point
#' l8_ts <- Rwtss::time_series(
#'                URL = wtss_server, 
#'                name = "LC8_30_16D_STK-1", 
#'                attributes = "NDVI", 
#'                latitude = -14.31, 
#'                longitude = -51.16,
#'                token = token
#' )
#' 
#' # Retrieve a time series for a bbox
#' geom <- c(xmin = -46.27916, ymin = -13.22281,
#'           xmax = -46.27816, ymax = -13.22181)
#' cbers_ts <- Rwtss::time_series(
#'                URL = wtss_server,
#'                name = "CB4_64_16D_STK-1",
#'                attributes = c("NDVI", "EVI"),
#'                geom = geom,
#'                start_date = "2020-01-01",
#'                end_date = "2020-02-01",
#'                token = token
#' )
#' 
#' # Retrieve a time series for a list
#' polygon <- list(
#'   type = "Polygon",
#'   coordinates = list(
#'     matrix(c(-46.27916, -13.22281,
#'              -46.27816, -13.22281,
#'              -46.27816, -13.22181,
#'              -46.27916, -13.22181,
#'              -46.27916, -13.22281),
#'            ncol = 2, byrow = TRUE)
#'   )
#' )
#' modis_ts <- Rwtss::time_series(
#'                URL = wtss_server,
#'                name = "MOD13Q1-6",
#'                attributes = c("NDVI", "EVI", "red_reflectance"),
#'                geom = polygon,
#'                start_date = "2018-01-01",
#'                end_date = "2020-02-01",
#'                scale = TRUE,
#'                pagination = "P1M",
#'                token = token
#' )
#' 
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

summarize_fns <-  c("min", "max", "mean", "median", "std")

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
                      geom,
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
    .check_chr(name, len_max = 1)
    .check_spatial(geom = geom)
    .check_chr(attributes, is_null = TRUE)
    .check_chr(aggregations, within = summarize_fns, len_max = 5)
    .check_lgl(x = scale, is_null = TRUE)
    .check_lgl(x = masked, is_null = TRUE)
    .check_chr(pixel_strategy, within = pixel_strategies, len_max = 1)
    .check_temporal(start_date, end_date)
    .check_chr(x = token, len_min = 1, len_max = 1)
    # Get summarized time series
    summarized_ts <- .sum_retrieve_summary(
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
    # Return the summarized time series
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
