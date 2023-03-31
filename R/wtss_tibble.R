#' @title Create a sits tibble to store the time series information
#' @name .wtss_tibble
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description This function returns an empty tibble that
#' contains the satellite image time series and its metadata. The columns are
#' <longitude, latitude, start_date, end_date, label, cube, time_series>.
#' WTSS functions produce a tibble as output.
#' 
#' @return A tibble.
.wtss_tibble <- function() {
    result <- tibble::tibble(longitude   = double(),
                             latitude    = double(),
                             start_date  = as.Date(character()),
                             end_date    = as.Date(character()),
                             label       = character(),
                             cube        = character(),
                             time_series = list()
    )
    return(result)
}

# wtss_interp <- function(ts, name) {
#     # retrieve the time series information
#     time_series <- ts[[name]]$attributes
#     
#     # determine the missing value for each band
#     missing_values <- cov_desc$missing_values[[1]]
#     names(missing_values) <- cov_desc$bands[[1]]
#     
#     # update missing values to NA
#     bands %>%
#         purrr::map(function(b) {
#             time_series[, b][time_series[, b] == missing_values[b]] <<- NA
#         })
#     
#     # interpolate missing values
#     time_series[, bands] <- zoo::na.spline(time_series[, bands])
#     
#     # create a list to store the time series coming from the WTSS service
#     ts.lst <- list()
#     ts.lst[[1]] <- ts.tb
#     
#     # create a tibble to store the WTSS data
#     data <- .wtss_tibble()
# }
