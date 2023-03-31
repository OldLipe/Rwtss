#' @title  Generic interface for ploting time series
#' @method plot wtss
#' @name plot
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @description Given a tibble with a set of time series, plot them.
#'
#'
#' @param  x            object of class "wtss"
#' @param  y            ignored
#' @param ...           further specifications for \link{plot}.
#' @param  colors       Color pallete to be used (based on Color Brewer - 
#'                      default is "Dark2").
#' @return              Input  tibble (useful for chaining functions).
#'
#' @examples
#' \dontrun{
#' # Access to external service
#' # Read one time series from the WTSS server
#' # plot one time series
#' wtss_service <- "https://brazildatacube.dpi.inpe.br/wtss/"
#' ts   <- Rwtss::time_series(
#'                 wtss_service, 
#'                 name = "MOD13Q1-6", 
#'                 attributes = c("NDVI","EVI"), 
#'                 longitude = -45.00, 
#'                 latitude  = -12.00,
#'                 start_date = "2000-02-18", 
#'                 end_date = "2016-12-18",
#'                 token = "YOUR-BDC-TOKEN")
#' plot(ts)
#' }
#' @export
plot.wtss <- function(x, y, n_plots = 1, ..., colors = "Dark2") {
    g <- .map(seq_len(n_plots), function(i) {
        .ts_plot(x[i, ], colors)
    })
    # return the plot - useful for testing
    return(invisible(g))
}

#' @title Plot one timeSeries using ggplot
#'
#' @name .wtss_ggplot_series
#'
#' @description Plots a set of time series using ggplot. This function is used
#' for showing the same lat/long location in a series of time steps.
#'
#' @param row         A row of a sits tibble with the time series to be plotted.
#' @param colors      The set of Brewer colors to be used for plotting.
.ts_plot <- function(ts, colors = "Dark2") {
    # Create the plot title
    plot_title <- paste(
        "Location = (", ts[["latitude"]], ",", ts[["longitude"]], ")"
    )
    # Extract the time series
    data_ts <- ts$time_series[[1]]
    # melt the data into long format
    melted.ts <- data_ts %>%
        reshape2::melt(id.vars = "Index") %>%
        as.data.frame()
    # plot the data with ggplot
    g <- ggplot2::ggplot(melted.ts, 
                         ggplot2::aes(x = Index, y = value, group = variable)) +
        ggplot2::geom_line(ggplot2::aes(color = variable)) +
        ggplot2::labs(title = plot_title) +
        ggplot2::scale_color_brewer(palette = colors)
    graphics::plot(g)
    return(g)
}
