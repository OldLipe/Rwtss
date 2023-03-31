#' @export
print.coverages <- function(x) {
    cli::cli_h1("Coverages")
    cli::cli_li("WTSS Version:")
    cli::cli_ul("{.field {x$wtss_version}}")
    cli::cli_li("Collections:")
    cli::cli_ul()
    col_idx <- .map_lgl(x[["links"]], function(item) item[["rel"]] == "data")
    coverages <- .map_chr(x[["links"]][col_idx], `[[`, "title")
    for (coverage in coverages) {
        cli::cli_ul("{.field {coverage}}")
    }
    return(invisible(x))    
}

#' @export 
print.describe_coverage <- function(cov_tb){
    cli::cli_h1("Describe Coverage")
    cli::cli_alert_success("WTSS server URL = {cov_tb$URL}")
    cli::cli_li("Satellite = {cov_tb$satellite}")
    cli::cli_li("Sensor = {cov_tb$sensor}")
    
    print(knitr::kable(
        dplyr::select(
            cov_tb, .data[["satellite"]], .data[["sensor"]], .data[["bands"]]
        ), padding = 0)
    )
    print(knitr::kable(dplyr::select(cov_tb, .data[["scale_factors"]]), padding = 0))
    print(knitr::kable(dplyr::select(cov_tb, .data[["minimum_values"]]), padding = 0))
    print(knitr::kable(dplyr::select(cov_tb, .data[["maximum_values"]]), padding = 0))
    print(knitr::kable(dplyr::select(cov_tb, .data[["nrows"]], .data[["ncols"]],
                                     .data[["xmin"]], .data[["xmax"]], .data[["ymin"]], 
                                     .data[["ymax"]], .data[["xres"]], 
                                     .data[["yres"]], .data[["crs"]]), padding = 0))
    # print the timeline
    timeline <- cov_tb$timeline[[1]]
    cli::cli_li("Timeline length:  {length(timeline)}")
    cli::cli_li("Start Date:  {min(timeline)}")
    cli::cli_li("End Date:  {max(timeline)}")
}