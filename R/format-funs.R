.format_datetime <- function(x) {
    if (grepl("*T\\d{2}:\\d{2}:\\d{2}Z$", x = x)) {
        return(x)
    }
    paste0(lubridate::as_date(x), "T00:00:00Z")
}

.format_bbox <- function(x) {
    pts <- unlist(x$extent$coordinates, recursive = FALSE)
    c(xmin = pts[[1]][[1]], ymin = pts[[1]][[2]],
      xmax = pts[[3]][[1]], ymax = pts[[3]][[2]])
}