test_that("List coverages", {
    wtss <-  "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    cov_list <- list_coverages(wtss)
    expect_s3_class(cov_list, "coverages")
    expect_equal(capture.output(cov_list), character(0))
    expect_error(list_coverages("htttt:;;;;ddddd.com"))
})

test_that("Describe coverage", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    expect_error(describe_coverage(wtss, TRUE))
    expect_error(describe_coverage(wtss, 123))
    cov_desc <- describe_coverage(wtss, "MOD13Q1-6")
    output <- capture.output(cov_desc)
    expect_true(as.logical(grep("TERRA", output[5])))
    expect_true(as.logical(grep("maximum_values", output[18])))
    expect_true(as.logical(grep("minimum_values", output[13])))
    expect_true(as.logical(grep("nrows", output[23])))
    expect_s3_class(cov_desc, "describe_coverage")
    expect_error(describe_coverage(wtss, "MOD13Q1dddd"))
    
})

test_that("Test time series check", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = -45.00,
            latitude  = -12.00,
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = -45.00,
            latitude  = -12.00,
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "Center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = -45.00,
            latitude  = -12.00,
            geom = "dddd",
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "Center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = "-45.00",
            latitude  = -12.00,
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "Center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = -45.00,
            latitude  = "-12.00",
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "Center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = -45.00,
            latitude  = 100,
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "Center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = 181,
            latitude  = 12.00,
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = 92,
            latitude  = 12.00,
            start_date = "2000-02-18T00:00:00Z",
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = 92,
            latitude  = 12.00,
            start_date = TRUE,
            end_date = "2016-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = c("NDVI","EVI"),
            longitude = 92,
            latitude  = 12.00,
            start_date = "2016-12-18T00:00:00Z",
            end_date = FALSE,
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            attributes = TRUE,
            longitude = 92,
            latitude  = 12.00,
            start_date = "2016-12-18T00:00:00Z",
            end_date = "2018-12-18T00:00:00Z",
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            longitude = 92,
            latitude  = 12.00,
            start_date = "2016-12-18T00:00:00Z",
            end_date = "2018-12-18T00:00:00Z",
            scale = 123,
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            longitude = 92,
            latitude  = 12.00,
            start_date = "2016-12-18T00:00:00Z",
            end_date = "2018-12-18T00:00:00Z",
            pagination = 1234,
            token = token,
            pixel_strategy = "center"
        )
    )
    
    expect_error(
        time_series(
            URL = wtss, 
            name = "MOD13Q1-6", 
            longitude = 92,
            latitude  = 12.00,
            start_date = "2016-12-18T00:00:00Z",
            end_date = "2018-12-18T00:00:00Z",
            pagination = "15D",
            token = token,
            pixel_strategy = "center"
        )
    )
})

test_that("Extract time series with a point", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    ts <- time_series(
        URL = wtss, 
        name = "MOD13Q1-6", 
        attributes = c("NDVI","EVI"),
        longitude = -45.00,
        latitude  = -12.00,
        start_date = "2000-02-18T00:00:00Z",
        end_date = "2016-12-18T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts$time_series[[1]]) == 388)
    expect_true(ncol(ts$time_series[[1]]) == 3)
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with a bbox", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    bbox <- c(xmin = -46.27916555820602, ymin = -13.22281233240166,
              xmax = -46.27816555820602, ymax = -13.22181233240166)
    
    ts <- time_series(
        URL = wtss, 
        name = "LC8_30_16D_STK-1", 
        attributes = c("NDVI","EVI", "band1", "band2", "Fmask4"),
        geom = bbox,
        start_date = "2019-01-01T00:00:00Z",
        end_date = "2019-01-30T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts) == 25)
    expect_true(ncol(ts$time_series[[1]]) == 6)
    cols <- c("Index", "NDVI", "EVI", "band1", "band2", "Fmask4")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with a list", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    x <- c(xmin = -46.27916555820602, ymin = -13.22281233240166,
           xmax = -46.27816555820602, ymax = -13.22181233240166)
    geom <- list(
        type = "Polygon",
        coordinates = list(
            rbind(
                c(x[[1]], x[[2]]),
                c(x[[3]], x[[2]]),
                c(x[[3]], x[[4]]),
                c(x[[1]], x[[4]]),
                c(x[[1]], x[[2]])
            )
        )
    )
        
    ts <- time_series(
        URL = wtss, 
        name = "S2-16D-2", 
        attributes = c("NDVI","EVI", "B12", "SCL"),
        geom = geom,
        start_date = "2021-01-01T00:00:00Z",
        end_date = "2021-01-30T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts) == 156)
    expect_true(ncol(ts$time_series[[1]]) == 5)
    cols <- c("Index", "NDVI", "EVI", "B12", "SCL")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with a character", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    geojson <- '{"type":"Polygon","coordinates":[[[-46.01720037136428,-12.255091513953374],[-46.01720037136428,-12.263395001098644],[-46.00633413519827,-12.263395001098644],[-46.00633413519827,-12.255091513953374],[-46.01720037136428,-12.255091513953374]]]}'
    
    ts <- time_series(
        URL = wtss, 
        name = "CB4-16D-2", 
        attributes = c("NDVI", "EVI", "BAND13", "CMASK"),
        geom = geojson,
        start_date = "2021-01-01T00:00:00Z",
        end_date = "2021-01-30T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts) == 320)
    expect_true(ncol(ts$time_series[[1]]) == 5)
    cols <- c("Index", "NDVI", "EVI", "BAND13", "CMASK")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with a sfg", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    sfg_obj <- sf::st_point(
        x = c(-46.01720037136428,-12.255091513953374)
    )
    
    ts <- time_series(
        URL = wtss, 
        name = "CB4_64_16D_STK-1", 
        attributes = c("NDVI", "EVI", "BAND13", "CMASK"),
        geom = sfg_obj,
        start_date = "2021-01-01T00:00:00Z",
        end_date = "2021-01-30T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts) == 1)
    expect_true(ncol(ts$time_series[[1]]) == 5)
    cols <- c("Index", "NDVI", "EVI", "BAND13", "CMASK")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with a sfc", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    sfg_obj <- sf::st_multipoint(
        x = rbind(c(-46.01720037136428, -12.255091513953374),
                  c(-46.02720037136428, -12.135091513953374))
    )
    sfc_obj <- sf::st_sfc(sfg_obj, crs = 4326)
    
    ts <- time_series(
        URL = wtss, 
        name = "CB4_64_16D_STK-1", 
        attributes = c("NDVI", "EVI", "BAND13", "CMASK"),
        geom = sfc_obj,
        start_date = "2021-01-01T00:00:00Z",
        end_date = "2021-01-30T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts) == 2)
    expect_true(ncol(ts$time_series[[1]]) == 5)
    cols <- c("Index", "NDVI", "EVI", "BAND13", "CMASK")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with a sf", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    poly1 <- sf::st_polygon(
        list(matrix(c(-46.224817403670386,
                      -12.738767771072162,
                      -46.223234785139255,
                      -12.74593185063361,
                      -46.214144873572906,
                      -12.738411540835301,
                      -46.224817403670386,
                      -12.738767771072162), ncol = 2, byrow = TRUE))
    )
        
    poly2 <- sf::st_polygon(
        list(matrix(c(-46.22165216660727,
                 -12.739915620650933,
                 -46.2219768063062,
                 -12.742013401274093,
                 -46.219623168489875,
                 -12.740311429644564,
                 -46.22165216660727,
                 -12.739915620650933), ncol = 2, byrow = TRUE))
    )
    
    multi_poly <- sf::st_as_sf(
        sf::st_sfc(sf::st_multipolygon(list(poly1, poly2)), crs = 4326)
    )
    
    ts <- time_series(
        URL = wtss, 
        name = "MYD13Q1-6", 
        attributes = c("NDVI", "EVI", "blue_reflectance", "red_reflectance"),
        geom = multi_poly,
        start_date = "2021-01-01T00:00:00Z",
        end_date = "2021-01-30T00:00:00Z",
        token = token
    )
    
    expect_true(nrow(ts) == 30)
    expect_true(ncol(ts$time_series[[1]]) == 5)
    cols <- c("Index", "NDVI", "EVI", "blue_reflectance", "red_reflectance")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_s3_class(ts, "timeseries")
})

test_that("Extract time series with pagination", {
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    pt <- sf::st_point(c(-46.22165216660727, -12.739915620650933))
    
    ts <- time_series(
        URL = wtss,
        name = "MYD13Q1-6",
        attributes = c("NDVI", "EVI", "blue_reflectance", "red_reflectance"),
        geom = pt,
        start_date = "2003-03-01T00:00:00Z",
        end_date = "2015-01-30T00:00:00Z",
        pagination = "P1Y",
        token = token
    )
    
    expect_true(nrow(ts) == 1)
    expect_true(ncol(ts$time_series[[1]]) == 5)
    cols <- c("Index", "NDVI", "EVI", "blue_reflectance", "red_reflectance")
    expect_true(all(names(ts$time_series[[1]]) %in% cols))
    expect_equal(nrow(ts$time_series[[1]]), 23)
    
    ts_2 <- next_timeseries(ts)
    expect_equal(nrow(ts_2$time_series[[1]]), 46)
    expect_s3_class(ts_2, "timeseries")
    
    ts_all <- fetch_timeseries(ts_2)
    expect_equal(nrow(ts_all$time_series[[1]]), 274)
    expect_s3_class(ts_all, "timeseries")
})

test_that("Plot",{
    wtss <- "https://brazildatacube.dpi.inpe.br/dev/wtss/v2/"
    token <- Sys.getenv("BDC_ACCESS_KEY")
    testthat::skip_if(!nzchar(token), "not avaliable token")
    
    tsg <- time_series(wtss, "MOD13Q1-6",
                       longitude = -45.00, 
                       latitude  = -12.00,
                       attributes = c("NDVI", "EVI"),
                       start_date = "2000-02-18T00:00:00Z",
                       end_date = "2002-02-18T00:00:00Z",
                       token = token)
    g <- plot(tsg)
    expect_true(unname(summary(g)[1,2]) == "gg")
})

test_that("Guess satellite",{
    sat_sensor <- .cov_guess_sat(0.002)
    expect_equal(unname(sat_sensor[1]), "UNKNOWN")
    expect_equal(unname(sat_sensor[2]), "UNKNOWN")
    
    sat_sensor <- .cov_guess_sat(10)
    expect_equal(unname(sat_sensor[1]), "SENTINEL-2")
    expect_equal(unname(sat_sensor[2]), "MSI")
    
    sat_sensor <- .cov_guess_sat(70)
    expect_equal(unname(sat_sensor[1]), "CBERS")
    expect_equal(unname(sat_sensor[2]), "AWFI")
    
    sat_sensor <- .cov_guess_sat(30)
    expect_equal(unname(sat_sensor[1]), "LANDSAT")
    expect_equal(unname(sat_sensor[2]), "OLI")
    
    sat_sensor <- .cov_guess_sat(5)
    expect_equal(unname(sat_sensor[1]), "UNKNOWN")
    expect_equal(unname(sat_sensor[2]), "UNKNOWN")
})
