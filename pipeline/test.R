facets <- list(
    parent = "ISNE",
    subba = "4001"
)


x <- eia_get(
    api_key = Sys.getenv("EIA_API_KEY"),
    api_path = paste(api_path, "data", sep = ""),
    data = "value",
    facets = facets,
    start = "2024-05-15T08",
    end = "2024-06-04T01",
    length = 5000,
    offset = 0,
    frequency = NULL,
    format = "data.frame"
)

x <- x |>
    dplyr::mutate(time = as.POSIXct(period)) |>
    dplyr::arrange(time)

plot(x$time, x$value, type = "l")

eia_backfill(
    start = as.POSIXct("2024-05-18 08:00:00"),
    end = as.POSIXct("2024-06-01 01:00:00"),
    offset = 2200,
    api_key = Sys.getenv("EIA_API_KEY"),
    api_path = paste(api_path, "data", sep = ""),
    facets = facets
)
