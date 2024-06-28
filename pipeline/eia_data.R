# Sourse: LinkedIn Learning - data-pipeline-automation-with-github-actions-4503382
# https://github.com/LinkedInLearning/data-pipeline-automation-with-github-actions-4503382
#' Create a Metadata data.frame Template
#' @description The function creates a template metadata data.frame
#' @return An empty data.frame

metadata_tamplate <- function() {
    metadata_template <- data.frame(
        index = integer(),
        parent = character(),
        subba = character(),
        time = lubridate::POSIXct(),
        start = lubridate::POSIXct(),
        end = lubridate::POSIXct(),
        start_act = lubridate::POSIXct(),
        end_act = lubridate::POSIXct(),
        start_match = logical(),
        end_match = logical(),
        n_obs = integer(),
        na = integer(),
        type = character(),
        update = logical(),
        success = logical(),
        comments = character()
    )

    return(metadata_template)
}


#' Create Metadata Table
#' @description The function creates metadata table to a given inputs
#' @param data the input data
#' @param start the start argument of the data request,
#' will be use to evluate if the first timestamp of the series is aligned with the request
#' @param end the end argument of the data request,
#' will be use to evluate if the last timestamp of the series is aligned with the request
#' @param type the refresh type, either "backfill" or "refresh"
#' @return A data.frame object with the data input metadata

create_metadata <- function(data, start, end, type) {
    meta <- list(
        index = NA,
        parent = NA,
        subba = NA,
        time = Sys.time(),
        start = start,
        end = end,
        start_act = NA,
        end_act = NA,
        start_match = NA,
        end_match = NA,
        n_obs = NA,
        na = NA,
        type = type,
        update = FALSE,
        success = FALSE,
        comments = ""
    )
    if (!is.null(data)) {
        d <- data |> dplyr::filter(!is.na(value))
        meta["parent"] <- unique(d$parent)
        meta["subba"] <- unique(d$subba)
        meta["start_act"] <- min(d$period)
        meta["end_act"] <- max(d$period)
        meta["start_match"] <- ifelse(min(d$period) == start, TRUE, FALSE)
        meta["end_match"] <- ifelse(max(d$period) == end, TRUE, FALSE)
        meta["n_obs"] <- nrow(data)
        meta["na"] <- sum(is.na(data$value))

        if (is.numeric(meta$start_act)) {
            meta$start_act <- as.POSIXct(meta$start_act)
        }

        if (is.numeric(meta$end_act)) {
            meta$end_act <- as.POSIXct(meta$end_act)
        }

        if (meta$start_match && meta$end_match && meta$type == "refresh" && meta$na == 0) {
            meta$success <- TRUE
        } else {
            meta$success <- FALSE
        }

        if (!meta$start_match) {
            meta["comments"] <- paste(meta["comments"], "The start argument does not match the actual; ", sep = "")
        }
        if (!meta$end_match) {
            meta["comments"] <- paste(meta["comments"], "The end argument does not match the actual; ", sep = "")
        }

        if (meta$na != 0) {
            meta["comments"] <- paste(meta["comments"], "Missing values were found; ", sep = "")
        }
    } else {
        meta["comments"] <- paste(meta["comments"], "No new data is available; ", sep = "")
    }

    return(meta)
}

#' Function to Append and Save New Data
#' @description The function enables to append new dataset to the historical dataset or to save a new dataset (e.g., backfill)
#' @param data_path The path and file name of the data CSV file
#' @param new_data The new dataset
#' @param init if set to TRUE will save the new dataset without append (e.g., overwrite the file).
#' Should be use when conducting a data backfill. By default is set to FALSE
#' @param save if set to TRUE will save the final dataset (appended or backfill) as CSV file using the data_path argument
#' @return a data.frame object

append_data <- function(data_path, new_data, init = FALSE, save = FALSE) {
    if (!init) {
        data <- readr::read_csv(file = data_path, col_types = readr::cols(
            period = readr::col_datetime(format = ""),
            subba = readr::col_character(),
            subba_name = readr::col_character(),
            parent = readr::col_character(),
            parent_name = readr::col_character(),
            value = readr::col_double(),
            value_units = readr::col_character()
        ))
        updated_data <- data |> dplyr::bind_rows(new_data)
    } else {
        print("Initial data pull")
        updated_data <- new_data
    }

    if (save) {
        print("Save the data to CSV file")
        write.csv(updated_data, data_path, row.names = FALSE)
    }

    return(updated_data)
}


#' Function to Append and Save Metadata
#' @description The function enables to append new metadata to the main metadata file or to save a new metadata (e.g., for backfill)
#' @param meta_path The path and file name of the metadata CSV file
#' @param new_meta The new metadata table
#' @param init if set to TRUE will save the new metadata without appending it to the main metadata (e.g., overwrite the file).
#' Should be use when conducting data backfill. By default is set to FALSE
#' @param save if set to TRUE will save the final metadata (appended or backfill) as CSV file using the meta_path argument
#' @return a data.frame object

append_metadata <- function(meta_path, new_meta, init = FALSE, save = FALSE) {
    if (!init) {
        meta_archive <- readr::read_csv(meta_path, col_types = readr::cols(
            index = readr::col_double(),
            parent = readr::col_character(),
            subba = readr::col_character(),
            time = readr::col_datetime(format = ""),
            start = readr::col_datetime(format = ""),
            end = readr::col_datetime(format = ""),
            start_act = readr::col_datetime(format = ""),
            end_act = readr::col_datetime(format = ""),
            start_match = readr::col_logical(),
            end_match = readr::col_logical(),
            n_obs = readr::col_double(),
            na = readr::col_double(),
            type = readr::col_character(),
            update = readr::col_logical(),
            success = readr::col_logical(),
            comments = readr::col_character()
        ))
        new_meta$index <- max(meta_archive$index) + 1
        meta <- dplyr::bind_rows(meta_archive, new_meta)
    } else {
        new_meta$index <- 1
        meta <- new_meta
    }

    if (save) {
        print("Saving the metadata file")
        write.csv(x = meta, file = meta_path, row.names = FALSE)
    }

    return(meta)
}


#' Load the Metadata
#' @description the function load the metadata and create the metadata summary for the next data request
#' @param meta_path The path and file name of the metadata CSV file
#' @param series the series information table read from the series.JSON file
#' @return a list object

load_metadata <- function(meta_path, series) {
    meta <- readr::read_csv(meta_path, col_types = readr::cols(
        index = readr::col_double(),
        parent = readr::col_character(),
        subba = readr::col_character(),
        time = readr::col_datetime(format = ""),
        start = readr::col_datetime(format = ""),
        end = readr::col_datetime(format = ""),
        start_act = readr::col_datetime(format = ""),
        end_act = readr::col_datetime(format = ""),
        start_match = readr::col_logical(),
        end_match = readr::col_logical(),
        n_obs = readr::col_double(),
        na = readr::col_double(),
        type = readr::col_character(),
        update = readr::col_logical(),
        success = readr::col_logical(),
        comments = readr::col_character()
    ))


    log_temp <- list(
        parent = NULL,
        subba = NULL,
        end_act = NULL,
        request_start = NULL
    )

    meta_success <- meta |> dplyr::filter(success)

    request_meta <- NULL
    for (i in 1:nrow(series)) {
        p <- series$parent_id[i]
        s <- series$subba_id[i]
        l <- meta_success |>
            dplyr::filter(parent == p, subba == s) |>
            dplyr::filter(index == max(index))
        log <- log_temp
        log$parent <- p
        log$subba <- s
        log$end_act <- max(l$end_act)
        log$request_start <- max(l$end_act) + lubridate::hours(1)
        request_meta <- dplyr::bind_rows(request_meta, as.data.frame(log))
    }


    output <- list(
        metadata = meta,
        last_index = max(meta$index),
        request_meta = request_meta
    )

    return(output)
}

#' Get Metadata
#' @description The function read the metadata file and check if new data is available on the API
#' @param api_key the API key
#' @param api_path the series API route
#' @param meta_path The path and file name of the metadata CSV file
#' @param series the series information table read from the series.JSON file
#' @return a data.frame

get_metadata <- function(api_key, api_path, meta_path, series) {
    meta <- load_metadata(meta_path = meta_path, series = series)
    api_metadata <- EIAapi::eia_metadata(api_key = api_key, api_path = api_path)
    end <- lubridate::ymd_h(api_metadata$endPeriod, tz = "UTC")
    meta$request_meta$end <- end
    meta$request_meta$updates_available <- ifelse(meta$request_meta$end > meta$request_meta$request_start, TRUE, FALSE)

    return(meta)
}
