#' Create the Backtesting Partitions Settings
#' @description The function creates the backtesting partitions splits settings
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @param partitions The number of partitions
#' @param overlap The number of observations overlap between each partition
#' @param test_length The length of the testing partition
#' @return A data.frame object with the backtestings splits settings

create_partitions <- function(input, index, partitions, overlap, test_length) {
    partitions_df <- lapply(1:partitions, function(i) {
        s <- 1
        n <- nrow(input)
        p <- partitions - i + 1
        e <- n - p * test_length + overlap * (p - 1)

        data <- data.frame(
            partition = i,
            train_start = input[s, index][[index]],
            train_end = input[e, index][[index]],
            test_start = input[e + 1, index][[index]],
            test_end = input[e + test_length, index][[index]]
        )
        return(data)
    }) |>
        dplyr::bind_rows()

    return(partitions_df)
}


#' Create the Backtesting Grid Table
#' @description The function creates the backtesting grid table
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @param partitions The number of partitions
#' @param overlap The number of observations overlap between each partition
#' @param test_length The length of the testing partition
#' @param methods A list with the forecasting methods
#' @return A data.frame object with the backtestings grid table

create_grid <- function(input, index, partitions, overlap, test_length, methods) {
    p_df <- create_partitions(
        input = input,
        index = index,
        partitions = partitions,
        overlap = overlap,
        test_length = test_length
    )

    grid <- lapply(names(methods), function(i) {
        grid_method <- p_df |>
            dplyr::mutate(method = i, type = "backtesting")

        return(grid_method)
    }) |>
        dplyr::bind_rows()
}

#' Execute the Backtesting Models
#' @description The function executes the backtesting forecasting models
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @param var The input numeric column name
#' @param lags The lag position values
#' @param grid The grid table, the output of the create_grid function
#' @param methods A list with the forecasting methods
#' @param cores The number of cores to use
#' @return A nated list object with the models forecast outputs models score

run_grid <- function(input, index, var, lags, grid, methods, cores = 1) {
    bkt <- parallel::mclapply(mc.cores = cores, 1:nrow(grid), function(i) {
        p <- grid$partition[i]
        message(paste("Partition:", i))
        train <- input[which(input[[index]] >= grid$train_start[i] & input[[index]] <= grid$train_end[i]), ]


        test <- input[which(input[[index]] >= grid$test_start[i] & input[[index]] <= grid$test_end[i]), ]

        if (!is.null(lags)) {
            train <- train |>
                add_lags(index = index, var = var, lags = lags)

            test <- add_lags_forecast(input = train, forecast = test, index = index, var = var, lags = lags)
        }

        models_names <- names(methods)
        # Fit models
        fit <- lapply(models_names, function(l) {
            if (is.null(methods[[l]]$function_args)) {
                fun_arg <- ""
            } else {
                fun_arg <- methods[[l]]$function_args
            }

            md <- eval(parse(text = paste(methods[[l]]$function_type, "(", fun_arg, ")", sep = ""))) |>
                set_engine(methods[[l]]$engine)

            if (!is.null(methods[[l]]$set_mode)) {
                md <- md |> set_mode(methods[[l]]$set_mode)
            }


            md <- md |> fit(methods[[l]]$formula, data = train)

            return(md)
        }) |> stats::setNames(models_names)

        # Calibrate models
        models_tbl <- as_modeltime_table(fit) |>
            modeltime_calibrate(new_data = test) |>
            dplyr::mutate(
                method = models_names,
                partition = p
            )

        # Score models
        score <- models_tbl |>
            modeltime_accuracy()


        output <- list(score = score, models = models_tbl)
        return(output)
    })

    gc()

    return(bkt)
}


#' Create Score Table
#' @description The function extract the models score and append them into a single table
#' @param bkt A backtesting object
#' @return A data.frame object with the models score on the backtesting partitions
get_score <- function(bkt) {
    score <- lapply(bkt, function(i) {
        return(i$score)
    }) |>
        dplyr::bind_rows()

    return(score)
}


#' Append Backtesting Forecast Moldes Table
#' @description The function extract the models forecast by testing partition and append them into a single table
#' @param bkt A backtesting object
#' @return A data.frame object with the models forecast by backtesting partitions
get_models <- function(bkt) {
    models <- lapply(bkt, function(i) {
        i$models
    }) |>
        dplyr::bind_rows()

    return(models)
}


#' A Backtesting Function
#' @description The function test, evaluate, and score forecasting models
#' @param input The input time series data in a data.frame format
#' @param var The input numeric column name
#' @param index The input data index column name
#' @param lags The lag position values
#' @param partitions The number of partitions
#' @param overlap The number of observations overlap between each partition
#' @param test_length The length of the testing partition
#' @param methods A list with the forecasting methods
#' @param cores The number of cores to use
#' @return A data.frame object with the backtestings grid table

backtesting <- function(
    input,
    var,
    index,
    lags,
    partitions,
    test_length,
    overlap,
    methods,
    cores = 1) {
    grid <- create_partitions(
        input = input,
        index = index,
        partitions = partitions,
        overlap = overlap,
        test_length = test_length
    )

    bkt <- run_grid(
        input = input,
        var = var,
        index = index,
        lags = lags,
        grid = grid,
        methods = methods,
        cores = cores
    )
    score <- get_score(bkt)
    calibrated_models <- get_models(bkt)

    leaderboard <- score |>
        dplyr::group_by(method) |>
        dplyr::summarise(
            mae = mean(mae),
            mape = mean(mape),
            smape = mean(smape),
            rmse = mean(rmse),
            rsq = mean(rsq)
        ) |>
        dplyr::arrange(mape)


    output <- list(
        calibrated_models = calibrated_models,
        leaderboard = leaderboard,
        score = score,
        testing = bkt
    )

    return(output)
}

#' Create the Forecast data.frame Object
#' @description The function sets the forecast future data.frame object. This includes the future seasonal features and trend
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @param h The forecast horizon
#' @return A data.frame object with forecast deterministic features for the forecast

create_future_frame <- function(input, index, h) {
    last <- max(input[[index]])

    new_index <- seq.POSIXt(from = last + lubridate::hours(1), length.out = h, by = "hour")

    future_data <- data.frame(new_index)
    names(future_data) <- index

    future_data <- future_data |>
        dplyr::mutate(
            trend = as.numeric(!!rlang::sym(index)),
            month = factor(lubridate::month(!!rlang::sym(index), label = TRUE), ordered = FALSE),
            wday = factor(lubridate::wday(!!rlang::sym(index), label = TRUE), ordered = FALSE),
            hour = factor(lubridate::hour(!!rlang::sym(index)), order = FALSE)
        )

    return((future_data))
}

#' Create a Forecast Using a ModelTime Calibrated Model
#' @description The function use a calibrated model from the backtesting and create a forecast
#' @param input The input time series data in a data.frame format
#' @param calibrated_models The backtesting calibrated models table
#' @param method The forecasting method using the model label
#' @param h The forecast horizon
#' @param lags The lag position values
#' @param forecast The forecast or testing data.frame object
#' @param index The input data index column name
#' @param var The input numeric column name
#' @param seasonal A boolean, if set to TRUE (default) will add seasonal
#' features to the input and new_data objects
#' @param trend A boolean, if set to TRUE (default) will add trend
#' feature to the input and new_data objects
#' @return A data.frame object with the forecast
create_forecast <- function(
    input,
    calibrated_models,
    method,
    h,
    index,
    var,
    lags,
    seasonal = TRUE,
    trend = TRUE) {
    p <- max(calibrated_models$partition)
    m <- method
    new_data <- create_future_frame(input = input, index = index, h = h)

    input$y <- input[[var]]

    if (!is.null(lags)) {
        input <- input |> add_lags(index = index, var = var, lags = lags)
        new_data <- add_lags_forecast(
            input = input,
            forecast = new_data,
            index = index,
            var = var,
            lags = lags
        )
    }

    if (seasonal) {
        input <- add_seasonal(input = input, index = index)
        new_data <- add_seasonal(input = new_data, index = index)
    }

    if (trend) {
        input <- add_trend(input = input, index = index)
        new_data <- add_trend(input = new_data, index = index)
    }
    refit <- calibrated_models |>
        dplyr::filter(partition == p) |>
        dplyr::filter(method == m) |>
        modeltime_refit(data = input)

    fc <- refit |>
        modeltime_forecast(new_data = new_data, actual_data = input) |>
        dplyr::filter(.key == "prediction")

    return(fc)
}



#' Create a Forecast Using a ModelTime Calibrated Model for Multiple Series
#' @description The function is a wrapper of the create_forecast function, enables to create forecast for multiple series
#' @param input The input time series data in a data.frame format
#' @param selected_models The top models by subba
#' @param h The forecast horizon
#' @param index The input data index column name
#' @param var The input numeric column name
#' @param lags The lag position values
#' @param train_length The length of the input training series for the refit function
#' @param seasonal A boolean, if set to TRUE (default) will add seasonal
#' features to the input and new_data objects
#' @param trend A boolean, if set to TRUE (default) will add trend
#' feature to the input and new_data objects
#' @return A data.frame object with the forecast

create_forecast_subba <- function(
    input,
    selected_models,
    h,
    index,
    var,
    lags,
    seasonal = TRUE,
    trend = TRUE) {
    subba_list <- unique(input$subba)

    fc <- lapply(seq_along(subba_list), function(i) {
        m <- selected_models |> dplyr::filter(subba == subba_list[i])
        method <- m$method[1]

        d <- input |> dplyr::filter(subba == subba_list[i])

        fc_subba <- create_forecast(
            input = d,
            calibrated_models = m,
            method = method,
            h = h,
            index = index,
            var = var,
            lags = lags,
            seasonal = seasonal,
            trend = trend
        ) |>
            dplyr::select(time = .index, subba, method, model = .model_desc, yhat = .value, lower = .conf_lo, upper = .conf_hi)
        fc_subba$forecast_label <- as.character(as.Date(min(fc_subba$time)))
        return(fc_subba)
    }) |> dplyr::bind_rows()

    attr(fc, "h") <- h
    attr(fc, "var") <- var
    attr(fc, "index") <- index
    attr(fc, "lags") <- lags
    attr(fc, "subba") <- subba_list


    return(fc)
}



#' A Backtesting Function
#' @description The function uses the backtesting function to test, evaluate, and score forecasting models over a multiple time series
#' @param input The input time series data in a data.frame format
#' @param var The input numeric column name
#' @param index The input data index column name
#' @param lags The lag position values
#' @param partitions The number of partitions
#' @param overlap The number of observations overlap between each partition
#' @param test_length The length of the testing partition
#' @param methods A list with the forecasting methods
#' @param cores The number of cores to use
#' @return A data.frame object with the backtestings grid table


backtesting_subba <- function(
    input,
    var,
    index,
    lags,
    partitions,
    test_length,
    overlap,
    cores) {
    subba <- unique(input$subba)
    bkt_subba <- lapply(subba, function(i) {
        message(paste("Subba: ", i, sep = ""))
        d <- input |> dplyr::filter(subba == i)

        bkt <- backtesting(
            input = d,
            index = index,
            var = var,
            lags = lags,
            partitions = partitions,
            test_length = test_length,
            overlap = overlap,
            methods = methods,
            cores = cores
        )

        gc()

        leaderboard <- bkt$leaderboard |> dplyr::mutate(subba = i)
        calibrated_models <- bkt$calibrated_models |>
            dplyr::filter(partition == partitions) |>
            dplyr::mutate(subba = i)

        score <- bkt$score |> dplyr::mutate(subba = i)

        output <- list(
            leaderboard = leaderboard,
            calibrated_models = calibrated_models,
            score = score
        )
        return(output)
    })

    leaderboard <- lapply(bkt_subba, function(i) {
        i$leaderboard
    }) |>
        dplyr::bind_rows()


    score <- lapply(bkt_subba, function(i) {
        i$score
    }) |>
        dplyr::bind_rows()

    calibrated_models <- lapply(bkt_subba, function(i) {
        i$calibrated_models
    }) |>
        dplyr::bind_rows()

    output <- list(
        leaderboard = leaderboard,
        score = score,
        calibrated_models = calibrated_models
    )
    return(output)
}

#' Add Lags
#' @description The function adds lags to a data.frame object
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @param var The input numeric column name
#' @param lags The lag position values

add_lags <- function(input, index, var, lags) {
    d <- input |>
        dplyr::arrange(!!rlang::sym(index))

    for (i in lags) {
        message(paste("Add lag:", i, sep = " "))
        d[[paste("lag", i, sep = "_")]] <- dplyr::lag(d[[var]], i)
    }

    return(d)
}


#' Add Lags to the Forecast Table
#' @description The function adds lags to the forecast data.frame object
#' @param input The input time series data in a data.frame format
#' @param forecast The forecast or testing data.frame object
#' @param index The input data index column name
#' @param var The input numeric column name
#' @param lags The lag position values

add_lags_forecast <- function(input, forecast, index, var, lags) {
    d <- input |>
        dplyr::arrange(!!rlang::sym(index))

    f <- forecast |>
        dplyr::arrange(!!rlang::sym(index))


    for (i in lags) {
        if (i <= nrow(f)) {
            l <- c(tail(d[[var]], i), rep(NA, nrow(f) - i))
        } else {
            l <- tail(d[[var]], i)[1:nrow(f)]
        }
        f[[paste("lag", i, sep = "_")]] <- l
    }

    return(f)
}

#' Plot the Backtesting Error
#' @description The function plots the backtesting error rate by partition and subba
#' @param bkt A backtesting object, the output of the backtesting_subba function
#' @return A plotly subplot object


plot_error <- function(bkt) {
    subba_list <- unique(bkt$score$subba)
    p <- lapply(seq_along(subba_list), function(i) {
        showlegend <- ifelse(i == 1, TRUE, FALSE)
        subba <- subba_list[i]
        score <- bkt$score |> dplyr::filter(subba == subba)

        p_subba <- plotly::plot_ly(
            data = score, x = ~method, y = ~mape, type = "box",
            jitter = 0.3,
            pointpos = -1.8,
            boxpoints = "all",
            color = ~method,
            legendgroup = ~method,
            showlegend = showlegend
        ) |>
            plotly::layout(
                yaxis = list(title = "MAPE"),
                xaxis = list(title = "Method")
            ) |>
            plotly::add_annotations(
                x = 0.1,
                y = 1.05,
                text = subba,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
            )
        return(p_subba)
    })

    sub <- plotly::subplot(p, nrows = ceiling(length(p) / 2), shareX = TRUE, shareY = TRUE)
    return(sub)
}

#' Backtesting Leaderboard Table
#' @description The function returns the best performing models by MAPE for each subba
#' @param bkt A backtesting object, the output of the backtesting_subba function
#' @return A summary table

get_leaderboard <- function(bkt) {
    leaderboard <- bkt$leaderboard |>
        as.data.frame() |>
        dplyr::group_by(subba) |>
        dplyr::filter(mape == min(mape)) |>
        dplyr::ungroup()
}

#' Backtesting Selected Models
#' @description The function returns the top calibrated models by MAPE for each subba
#' @param bkt A backtesting object, the output of the backtesting_subba function
#' @return A calibrated models table

get_selected_models <- function(bkt) {
    leaderboard <- get_leaderboard(bkt)
    selected_models <- lapply(1:nrow(leaderboard), function(i) {
        c <- bkt$calibrated_models |>
            dplyr::filter(subba == leaderboard$subba[i], method == leaderboard$method[i], partition == max(partition))
        return(c)
    }) |> dplyr::bind_rows()

    return(selected_models)
}

#' Plot Forecast by Subba
#' @description The function enables to plot the forecast by subba using Plotly subplots
#' @param input The input time series data in a data.frame format
#' @param forecast The forecast object
#' @param hours The number of hours to display actual observations
#' @param var The input numeric column name
#' @return A Plotly subplot object

plot_forecast <- function(input, forecast, hours, index, var) {
    subba_list <- unique(input$subba)

    start_time <- min((input |> dplyr::group_by(subba) |>
        dplyr::filter(!!rlang::sym(index) == max(!!rlang::sym(index))))[[index]]) - lubridate::hours(hours)




    p <- lapply(seq_along(subba_list), function(i) {
        d <- input |>
            dplyr::filter(
                !!rlang::sym(index) > start_time,
                subba == subba_list[i]
            )

        f <- forecast |>
            dplyr::filter(subba == subba_list[i])


        p <- plotly::plot_ly() |>
            plotly::add_ribbons(
                x = f$time,
                ymin = f$lower,
                ymax = f$upper,
                line = list(color = "rgba(144,224,239, 0.75)"),
                fillcolor = "rgba(144,224,239, 0.5)",
                name = "95% Prediction Intervals",
                showlegend = FALSE
            ) |>
            plotly::add_lines(
                x = d[[index]],
                y = d[[var]],
                name = subba_list[i],
                showlegend = FALSE,
                line = list(color = "#1f77b4")
            ) |>
            plotly::add_lines(
                x = f$time,
                y = f$yhat,
                line = list(color = "#17becf", dash = "dash"),
                name = "Forecast",
                showlegend = FALSE
            ) |>
            plotly::add_annotations(
                x = 0.1,
                y = 1.05,
                text = subba_list[i],
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
            )

        return(p)
    })

    p_sub <- plotly::subplot(p, nrows = ceiling(length(p) / 2), shareX = TRUE)

    return(p_sub)
}


#' Log Template
#' @description The function returns the log template - an empty data.frame with the logs columns
#' @return A data.frame object

log_frame <- function() {
    log <- data.frame(
        index = integer(),
        subba = character(),
        model = character(),
        time = as.POSIXct(character()),
        forecast_label = character(),
        start = as.POSIXct(character()),
        end = as.POSIXct(character()),
        h = integer(),
        n_obs = integer(),
        n_obs_flag = logical(),
        na_flag = logical(),
        success = logical(),
        score = logical(),
        mape = double(),
        rmse = double(),
        coverage = double()
    )

    return(log)
}


#' Log the Forecast Metadata
#' @description  The function creates log for each forecast, append and save it locally
#' @param forecast A forecast object
#' @param forecast_log_path The forecast log file path and name
#' @param h The forecast horizon
#' @param init Initialize the forecast file (overwrite previous file if exists) when sets to TRUE,
#' by default set to FALSE
#' @param save Saves the log results if sets to TRUE, by default sets to FALSE
#' @return A data.frame object with the log output
create_forecast_log <- function(
    forecast,
    forecast_log_path,
    h,
    init = FALSE,
    save = FALSE) {
    if (init) {
        index <- 1
        log <- log_frame()
    } else {
        log <- load_forecast_log(forecast_log_path = forecast_log_path)

        index <- max(log$index) + 1
    }

    fc_attr <- attributes(forecast)

    subba <- fc_attr$subba

    for (i in subba) {
        log_temp <- f <- NULL

        f <- forecast |> dplyr::filter(subba == i)

        log_temp <- data.frame(
            index = index,
            subba = i,
            model = unique(f$model),
            method = unique(f$method),
            time = Sys.time(),
            forecast_label = unique(f$forecast_label),
            start = min(f$time),
            end = max(f$time),
            h = fc_attr$h,
            n_obs = nrow(f),
            n_obs_flag = ifelse(fc_attr$h == nrow(f), TRUE, FALSE),
            na_flag = any(is.na(f$yhat)),
            success = FALSE,
            score = FALSE,
            mape = NA,
            rmse = NA,
            coverage = NA
        )

        log_temp$success[1] <- ifelse(log_temp$n_obs_flag[1] & !log_temp$na_flag[1], TRUE, FALSE)


        log <- rbind(log, log_temp)

        index <- index + 1
    }

    if (save) {
        write.csv(log, forecast_log_path, row.names = FALSE)
    }

    invisible(log)
}



#' Save Forecast
#' @description The function append new forecast to the forecast archive file
#' @param forecast A forecast object
#' @param forecast_path The forecast file path and name
#' @param init Initialize the forecast file (overwrite previous file if exists) when sets to TRUE,
#' by default set to FALSE
#' @param save Saves the log results if sets to TRUE, by default sets to FALSE
save_forecast <- function(
    forecast,
    forecast_path,
    init = FALSE,
    save = FALSE) {
    if (!init) {
        message("Load archive forecast and append new forecast")
        forecast_archive <- read.csv(forecast_path) |>
            dplyr::mutate(time = as.POSIXct(time))

        f <- rbind(forecast_archive, forecast)
    } else {
        message("Initialize the forecast file")
        f <- forecast
    }

    if (save) {
        message(paste("Save the forecast to ", forecast_path, sep = ""))
        write.csv(f, forecast_path, row.names = FALSE)
    }
}


#' Load the Forecast Log
#' @description The function load the forecast log
#' @param forecast_log_path The forecast log file path and name
#' @return A data.frame object

load_forecast_log <- function(forecast_log_path) {
    log <- read.csv(forecast_log_path) |>
        dplyr::mutate(
            time = as.POSIXct(time),
            start = as.POSIXct(start),
            end = as.POSIXct(end)
        )

    return(log)
}


#' Load the Forecast Log
#' @description The function load the forecast log
#' @param forecast_path The forecast file path and name
#' @return A data.frame object

load_forecast <- function(forecast_path) {
    forecast <- read.csv(forecast_path) |>
        dplyr::mutate(
            time_str = ifelse(nchar(time) == 10, paste(time, "00:00:00", sep = " "), time),
            time = lubridate::ymd_hms(time_str)
        )

    return(forecast)
}

#' Add Trend Feature
#' @description The function adds trend feature to a time series object
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @return The input object with a trend column

add_trend <- function(input, index) {
    input$trend <- as.numeric(input[[index]])
    return(input)
}
#' Add Seasonal Features
#' @description The function adds trend feature to a time series object
#' @param input The input time series data in a data.frame format
#' @param index The input data index column name
#' @return The input object with a trend column

add_seasonal <- function(input, index) {
    input$month <- factor(lubridate::month(input[[index]], label = TRUE), ordered = FALSE)
    input$wday <- factor(lubridate::wday(input[[index]], label = TRUE), ordered = FALSE)
    input$hour <- factor(lubridate::hour(input[[index]]), order = FALSE)

    return(input)
}

#' Refresh the Forecast
#' @description The function checks if new data points are available and refresh the forecast
#' based on condition
#' @param input The input time series data in a data.frame format
#' @param forecast_log_path The forecast log file path and name
#' @param forecast_path The forecast file path and name
#' @param calibrated_models_path The calibrated models path
#' @param h The forecast horizon
#' @param index The input data index column name
#' @param var The input numeric column name
#' @param train_length The length of the input training series for the refit function
#' @param lags The lag position values
#' @param init Initialize the forecast file (overwrite previous file if exists) when sets to TRUE,
#' by default set to FALSE
#' @param save Saves the log results if sets to TRUE, by default sets to FALSE
#' @return A forecast object, if meet the refresh condition

refresh_forecast <- function(
    input,
    forecast_log_path,
    forecast_path,
    calibrated_models_path,
    h,
    index,
    var,
    train_length = 24 * 31 * 25,
    lags,
    init = FALSE,
    save = FALSE,
    seasonal = TRUE,
    trend = TRUE) {
    forecast <- NULL
    input <- input |>
        dplyr::select(subba, !!rlang::sym(index), y = !!rlang::sym(var))

    input_last_point <- input |>
        dplyr::group_by(subba) |>
        dplyr::filter(!!rlang::sym(index) == max(!!rlang::sym(index))) |>
        dplyr::ungroup() |>
        dplyr::select(subba, last_time = !!rlang::sym(index))


    log <- load_forecast_log(forecast_log_path = forecast_log_path)

    calibrated_models <- readRDS(calibrated_models_path) |>
        dplyr::left_join(
            log |>
                dplyr::filter(success) |>
                dplyr::group_by(subba) |>
                dplyr::filter(end == max(end)) |>
                dplyr::select(subba, method, end),
            by = c("subba", "method")
        ) |>
        dplyr::left_join(input_last_point, by = "subba") |>
        dplyr::mutate(end_filter = lubridate::floor_date(last_time, unit = "day") - lubridate::hours(1)) |>
        dplyr::mutate(refresh = ifelse(end_filter > end, TRUE, FALSE))


    if (!any(calibrated_models$refresh)) {
        message("No new data is available to refresh the forecast")
    } else {
        message("New data is avaiable, starting the forecast refresh process")
        calibrated_models <- calibrated_models |> dplyr::filter(refresh == TRUE)

        for (i in 1:nrow(calibrated_models)) {
            end <- calibrated_models$end_filter[i]
            start <- end - lubridate::hours(train_length)


            temp <- input |>
                dplyr::filter(
                    subba == calibrated_models$subba[i],
                    !!rlang::sym(index) >= start & !!rlang::sym(index) <= end
                )
            if (i == 1) {
                d <- temp
            } else {
                d <- rbind(d, temp)
            }
        }



        forecast <- create_forecast_subba(
            input = d,
            selected_models = calibrated_models,
            h = h,
            index = index,
            var = "y",
            lags = lags,
            seasonal = TRUE,
            trend = TRUE
        )

        log <- create_forecast_log(
            forecast = forecast,
            forecast_log_path = forecast_log_path,
            h = h,
            init = init,
            save = save
        )

        subba_success <- log$subba[which(log$success)]


        if (length(subba_success) > 0) {
            forecast_save <- forecast |> dplyr::filter(subba %in% subba_success)
            save_forecast(forecast = forecast_save, forecast_path = forecast_path, init = init, save = save)
        }
    }

    if (!is.null(forecast)) {
        return(forecast)
    }
}



#' Pull Input Data from Snowflake
#' @description The function enables to pull input data from Snowflake and reformat it into a tsibble object
#' @param con Snowflake connection object
#' @param database The Snowflake database name
#' @param schema The Snowflake schema name
#' @param table The Snowflake table name
#' @param product The product name
#' @param itune_org The iTune Org name
#' @param start Optional, enables to filter the input data starting date (greater or equal to the start date)
#' @param end Optional, enables to filter the input data ending date (greater or equal to the end date)
#' @param index The column name of the series timestamp, by default using the date of the first day of the fiscal week
#' @param y The column name of the series numeric value, by default using the local currency column
#' @return A tsibble object with the following columns - date (Date class), index (yearweek class), and y (numeric class)
#' @export

pull_input <- function(
    con,
    table,
    database,
    schema,
    product,
    itune_org,
    start = NULL,
    end = NULL,
    index = "STD_WEEK_BEGIN_DT",
    y = "AMOUNT_LC") {
    # Check the function arguments
    check_con(obj = con)
    check_string(obj = table)
    check_string(obj = database)
    check_string(obj = schema)
    check_string(obj = product)
    check_string(obj = itune_org)
    check_string(obj = index)
    check_string(obj = y)


    query <- sprintf(
        'SELECT * FROM "%s"."%s"."%s" WHERE "FORECAST_PRODUCT" = \'%s\' AND "ITUNES_PLANNING_ORG" = \'%s\'',
        database,
        schema,
        actual_table,
        product,
        itune_org
    )
    if (is.null(start) && is.null(end)) {
        query <- sprintf("%s;", query)
    } else if (!is.null(start) && !is.null(end)) {
        query <- sprintf('%s AND "%s" >= \'%s\' AND "%s" <= \'%s\';', query, index, start, index, end)
    } else if (is.null(start) && !is.null(end)) {
        query <- sprintf('%s AND "%s" <= \'%s\';', query, index, end)
    } else if (!is.null(start) && is.null(end)) {
        query <- sprintf('%s AND "%s" >= \'%s\';', query, index, start)
    }

    df <- NULL

    tryCatch(
        expr = {
            df <- DBI::dbGetQuery(conn = con, statement = query)
        },
        error = function(e) {
            message("Error with the pull_input function")
        }
    )
    if (is.null(df)) {
        stop("Failed to pull the table from Snowflake, please check your connection settings")
    }

    df$date <- as.Date(df[[index]])


    ts <- df[, c("date", y)]
    names(ts) <- c("date", "y")

    ts$index <- tsibble::yearweek(ts$date)

    ts <- ts[, c("date", "index", "y")]

    ts <- tsibble::as_tsibble(ts, index = "index")


    return(ts)
}


#' Pull Input Data from Snowflake
#' @description The function enables to pull input data from Snowflake and reformat it into a tsibble object
#' @param con Snowflake connection object
#' @param database The Snowflake database name
#' @param schema The Snowflake schema name
#' @param table The Snowflake table name
#' @param product The product name
#' @param itune_org The iTune Org name
#' @param events Optional, a character vector with events names to filter, if set to NULL (default) will return all events
#' @return The events table
#' @export

pull_events <- function(
    con,
    table,
    database,
    schema,
    product,
    itune_org,
    events = NULL) {
    # Check the function arguments
    check_con(obj = con)
    check_string(obj = database)
    check_string(obj = schema)
    check_string(obj = table)
    check_string(obj = product)
    check_string(obj = itune_org)

    query <- sprintf(
        'SELECT * FROM "%s"."%s"."%s" WHERE "product" = \'%s\' AND "itunes_planning_org" = \'%s\'',
        database,
        schema,
        table,
        product,
        itune_org
    )

    if (is.null(events)) {
        query <- sprintf("%s;", query)
    } else {
        e <- add_brackets((events))
        query <- sprintf('%s AND "event_name" in %s;', query, e)
    }

    tryCatch(
        expr = {
            df <- DBI::dbGetQuery(conn = con, statement = query)
        },
        error = function(e) {
            message("Error with the pull_events function")
        }
    )
    if (is.null(df)) {
        stop("Failed to pull the events table, please check your Snowflake settings")
    }


    df <- df |>
        dplyr::select(date = index, dplyr::everything()) |>
        dplyr::mutate(index = tsibble::yearweek(date)) |>
        dplyr::select(date, index, event_name, type, itunes_planning_org, product)

    return(df)
}
