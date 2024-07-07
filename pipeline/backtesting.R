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
#' @return A data.frame object with the forecast
create_forecast <- function(
    input,
    calibrated_models,
    method,
    h,
    index,
    var,
    lags) {
    p <- max(calibrated_models$partition)
    m <- method
    new_data <- create_future_frame(input = input, index = index, h = h)

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
#' @return A data.frame object with the forecast

create_forecast_subba <- function(
    input,
    selected_models,
    h,
    index,
    var,
    lags) {
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
            index = "time",
            var = "y",
            lags = lags
        ) |>
            dplyr::select(time = .index, subba, method, model = .model_desc, yhat = .value, lower = .conf_lo, upper = .conf_hi)

        return(fc_subba)
    }) |> dplyr::bind_rows()

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
#' @return A Plotly subplot object

plot_forecast <- function(input, forecast, hours) {
    subba_list <- unique(input$subba)

    start_time <- min((input |> dplyr::group_by(subba) |>
        dplyr::filter(time == max(time)))$time) - lubridate::hours(hours)



    p <- lapply(seq_along(subba_list), function(i) {
        d <- input |>
            dplyr::filter(
                time > start_time,
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
                x = d$time,
                y = d$y,
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
