#' Train a Modeltime Model
#' @param input The input time series data in a data.frame format
#' @param model_arg The model arguments from the settings file
#' @return A trained modeltime object

train_model <- function(input, model_arg) {
    if (is.null(model_arg$function_args)) {
        fun_arg <- ""
    } else {
        fun_arg <- model_arg$function_args
    }

    md <- eval(parse(text = paste(model_arg$function_type, "(", fun_arg, ")", sep = ""))) |>
        set_engine(model_arg$engine)

    if (!is.null(model_arg$set_mode)) {
        md <- md |> set_mode(model_arg$set_mode)
    }

    md_f <- as.formula(model_arg$formula)

    md <- md |> fit(md_f, data = input)

    return(md)
}

#' Forecast a Modeltime Model
#' @param input The input time series data in a data.frame format
#' @param new_data The forecast inputs data.frame
#' @param model_arg The model arguments from the settings file
#' @param filter A boolean, if set to TRUE (default), will return the modeltime
#' forecast object with only the forecast (e.g., without the actual)
#' @return A trained modeltime object
forecast_model <- function(input, new_data, model_arg, filter = TRUE) {
    md <- train_model(input = input, model_arg = model_arg)

    calibration_md <- md %>%
        modeltime_calibrate(new_data = input)

    forecast <- calibration_md |>
        modeltime_forecast(
            new_data    = new_data,
            actual_data = input
        )

    if (filter) {
        forecast <- forecast |>
            dplyr::filter(.key == "prediction")
    }

    return(forecast)
}


forecast_model(input = input, new_data = new_data, model_arg = model_arg, filter = TRUE)
