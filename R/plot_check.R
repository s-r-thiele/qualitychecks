#' Plot the raw values of one measure from a quality_checks result
#'
#' @description
#' Takes the result of [quality_checks()] and plots all data columns whose
#' name matches `measure` (e.g. `"scr"` or `"expect"`), using the data
#' stored in `object$inputs`. Supports a point plot (value per participant),
#' a boxplot, and a density plot, either all columns together or one panel
#' per column. Also supports basic data transformations described below.
#'
#' @param object An object of class `quality_checks`, as returned by
#'   [quality_checks()].
#' @param measure Character. Name (or regular expression) of the measure to
#'   plot. Every column whose name matches is included.
#' @param type Character. `"point"` (default), `"boxplot"` or `"density"`.
#' @param wrap Logical. `FALSE` (default): all matching columns in one plot.
#'   `TRUE`: one facet per column.
#' @param transform Character. `"raw"` (default), `"square"`, `"sqrt"`, `"exp"`
#'    or `"log"`.
#'
#' @importFrom rlang .data
#'
#' @return A ggplot object (or, for an unsupported `type` or `transformation`,
#'    an error).
#' @export
plot_check <- function(object, measure,
                       type = "point",
                       wrap = FALSE,
                       transform = "raw"){

  if (object$inputs$data_format == "wide"){

    data <- as.data.frame(object$inputs$data)

    # Prepare data

    # Keep only data of the selected measure
    data_mes <- data |>
      dplyr::select(tidyselect::matches(measure))

    # Error if the selection does not match any column
    if (length(data_mes) == 0){
      stop("No column matches your specified measure. Please check that the selected measure is present in the data")
    }

    # Create id variable for datasets without participant id
    id_col <- grep("^participant_id", names(data), value = TRUE)[1]

    if (is.na(id_col)){

      data_mes <- data_mes |>
        dplyr::mutate(id = seq_len(nrow(data)))

    }

    # Transform participant ids to factor
    else{
      data_mes <- data_mes |>
        dplyr::mutate(id = as.factor(data[[id_col]]))
    }


    # Pivot dataset
    data_mes_l <- data_mes |>
      tidyr::pivot_longer(cols = -"id",
                          names_to = "names",
                          values_to = "vals")

    # Transform data if specified

    if (transform == "raw"){
      transform_info = "untransformed"
    }

    else if (transform %in% c("^2", "square")){
      data_mes_l <- data_mes_l |>
        dplyr::mutate(vals = vals^2)

      transform_info = "squared"
    }

    else if (transform == "sqrt"){
      data_mes_l <- data_mes_l |>
        dplyr::mutate(vals = sqrt(vals))

      transform_info = "sqrt"
    }

    else if (transform == "exp"){
      data_mes_l <- data_mes_l |>
        dplyr::mutate(vals = exp(vals))

      transform_info = "exp"
    }

    else if (transform == "log"){
      data_mes_l <- data_mes_l |>
        dplyr::mutate(vals = log(vals))

      transform_info = "log"
    }
    else {
      stop("The function currently only supports transformations 'raw', 'square', 'sqrt', 'exp' and 'log'.")
    }

    # Point Cloud

    if (type == "point"){

      if (wrap == FALSE){

        ggplot2::ggplot(data = data_mes_l, ggplot2::aes(x = .data$id,
                                                        y = .data$vals))+
          ggplot2::geom_point()+
          ggplot2::xlab("Participant ID")+
          ggplot2::ylab(paste(transform_info, measure, "values"))

      }

      else if (wrap == TRUE){

        ggplot2::ggplot(data = data_mes_l, ggplot2::aes(x = .data$id,
                                                        y = .data$vals,
                                                        group = .data$names))+
          ggplot2::geom_point()+
          ggplot2::facet_wrap(~names, scales = "free")+
          ggplot2::xlab("Participant ID")+
          ggplot2::ylab(paste(transform_info, measure, "values"))
      }

    }

    # Boxplot

    else if (type == "boxplot"){

      if (wrap == FALSE){

        ggplot2::ggplot(data = data_mes_l, ggplot2::aes(y = .data$vals))+
          ggplot2::geom_boxplot()+
          ggplot2::ylab(paste(transform_info, measure, "values"))

      }

      else if (wrap == TRUE){

        ggplot2::ggplot(data = data_mes_l, ggplot2::aes(y = .data$vals, group = .data$names))+
          ggplot2::geom_boxplot()+
          ggplot2::facet_wrap(~names, scales = "free")+
          ggplot2::ylab(paste(transform_info, measure, "values"))
      }

    }

    # Density Plot

    else if (type == "density"){

      if (wrap == FALSE){

        ggplot2::ggplot(data = data_mes_l, ggplot2::aes(x = .data$vals))+
          ggplot2::geom_density()+
          ggplot2::xlab(paste(transform_info, measure, "values"))+
          ggplot2::ylab("Density")

      }

      else if (wrap == TRUE){

        ggplot2::ggplot(data = data_mes_l, ggplot2::aes(x = .data$vals, group = .data$names))+
          ggplot2::geom_density()+
          ggplot2::facet_wrap(~names)+
          ggplot2::xlab(paste(transform_info, measure, "values"))+
          ggplot2::ylab("Density")
      }

    }

    else {
      stop("The function currently only supports types 'point','boxplot' and 'density'.")
    }

  }

  else if (object$inputs$data_format == "long"){
    stop(
      "Long format for data not yet implemented. In the meantime,
         please use wide format data."
    )
  }

  else {
    stop(
      "Internal error; plot_check : data_format"
    )
  }

}
