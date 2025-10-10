#' Simulate an Analysis Dataset Based on Created Dataset
#'
#' This function simulates an analysis dataset by generating random data based on the structure of a given dataset created via the `create_adam_dataset` function.
#'
#' @param dataset A tibble created via the `create_adam_dataset` function.
#' @param n A numeric value specifying the number of rows to be simulated.
#'
#' @return A tibble containing the simulated data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Simulate a dataset with 10 rows based on the structure from 'ADSL.xlsx'
#' x <- simulate_adam_dataset(create_adam_dataset('ADSL.xlsx'), n = 10)
#' }
simulate_adam_dataset <- function(dataset, n = 10) {

 # Validation Step -------------------------------------------------------------
  stopifnot(
    "`n` must be provided." = !is.na(n),
    "`n` must be a character." = is.numeric(n),
    "`n` cannot be an array." = length(n) == 1
  )
 # -----------------------------------------------------------------------------

  stopifnot("Validation error: The dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(dataset))

  path <- attr(dataset, "path")
  name <- attr(dataset, "name")

  cols <- lapply(names(dataset), function(colname) {
    col <- dataset[[colname]]

    data_type <- attr(col, 'data_type')
    origin <- attr(col, 'origin')
    levels <- attr(col, 'levels')
    has_no_data <- attr(col, 'has_no_data')

    length_attr <- as.integer(attr(col, 'length'))
    significant_digits_attr <- as.integer(attr(col, 'significant_digits'))

    is_simulated <- origin == 'Collected' & has_no_data == 'No'

    convert_levels_to_type <- function(levels_vec, data_type) {
      switch(tolower(data_type),
             "integer" = as.integer(levels_vec),
             "float" = as.numeric(levels_vec),
             "datetime" = as.POSIXct(levels_vec),
             "date" = as.Date(levels_vec),
             "time" = hms::as_hms(levels_vec),
             "partialdate" = as.character(levels_vec),
             "partialtime" = as.character(levels_vec),
             "partialdatetime" = as.character(levels_vec),
             "text" = as.character(levels_vec),
             as.character(levels_vec)
      )
    }

    simulated_col <- if (!is_simulated) {
      if (!is.null(levels)) {
        converted_levels <- convert_levels_to_type(levels, data_type)
        factor(rep(NA, n), levels = converted_levels)
      } else {
        switch(data_type,
               "text" = rep(NA_character_, n),
               "integer" = rep(NA_integer_, n),
               "float" = rep(NA_real_, n),
               "datetime" = rep(as.POSIXct(NA), n),
               "date" = rep(as.Date(NA), n),
               "time" = rep(hms::as_hms(NA), n),
               "partialdate" = rep(NA_character_, n),
               "partialtime" = rep(NA_character_, n),
               "partialdatetime" = rep(NA_character_, n),
               stop(paste("Data Type not supported:", data_type))
        )
      }
    } else {
      if (!is.null(levels)) {
        converted_levels <- convert_levels_to_type(levels, data_type)
        factor(sample(converted_levels, n, replace = TRUE), levels = converted_levels)
      } else {
        switch(data_type,
               "text" = toupper(replicate(n, paste0(sample(letters, length_attr, replace = TRUE), collapse = ""))),
               "integer" = {
                 max_val <- min(10^length_attr - 1, .Machine$integer.max)
                 sample.int(max_val, n, replace = TRUE)
               },
               "float" = {
                 max_value <- 10^(length_attr - significant_digits_attr - 1) - 1
                 round(runif(n, min = 0, max = max_value), digits = significant_digits_attr)
               },
               "datetime" = as.POSIXct("2000-01-01") + sample(0:(3600 * 24 * 1000), n, replace = TRUE),
               "date" = as.Date("2000-01-01") + sample(0:10000, n, replace = TRUE),
               "time" = hms::as_hms(sprintf("%02d:%02d:%02d", sample(0:23, n, TRUE), sample(0:59, n, TRUE), sample(0:59, n, TRUE))),
               "partialdate" = sample(c("2025-07", "2024", "2022-03"), n, replace = TRUE),
               "partialtime" = sample(c("14:30", "09", "08:15"), n, replace = TRUE),
               "partialdatetime" = sample(c("2025-07", "2022-03-12 10", "2024-12-31 23:45"), n, replace = TRUE),
               stop(paste("Data Type not supported:", data_type))
        )
      }
    }

    attr(simulated_col, 'levels') <- levels

    return(simulated_col)
  })

  simulated_dataset <- tibble::tibble(!!!rlang::set_names(cols, names(dataset)))

  simulated_dataset <- set_adam_attr(simulated_dataset, path, name)

  return(simulated_dataset)
}

