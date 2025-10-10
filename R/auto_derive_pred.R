#' Automatically Derive Variables Based on Predecessors
#'
#' This function automates the derivation of variables in a dataset based on their "Predecessor" origin attribute. It identifies variables that match specific conditions and applies the `StatsTLF::derive()` function. Variables that cannot be derived are listed in a warning message.
#'
#' @param dataset A data frame or tibble containing the dataset to process. Columns must have `origin` and `predecessor` attributes.
#' @param from A named list of datasets. The names correspond to dataset identifiers, and each element is a dataset containing the variables referenced in the `predecessor` attributes.
#' @param by A list of expressions used for grouping (default is `rlang::exprs(USUBJID)`).
#'
#' @return The dataset with derived variables updated where applicable.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Identifies columns in `dataset` where:
#'     \itemize{
#'       \item The `origin` attribute is `"Predecessor"`.
#'       \item The `predecessor` attribute specifies a dataset and variable in the format `dataset_name.variable_name`.
#'       \item The dataset (`dataset_name`) exists in the `from` list, and the variable (`variable_name`) exists in the corresponding dataset.
#'     }
#'   \item For each valid column, applies `StatsTLF::derive()` to derive the variable.
#'   \item Variables that cannot be derived (due to missing datasets or variables) are listed in a warning message.
#' }
#'
#' @examples
#' # Example usage
#' dataset <- tibble::tibble(VAR1 = c(1, 2, 3))
#' attr(dataset$VAR1, "origin") <- "Predecessor"
#' attr(dataset$VAR1, "predecessor") <- "SOURCE.VAR1"
#' from <- list(SOURCE = tibble::tibble(VAR1 = c(4, 5, 6)))
#'
#' auto_derive_pred(dataset, from = from)
#'
#' @seealso \code{\link[StatsTLF]{derive}} for the derivation logic.
#'
#' @export
auto_derive_pred <- function(dataset, from, by = rlang::exprs(USUBJID)) {
  valid_cols <- base::colnames(dataset)[purrr::map_lgl(
    base::colnames(dataset),
    ~ {
      attr_origin <- base::attributes(dataset[[.x]])$origin
      attr_predecessor <- base::attributes(dataset[[.x]])$predecessor
      if (!is.null(attr_origin) && !is.null(attr_predecessor)) {
        attr_origin == "Predecessor" &&
          base::sub("\\..*", "", attr_predecessor) %in% names(from)
      } else {
        FALSE
      }
    }
  )]

  not_derived <- c()

  dataset <- purrr::reduce(valid_cols, function(data, col) {
    predecessor <- base::attributes(data[[col]])$predecessor
    dataset_name <- base::sub("\\..*", "", predecessor)
    variable_name <- base::sub(".*\\.", "", predecessor)

    if (!dataset_name %in% names(from) || !(variable_name %in% colnames(from[[dataset_name]]))) {
      not_derived <<- c(not_derived, col)
      return(data)
    }

    default_expr <- StatsTLF::derive_expr(from[[dataset_name]][[variable_name]])
    attr(default_expr, "dataset_name") <- dataset_name
    attr(default_expr, "variable_name") <- variable_name

    data |>
      StatsTLF::derive(
        var = !!col,
        from = from,
        by = by,
        default = default_expr
      )
  }, .init = dataset)

  if (length(not_derived) > 0) {
    cli::cli_alert_warning(
      "The following variables were not derived: {paste(not_derived, collapse = ', ')}"
    )
  }

  return(dataset)
}
