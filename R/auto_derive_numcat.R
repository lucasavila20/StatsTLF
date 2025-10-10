#' Automatically Derive Numeric Categories
#'
#' Automates the derivation of numeric categories in a dataset by identifying variable pairs based on their attributes and applying the `derive2` function.
#'
#' @param dataset A data frame or tibble. Columns must have `method_id`, `origin`, and `label` attributes.
#' @param numcat_method A string specifying the `method_id` to filter variables for derivation.
#' @param by Symbol. The subject identifier variable name common to `.data` and all datasets in `from`. Used to match rows across datasets (default is `"USUBJID"`).
#'
#' @return The modified dataset with derived numeric categories applied where applicable.
#'
#' @details
#' Filters variables with `method_id == numcat_method` and `origin == "Derived"`. Identifies pairs where one variable's `label` ends with "(N)" and another has the same `label` without "(N)`. Applies `derive2` to such pairs. Lists variables that could not be derived automatically.
#'
#' @examples
#' # Example usage
#' auto_derive(dataset, "example_method", by = rlang::exprs(USUBJID))
#'
#' @export
auto_derive_numcat <- function(dataset, numcat_method, by) {
  cols <- colnames(dataset)
  filtered_indices <- which(
    purrr::map_lgl(cols, ~ attr(dataset[[.x]], "method_id") == numcat_method & attr(dataset[[.x]], "origin") == "Derived")
  )

  not_derived <- c()
  for (j in filtered_indices) {
    target_label <- attr(dataset[[cols[j]]], "label")
    source_index <- which(
      purrr::map_lgl(cols, ~ attr(dataset[[.x]], "label") == stringr::str_remove(target_label, " \\(N\\)") &&
                       .x != cols[j])
    )

    if (length(source_index) == 1) {
      dataset <- dataset |>
        StatsTLF::derive2(
          var_target = !!cols[j],
          var_source = !!cols[source_index],
          by = by
        )
    } else {
      not_derived <- c(not_derived, cols[j])
    }
  }

  if (length(not_derived) > 0) {
    cli::cli_alert_warning(
      "The following variables were not auto derived for {numcat_method}: {paste(not_derived, collapse = ', ')}"
    )
  }

  return(dataset)
}
