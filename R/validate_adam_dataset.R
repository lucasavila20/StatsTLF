#' Validate ADaM Dataset Against Metadata File
#'
#' This function validates a dataset by comparing it against a metadata file, checking for structural mismatches and attribute inconsistencies. It reports any issues found and suggests using `set_adam_attr()` to fix missing metadata paths.
#'
#' @param dataset A tibble containing the dataset to be validated.
#' @param print A boolean indicating if validation process should be printed..
#'
#' @return TRUE if the dataset is validated successfully; FALSE if there are mismatches.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Validate a dataset against the 'ADSL.xlsx' metadata file
#' x <- validate_adam_dataset(dataset)
#' }
validate_adam_dataset <- function(dataset, print = TRUE) {
  # --- 0. Check metadata path -------------------------------------------------
  path <- attr(dataset, "path")
  if (is.null(path)) {
    stop("`Path` is missing. Please, use `set_adam_attr()` to fix this.")
  }

  template <- create_adam_datasets(path)
  target_name <- attr(dataset, "name")

  template_names <- vapply(template, function(x) attr(x, "name"), character(1))
  match_idx <- match(target_name, template_names)

  stopifnot("Dataset name not found in metadata." = !is.na(match_idx))
  template <- template[[match_idx]]

  issues <- list()
  issues_attr <- list()

  # --- 1. Check column structure ----------------------------------------------
  cols_dataset  <- names(dataset)
  cols_template <- names(template)

  missing_cols <- setdiff(cols_template, cols_dataset)
  extra_cols   <- setdiff(cols_dataset, cols_template)

  if (length(missing_cols)) issues[["Missing columns"]] <- missing_cols
  if (length(extra_cols))   issues[["Extra columns"]]   <- extra_cols

  if (!length(missing_cols) && !length(extra_cols) &&
      !identical(cols_dataset, cols_template)) {
    issues[["Column order mismatch"]] <- list(
      dataset_order = cols_dataset,
      template_order = cols_template
    )
  }

  # --- 2. Compare common columns ----------------------------------------------
  common_cols <- intersect(cols_template, cols_dataset)

  attrs_template_all <- lapply(template[common_cols], attributes)
  attrs_dataset_all  <- lapply(dataset[common_cols], attributes)

  for (col in common_cols) {
    col_dataset  <- dataset[[col]]
    col_template <- template[[col]]

    attr_template <- attrs_template_all[[col]]
    attr_dataset  <- attrs_dataset_all[[col]]

    # --- Check classes --------------------------------------------------------
    class_dataset  <- class(col_dataset)
    class_template <- class(col_template)
    if (!identical(class_dataset, class_template)) {
      issues[[paste0("Class mismatch: ", col)]] <- list(
        expected = class_template,
        found = class_dataset
      )
    }

    # --- Check levels (if fator) ----------------------------------------------
    if ("factor" %in% class_template) {
      levels_dataset  <- levels(col_dataset)
      levels_template <- levels(col_template)
      if (!identical(levels_dataset, levels_template)) {
        issues[[paste0("Levels mismatch: ", col)]] <- list(
          expected = levels_template,
          found = levels_dataset
        )
      }
    }

    # --- Check max length -----------------------------------------------------
    if (!(attr_template$data_type %in% c("datetime", "date")) &&
        !all(is.na(col_dataset)) && !is.na(attr_template$length) &&
        is.character(col_dataset)) {
      max_len <- max(nchar(col_dataset), na.rm = TRUE)
      len_limit <- as.numeric(attr_template$length)
      if (max_len > len_limit) {
        issues[[paste0("Length above prespecified limit: ", col)]] <- list(
          expected = len_limit,
          found = max_len
        )
      }
    }

    # --- Check has no data ----------------------------------------------------
    if (identical(attr_template$has_no_data, "Yes") && any(!is.na(col_dataset))) {
      non_na <- col_dataset[!is.na(col_dataset)]
      preview <- head(non_na, 3)
      msg <- if (length(non_na) > 3)
        list(expected = NA, found = c(noquote("For example:"), paste(format(preview), collapse = ", ")))
      else
        list(expected = NA, found = preview)

      issues[[paste0("Data in column that should be empty: ", col)]] <- msg
    }

    # --- Additional attributes ------------------------------------------------
    attr_names <- setdiff(names(attr_template), c("levels", "class"))
    mismatched <- vapply(attr_names, function(a) {
      !identical(attr_template[[a]], attr_dataset[[a]])
    }, logical(1))

    if (any(mismatched)) {
      issues_attr[[col]] <- attr_names[mismatched]
    }
  }

  # --- Final result -----------------------------------------------------------
  if (!length(issues) && !length(issues_attr)) {
    if (print) message("✅ No structural mismatches found between dataset and metadata")
    result <- TRUE
  } else {
    if (print) {
      message("⚠️ Structural mismatches found:\n")
      for (issue in names(issues)) {
        cat("•", issue, ":\n")
        print(issues[[issue]])
        cat("\n")
      }
      for (issue_attr in names(issues_attr)) {
        cat("• Attributes mismatch:", issue_attr, ":", paste(issues_attr[[issue_attr]], collapse = ", "), "\n\n")
      }
    }
    result <- FALSE
  }

  invisible(result)
}
