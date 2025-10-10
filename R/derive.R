#' Derive Variable in an ADaM Dataset with Complex Conditions
#'
#' This function derives a variable in a primary dataset (e.g., ADSL) based on complex conditions involving multiple auxiliary datasets. It supports multiple conditional assignments and a default value.
#'
#' @param .data A data frame (primary dataset) where the variable will be derived.
#' @param var Symbol. Name of the variable to be derived in `.data`.
#' @param from Named list of data frames. Auxiliary datasets used in conditional expressions. Each dataset must be named, e.g., `list(ADPD = adpd_df, ADIS = adis_df)`.
#' @param cases List of lists. Each inner list must have two elements: `condition` and `value`.
#'   - `condition`: an expression evaluated within the environment containing `.data` and `from` datasets. Use `derive_expr()` to encapsulate the condition.
#'   - `value`: expression or constant to assign to `var` when `condition` is TRUE.
#' The conditions are evaluated in order; the first matching case has precedence.
#' @param by Symbol. The subject identifier variable name common to `.data` and all datasets in `from`. Used to match rows across datasets (default is `"USUBJID"`).
#' @param default Value or expression assigned to `var` for rows where no `cases` condition matches. Defaults to `NA`.
#'
#' @return The original dataset `.data` with the variable `var` derived accordingly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Derive a variable in ADSL based on conditions from ADPD
#' adsl <- derive(adsl, var = FASFL,
#'   from = list(adpd = adpd_df),
#'   cases = list(
#'     list(
#'       condition = derive_expr(ADSL$RANDFL == "Y" & ADPD$PARAMCD == "HI - A H5N8" & ADPD$AVAL != "Vazio"),
#'       value = derive_expr("Y")
#'     )
#'   ),
#'   default = derive_expr("N")
#' )
#' }
derive <- function(.data, var, from = list(), cases = list(), by = USUBJID, default = NA) {
  var <- rlang::ensym(var)
  by_str <- purrr::map_chr(by, rlang::as_string)
  var_str <- rlang::as_string(var)

  ### Validate Inputs ----------------------------------------------------------
  stopifnot(
    '`var` and `by` cannot be the same variable.' = !any(by_str == var_str),
    "`var` must be a single name or symbol." = rlang::is_symbol(var) || (is.character(var) && length(var) == 1),
    "`var` must exist in .data." = var_str %in% colnames(.data),
    "`by` must exist in .data." = all(by_str %in% colnames(.data)),
    "`from` must be a list." = is.list(from),
    "`.data` must be created via `create_adam_datasets()` function." = !is.null(attr(.data, 'name'))
  )

  if (length(from) > 0) {
    current_names <- names(from)
    names(from) <- purrr::map_chr(seq_along(from), function(i) {
      n <- current_names[i]
      if (!is.null(n) && nzchar(n)) {
        n
      } else {
        attr_name <- attr(from[[i]], "name")
        if (!is.null(attr_name) && nzchar(attr_name)) attr_name else ""
      }
    })

    stopifnot("All elements of `from` must have valid names." = all(nzchar(names(from))))
  }

  stopifnot(
    "`cases` must be a list." = is.list(cases),
    "Each element of `cases` must be a list with `condition` and `value`." =
      length(cases) == 0 || all(
        vapply(cases, function(case) {
          is.list(case) && all(c("condition", "value") %in% names(case)) && length(case) == 2
        }, logical(1))
      )
  )

  ### Setup Environment --------------------------------------------------------
  cat('Deriving', var_str, '... ')

  main <- attr(.data, 'name')

  levels <- if (is.factor(.data[[var_str]])) levels(.data[[var_str]]) else NULL

  datasets_env <- from
  datasets_env[[main]] <- .data

  # Prepare result vector (keep column type/levels)
  result_col <- .data[[var_str]]
  result_col[!is.na(result_col)] <- NA

  n <- nrow(.data)

  # Precompute key for .data (single or composite)
  if (length(by_str) == 1) {
    data_keys <- as.character(.data[[by_str]])
  } else {
    data_keys <- do.call(paste0, .data[, by_str, drop = FALSE])
  }

  caller_env <- rlang::caller_env()

  cum_keys <- c()

  ### Loop cases ---------------------------------------------------------------
  for (case in cases) {
    ### Evaluate condition -----------------------------------------------------
    cond_expr <- rlang::get_expr(case$condition)
    val_expr  <- rlang::get_expr(case$value)

    condition_result <- if (rlang::is_call(cond_expr) || rlang::is_symbol(cond_expr)) {
      rlang::eval_tidy(cond_expr, data = datasets_env, env = caller_env)
    } else {
      rep(cond_expr, n)
    }

    cond_expr_text <- rlang::expr_text(cond_expr)
    match_ds <- unique(stringr::str_extract_all(cond_expr_text, "\\b\\w+(?=\\$)")[[1]])

    stopifnot("`condition` must depend only on the main dataset." = length(match_ds) == 0 | all(match_ds == main))

    if (!is.logical(condition_result)) stop("`condition` must evaluate to a logical vector.")
    stopifnot("`condition` length does not match the main dataset." = length(condition_result) == n)

    if (!any(condition_result, na.rm = TRUE)) next
    matched_keys <- unique(data_keys[condition_result])

    ### Evaluate value ---------------------------------------------------------
    value_result <- if (rlang::is_call(val_expr) || rlang::is_symbol(val_expr)) {
      rlang::eval_tidy(val_expr, data = datasets_env, env = caller_env)
    } else {
      rep(val_expr, n)
    }

    assign_indices <- which(data_keys %in% matched_keys)
    assign_indices <- assign_indices[!assign_indices %in% cum_keys]

    val_expr_text <- rlang::expr_text(val_expr)
    match_ds <- unique(stringr::str_extract_all(val_expr_text, "\\b\\w+(?=\\$)")[[1]])

    if (length(match_ds) == 0) {
      result_col[assign_indices] <- value_result[assign_indices]
      cum_keys <- c(cum_keys, unique(assign_indices))
    } else if (length(match_ds) > 1) {
      stop('`Value` must come from only one dataset.')
    } else {
      src_df <- datasets_env[[match_ds]]

      src_keys <- if (length(by_str) == 1) {
        as.character(src_df[[by_str]])
      } else {
        do.call(paste0, src_df[, by_str, drop = FALSE])
      }

      matched_pos <- match(data_keys[assign_indices], src_keys)

      aligned_values <- value_result[matched_pos]

      result_col[assign_indices] <- aligned_values
      cum_keys <- c(cum_keys, unique(assign_indices))
    }
  }

  ### Add Default Value --------------------------------------------------------
  if (!identical(default, NA)) {
    default_expr <- rlang::get_expr(default)

    dataset_name_attr <- attr(default, "dataset_name")
    variable_name_attr <- attr(default, "variable_name")

    default_result <- if (rlang::is_call(default_expr) || rlang::is_symbol(default_expr)) {
      rlang::eval_tidy(default_expr, data = datasets_env, env = caller_env)
    } else {
      rep(default_expr, n)
    }

    seq_n <- seq_len(n)
    default_indices <- which(!seq_n %in% cum_keys)

    if (!all(is.na(result_col))) {
      existing_type <- class(stats::na.omit(result_col)[1])
      default_type <- class(stats::na.omit(default_result)[1])
      is_compatible <- function(existing, incoming) {
        (existing == incoming) ||
          (existing == 'character' & incoming == 'factor') ||
          (existing == 'factor' & incoming == 'character') ||
          (existing == 'double' & incoming == 'integer') ||
          (existing == 'integer' & incoming == 'double')
      }
      stopifnot("Validation error: Default value type mismatch for variable `var`." = is_compatible(existing_type, default_type))
    }

    if (!is.null(levels)) {
      default_values <- unique(na.omit(default_result[default_indices]))
      invalid_defaults <- setdiff(default_values, levels)
      stopifnot("Validation error: Default value(s) assigned to factor `var` — not in defined levels: " = length(invalid_defaults) == 0)
    }

    if (is.null(dataset_name_attr) || is.null(variable_name_attr)) {
      result_col[default_indices] <- default_result[default_indices]

    } else {
      src_df <- datasets_env[[dataset_name_attr]]

      src_keys <- if (length(by_str) == 1) {
        as.character(src_df[[by_str]])
      } else {
        do.call(paste0, src_df[, by_str, drop = FALSE])
      }

      matched_pos <- match(data_keys[default_indices], src_keys)
      aligned_values <- src_df[[variable_name_attr]][matched_pos]

      result_col[default_indices] <- aligned_values
    }
  }

  ### Set results & finalize ---------------------------------------------------
  .data[[var_str]] <- result_col

  cat('Ok\n')

  return(.data)
}
