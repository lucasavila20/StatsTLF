#' Create Analysis Datasets
#'
#' This function generates an analysis dataset structure with zero rows based on the provided metadata file in .xlsx format. The metadata should follow the package template, including sheets for datasets, variables, and codelists.
#'
#' @param path A character string specifying the path to the metadata file in .xlsx format.
#'
#' @return A list of tibbles, each representing a dataset defined in the metadata file, with zero rows.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create an analysis datasets structure from 'Spec.xlsx'
#' x <- create_adam_datasets('Spec.xlsx')
#' }
create_adam_datasets <- function(path) {

  # Validation Step -------------------------------------------------------------
  stopifnot(
    "`path` must be a character." = is.character(path),
    "`path` cannot be an array." = length(path) == 1
  )

  stopifnot("File not found." = file.exists(path))

  ensure_columns <- function(data, cols) {
    for (col in cols) {
      if (!(col %in% names(data))) {
        data <- data |> dplyr::mutate(!!col := as.character(NA))
      }
    }
    return(data)
  }

 datasets_spec <- readxl::read_xlsx(path, col_types = 'text', sheet = 'Datasets')
 variables_spec <- readxl::read_xlsx(path, col_types = 'text', sheet = 'Variables')
 codelists_spec <- readxl::read_xlsx(path, col_types = 'text', sheet = 'Codelists')
 valuelevel_spec <- tryCatch(readxl::read_xlsx(path, col_types = 'text', sheet = 'ValueLevel'), error = function(e) tibble::tibble())
 methods_spec <- tryCatch(readxl::read_xlsx(path, col_types = 'text', sheet = 'Methods'), error = function(e) tibble::tibble())
 comments_spec <- tryCatch(readxl::read_xlsx(path, col_types = 'text', sheet = 'Comments'), error = function(e) tibble::tibble())

 # -----------------------------------------------------------------------------

 datasets_spec <- ensure_columns(datasets_spec, c("Dataset", "Label", "Class", "SubClass", "Structure", "Key Variables", "Standard",
                                                  "Has No Data", "Repeating", "Reference Data", "Comment", "Developer Notes"))

 variables_spec <- ensure_columns(variables_spec, c("Order", "Dataset", "Variable", "Label", "Data Type", "Length", "Significant Digits",
                                                    "Format", "Mandatory", "Assigned Value", "Codelist", "Common", "Origin", "Source",
                                                    "Pages", "Method", "Predecessor", "Role", "Has No Data", "Comment", "Developer Notes"))

 codelists_spec <- ensure_columns(codelists_spec, c("ID", "Name", "NCI Codelist Code", "Data Type", "Terminology", "Comment", "Order",
                                                    "Term", "NCI Term Code", "Decoded Value"))

 valuelevel_spec <- ensure_columns(valuelevel_spec, c("Order", "Dataset", "Variable", "Where Clause", "Label", "Data Type", "Length", "Significant Digits",
                                                      "Format", "Mandatory", "Assigned Value", "Codelist", "Origin", "Source",
                                                      "Pages", "Method", "Predecessor", "Comment", "Developer Notes"))

 methods_spec <- ensure_columns(methods_spec, c( "ID", "Name", "Type", "Description", "Expression Context", "Expression Code", "Document", "Pages"))

 comments_spec <- ensure_columns(comments_spec, c("ID", "Description", "Document", "Pages"))

 # -----------------------------------------------------------------------------

 names(datasets_spec) <- tolower(names(datasets_spec))
 names(variables_spec) <- tolower(names(variables_spec))
 names(codelists_spec) <- tolower(names(codelists_spec))
 names(valuelevel_spec) <- tolower(names(valuelevel_spec))
 names(methods_spec) <- tolower(names(methods_spec))
 names(comments_spec) <- tolower(names(comments_spec))

 codelists <- dplyr::filter(codelists_spec, !is.na(id)) |>
   dplyr::group_by(id) |>
   dplyr::summarise(values = list(term), .groups = "drop") |>
   tibble::deframe()

 create_column <- function(data_type, params) {
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

   col <- switch(
     tolower(data_type),
     "text" = character(),
     "integer" = integer(),
     "float" = numeric(),
     "datetime" = as.POSIXct(character()),
     "date" = as.Date(character()),
     "time" = hms::as_hms(character()),
     "partialdate" = character(),
     "partialtime" = character(),
     "partialdatetime" = character(),
     character()
   )

   if (!is.null(params$codelist) && params$codelist %in% names(codelists)) {
     levels_converted <- convert_levels_to_type(codelists[[params$codelist]], data_type)
     col <- factor(col, levels = levels_converted)
   }

   if (!is.null(attr(col, "tzone"))) attr(col, "tzone") <- NULL

   attr(col, "order") <- params$order
   attr(col, "dataset") <- params$dataset
   attr(col, "variable") <- params$variable
   attr(col, "label") <- params$label
   attr(col, "data_type") <- data_type
   attr(col, "length") <- params$length
   attr(col, "significant_digits") <- params$significant_digits
   attr(col, "format") <- params$format
   attr(col, "mandatory") <- params$mandatory
   attr(col, "closed") <- !is.null(params$codelist) && params$codelist %in% names(codelists)

   if (is.factor(col)) {
     attr(col, "codelist") <- levels_converted
   } else {
     attr(col, "codelist") <- NA_character_
   }

   attr(col, "origin") <- params$origin
   attr(col, "source") <- params$source

   if (!(is.na(params$method) | length(params$method) == 0)) {
     met_spec <- dplyr::filter(methods_spec, id == params$method)

     attr(col, "method_id") <- ifelse(length(met_spec$id) == 0, NA_character_, met_spec$id)
     attr(col, "method_name") <- ifelse(length(met_spec$name) == 0, NA_character_, met_spec$name)
     attr(col, "method_type") <- ifelse(length(met_spec$type) == 0, NA_character_, met_spec$type)
     attr(col, "method_description") <- ifelse(length(met_spec$description) == 0, NA_character_, met_spec$description)
     attr(col, "method_expression_context") <- ifelse(length(met_spec$`expression context`) == 0, NA_character_, met_spec$`expression context`)
     attr(col, "method_expression_code") <- ifelse(length(met_spec$`expression code`) == 0, NA_character_, met_spec$`expression code`)
   } else {
     attr(col, "method_id") <- NA_character_
     attr(col, "method_name") <- NA_character_
     attr(col, "method_type") <- NA_character_
     attr(col, "method_description") <- NA_character_
     attr(col, "method_expression_context") <- NA_character_
     attr(col, "method_expression_code") <- NA_character_
   }

   attr(col, "predecessor") <- params$predecessor
   attr(col, "has_no_data") <- params$has_no_data

   if (!(is.na(params$comment) | length(params$comment) == 0)) {
     com_spec <- dplyr::filter(comments_spec, id == params$comment)

     attr(col, "comment") <- ifelse(length(com_spec$description) == 0, NA_character_, com_spec$description)
   } else {
     attr(col, "comment") <- NA_character_
   }

   valuelevels <- valuelevel_spec |> dplyr::filter(dataset == params$dataset & variable == params$variable)

   if (dim(valuelevels)[1] > 0) {
     attr(col, "valuelevel_where_clause") <- paste0(valuelevels$`where clause`, collapse = '; ')
     attr(col, "valuelevel_codelist") <- paste0(valuelevels$codelist, collapse = '; ')
     attr(col, "valuelevel_origin") <- paste0(valuelevels$origin, collapse = '; ')
     attr(col, "valuelevel_source") <- paste0(valuelevels$source, collapse = '; ')
     attr(col, "valuelevel_predecessor") <- paste0(valuelevels$predecessor, collapse = '; ')
     attr(col, "valuelevel_method_id") <- paste0(valuelevels$method, collapse = '; ')
     attr(col, "valuelevel_comment_id") <- paste0(valuelevels$comment, collapse = '; ')

     attr(col, "valuelevel_where_clause") <- ifelse(attr(col, "valuelevel_where_clause") == 'NA; NA', NA_character_, attr(col, "valuelevel_where_clause"))
     attr(col, "valuelevel_codelist") <- ifelse(attr(col, "valuelevel_codelist") == 'NA; NA', NA_character_, attr(col, "valuelevel_codelist"))
     attr(col, "valuelevel_origin") <- ifelse(attr(col, "valuelevel_origin") == 'NA; NA', NA_character_, attr(col, "valuelevel_origin"))
     attr(col, "valuelevel_source") <- ifelse(attr(col, "valuelevel_source") == 'NA; NA', NA_character_, attr(col, "valuelevel_source"))
     attr(col, "valuelevel_predecessor") <- ifelse(attr(col, "valuelevel_predecessor") == 'NA; NA', NA_character_, attr(col, "valuelevel_predecessor"))
     attr(col, "valuelevel_method_id") <- ifelse(attr(col, "valuelevel_method_id") == 'NA; NA', NA_character_, attr(col, "valuelevel_method_id"))
     attr(col, "valuelevel_comment_id") <- ifelse(attr(col, "valuelevel_comment_id") == 'NA; NA', NA_character_, attr(col, "valuelevel_comment_id"))
   } else {
     attr(col, "valuelevel_where_clause") <- NA_character_
     attr(col, "valuelevel_codelist") <- NA_character_
     attr(col, "valuelevel_origin") <- NA_character_
     attr(col, "valuelevel_source") <- NA_character_
     attr(col, "valuelevel_predecessor") <- NA_character_
     attr(col, "valuelevel_method_id") <- NA_character_
     attr(col, "valuelevel_comment_id") <- NA_character_
   }

   col
 }

 datasets <- list()

 for (dataset_name in datasets_spec$dataset) {
   var_spec <- dplyr::filter(variables_spec, tolower(dataset) == tolower(dataset_name))
   das_spec <- dplyr::filter(datasets_spec, tolower(dataset) == tolower(dataset_name))

   col_list <- purrr::map2(
     .x = var_spec$`data type`,
     .y = var_spec$variable,
     .f = function(dtype, varname) {

       params <- list(
         "order" = var_spec$order[var_spec$variable == varname],
         "dataset" = var_spec$dataset[var_spec$variable == varname],
         "variable" = var_spec$variable[var_spec$variable == varname],
         "label" = var_spec$label[var_spec$variable == varname],
         "length" = var_spec$length[var_spec$variable == varname],
         "significant_digits" = var_spec$`significant digits`[var_spec$variable == varname],
         "format" = var_spec$format[var_spec$variable == varname],
         "mandatory" = var_spec$mandatory[var_spec$variable == varname],
         "codelist" = var_spec$codelist[var_spec$variable == varname],
         "origin" = var_spec$origin[var_spec$variable == varname],
         "source" = var_spec$source[var_spec$variable == varname],
         "method" = var_spec$method[var_spec$variable == varname],
         "predecessor" = var_spec$predecessor[var_spec$variable == varname],
         "has_no_data" = var_spec$`has no data`[var_spec$variable == varname],
         "comment" = var_spec$comment[var_spec$variable == varname]
       )

       col <- create_column(dtype, params)
       rlang::set_names(list(col), varname)
     }
   ) |> purrr::flatten()

   datasets[[dataset_name]] <- tibble::as_tibble(col_list)
   attr(datasets[[dataset_name]], "path") <- path
   attr(datasets[[dataset_name]], "name") <- dataset_name
   attr(datasets[[dataset_name]], "label") <- ifelse(length(das_spec$label) == 0, NA_character_, das_spec$label)
   attr(datasets[[dataset_name]], "dataset_class") <- ifelse(length(das_spec$class) == 0, NA_character_, das_spec$class)
   attr(datasets[[dataset_name]], "dataset_subclass") <- ifelse(length(das_spec$subclass) == 0, NA_character_, das_spec$subclass)
   attr(datasets[[dataset_name]], "dataset_structure") <- ifelse(length(das_spec$structure) == 0, NA_character_, das_spec$structure)
   attr(datasets[[dataset_name]], "key_variables") <- ifelse(length(das_spec$`key variables`) == 0, NA_character_, das_spec$`key variables`)
   attr(datasets[[dataset_name]], "standard") <- ifelse(length(das_spec$standard) == 0, NA_character_, das_spec$standard)
   attr(datasets[[dataset_name]], "has_no_data") <- ifelse(length(das_spec$`has no data`) == 0, NA_character_, das_spec$`has no data`)
   attr(datasets[[dataset_name]], "repeating") <- ifelse(length(das_spec$repeating) == 0, NA_character_, das_spec$repeating)
   attr(datasets[[dataset_name]], "reference_data") <- ifelse(length(das_spec$`reference data`) == 0, NA_character_, das_spec$`reference data`)

   if (!(is.na(das_spec$comment) | length(das_spec$comment) == 0)) {
     com_spec <- dplyr::filter(comments_spec, id == das_spec$comment)

     attr(datasets[[dataset_name]], "comment") <- ifelse(length(com_spec$description) == 0, NA_character_, com_spec$description)
   } else {
     attr(datasets[[dataset_name]], "comment") <- NA_character_
   }
 }

 return(datasets)
}
