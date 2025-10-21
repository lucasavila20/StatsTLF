#' Export Dataset Package
#'
#' This function exports a dataset package to a specified file format.
#'
#' @param package An object of class `ContentPackage`.
#' @param spec A character string with path to the spec file.
#'
#' @return The path to the exported package.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create and export a content package
#' x <- create_content_package() |>
#'  add_to_package(
#'   create_content(
#'    create_content_backbone(
#'     'analysis of dataset',
#'     'T',
#'     function(dataset) {
#'      dataset |>
#'       flextable::flextable()
#'     }
#'    ),
#'    dataset = tibble::tibble(x = c(1, 2, 3))
#'   )
#' ) |>
#' export_package('teste')
#' }
export_datasets <- function(package, spec) {

  # Validation Step -------------------------------------------------------------
  stopifnot("Package must contain at least one content to be exported." = length(package@content_list) > 0)

  stopifnot(
    "Folder './04_Datasets' doesn't exist." = dir.exists(here::here('04_Datasets'))
  )

  stopifnot(
    "`spec` must be provided." = !is.na(spec),
    "`spec` must be a character." = is.character(spec),
    "`spec` cannot be an array." = length(spec) == 1
  )

  stopifnot("Spec not found." = file.exists(spec) | spec == '')

  types <- sapply(package@content_list, function(y) y@type)
  stopifnot('All contents must have `type` "L".' = all(types == "L"))

  export_names <- sapply(seq_along(package@content_list), function(i) {
    caux <- package@content_list[[i]]
    if (is.na(caux@export_name)) {
      return(paste0('adyy', i))
    } else {
      return(caux@export_name)
    }
  })
  stopifnot('Content `export_name` must be unique.' = !any(duplicated(export_names)))
  # -----------------------------------------------------------------------------

  return(export_datasets_method(x = package, spec = spec))
}
