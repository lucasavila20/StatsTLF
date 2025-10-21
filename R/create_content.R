#' Create Content from Content Backbone
#'
#' This function creates content using a specified content backbone and dataset. Additional parameters allow customization of subtitles, population, section, and dimensions for figures.
#'
#' @param content_backbone An object of class `ContentBackbone`.
#' @param subtitle A character string specifying the content subtitle (optional).
#' @param population A character string specifying the content population (optional).
#' @param section A character string specifying the content section (optional).
#' @param fdim A list of 3 elements: 'width', 'height', and 'dpi', each with a numeric value. This is only used when the type is 'F' (figure). Max Width, Height: 9 and 5. Min dpi: 600.
#' @param export_name A character string specifying the content export name. Must be unique between contents. If not specified, then a default name to content will be generated "adyyi". Only used for datasets export.
#' @param ... Extra arguments to pass to the 'fun' in the content backbone (optional).
#'
#' @return A content object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Create content from a content backbone with a dataset
#' x <- create_content(
#'  create_content_backbone(
#'   'analysis of dataset',
#'   'T',
#'   function(dataset) {
#'    dataset |>
#'     flextable::flextable()
#'   }
#'  ),
#'  dataset = tibble::tibble(x = c(1, 2, 3))
#' )
#' }
create_content <- function(content_backbone, subtitle = NA_character_, population = NA_character_, section = NA_character_, fdim = list(width = 9, height = 5, dpi = 600), export_name = NA_character_, ...) {

 # Validation Step -------------------------------------------------------------
 stopifnot("`content_backbone`must be a ContentBackbone object." = class(content_backbone) == 'ContentBackbone')

 stopifnot(
  "`subtitle` must be a character." = is.character(subtitle),
  "`subtitle` cannot be an array." = length(subtitle) == 1
 )

 stopifnot(
  "`population` must be a character." = is.character(population),
  "`population` cannot be an array." = length(population) == 1
 )

 stopifnot(
  "`section` must be a character." = is.character(section),
  "`section` cannot be an array." = length(section) == 1
 )

 value <- tryCatch(content_backbone@fun(...), error = function(e) stop(paste('Error applying the dataset to the content backbone. Please check `fun` code.\n', e)))

 stopifnot("Return of `fun` must be a flextable, tibble or ggplot object." = any(class(value) %in% c('flextable', 'gg', 'tbl_df', 'gtable')))

 if (content_backbone@type == 'T') stopifnot("`fun` must return a flextable for `type` = 'T'." = any(class(value) == 'flextable'))
 if (content_backbone@type == 'F') stopifnot("`fun` must return a ggplot for `type` = 'F'." = any(class(value) %in% c('gg', 'gtable')))
 if (content_backbone@type == 'L') stopifnot("`fun` must return a tibble for `type` = 'L'." = any(class(value) == 'tbl_df'))

 stopifnot(
   "`fdim` must be provided." = !is.na(fdim),
   "`fdim` must be a list." = is.list(fdim),
   "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = length(fdim) == 3,
   "`fdim` must have 3 elements with names: 'width', 'height' and 'dpi', respectively." = names(fdim) == c('width', 'height', 'dpi'),
   "`fdim` values must be numeric." = is.numeric(unlist(fdim)),
   "`fdim$width` must be less or equal to 9." = fdim$width <= 9,
   "`fdim$height` must be less or equal to 5." = fdim$height <= 5,
   "`fdim$dpi` must be greater or equal to 600." = fdim$dpi >= 600
 )

 stopifnot(
   "`export_name` must be a character." = is.character(export_name),
   "`export_name` cannot be an array." = length(export_name) == 1
 )
 # -----------------------------------------------------------------------------

 bk_name <- deparse(substitute(content_backbone))
 bk_name <- strsplit(bk_name, "\\$")[[1]]
 bk_name <- bk_name[length(bk_name)]

 return(create_content_method(x = content_backbone, value = value, subtitle = subtitle, population = population, section = section, fdim = fdim, export_name = export_name, bk_name = bk_name, ...))
}
