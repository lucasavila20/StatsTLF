#' Export Report Package
#'
#' This function exports a report package to a specified file format using a template. It allows customization of the template, and other export options such as adding a table of contents and supplementary content.
#'
#' @param package An object of class `ContentPackage`.
#' @param template_name A character string specifying the name of the template report file located in the `./00_Template` folder. Defaults to "template_PT-BR.docx".
#' @param supp A boolean indicating whether 'S' should be added before the content number in the export. Defaults to FALSE.
#' @param add_toc A boolean indicating whether a table of contents should be added to the report. Defaults to TRUE.
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
#' export_report('teste')
#' }
export_report <- function(package, template_name = "template_PT-BR.docx", supp = FALSE, add_toc = TRUE, spec = "") {

 # Validation Step -------------------------------------------------------------
 stopifnot(
  "`template_name` must be provided." = !is.na(template_name),
  "`template_name` must be a character." = is.character(template_name),
  "`template_name` cannot be an array." = length(template_name) == 1
 )

 stopifnot("Template file in `template_name` doesn't exist." = file.exists(paste0(here::here('00_Template'), '\\', template_name)))

 stopifnot(
   "`supp` must be provided." = !is.na(supp),
   "`supp` must be a boolean." = is.logical(supp),
   "`supp` cannot be an array." = length(supp) == 1
 )

 stopifnot(
   "Folder './05_Results' doesn't exist." = dir.exists(here::here('05_Results'))
 )

 stopifnot("Package must contain at least one content to be exported." = length(package@content_list) > 0)

 stopifnot(
   "`add_toc` must be provided." = !is.na(add_toc),
   "`add_toc` must be a boolean." = is.logical(add_toc),
   "`add_toc` cannot be an array." = length(add_toc) == 1
 )

 stopifnot(
   "`spec` must be provided." = !is.na(spec),
   "`spec` must be a character." = is.character(spec),
   "`spec` cannot be an array." = length(spec) == 1
 )

 stopifnot("Spec not found." = file.exists(spec) | spec == '')

 # -----------------------------------------------------------------------------

 return(export_report_method(x = package, template_name = template_name, supp = supp, add_toc = add_toc, spec = spec))
}
