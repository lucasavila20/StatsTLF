# S4 Class 'ContentBackbone' ---------------------------------------------------
setClass(
 'ContentBackbone',
 slots = c(
  title = 'character',
  type = 'character',
  fun = 'function'
 )
)

# 'ContentBackbone' methods -----------------------------------------------------
setGeneric('create_content_method', function(x, value, subtitle, population, section, fdim, export_name, bk_name, ...) standardGeneric('create_content_method'))
setMethod('create_content_method', 'ContentBackbone', function(x, value, subtitle, population, section, fdim, export_name, bk_name, ...) {

 cat('  \u2500 Creating content:\n')
 cat('    \u2500 Type:', ifelse(is.na(x@type), "", x@type), '\n')
 cat('    \u2500 Title:', ifelse(is.na(x@title), "", x@title), '\n')
 cat('    \u2500 Subtitle:', ifelse(is.na(subtitle), "", subtitle), '\n')
 cat('    \u2500 Population:', ifelse(is.na(population), "", population), '\n')
 cat('    \u2500 Section:', ifelse(is.na(section), "", section), '\n')

 content <- new(
  'Content',
  section = section,
  title = x@title,
  subtitle = subtitle,
  population = population,
  type = x@type,
  content = value,
  fdim = fdim,
  export_name = export_name,
  fun = x@fun,
  bk_name = bk_name
 )

 cat('  Done!\n')

 return(content)
})

# S4 Class 'Content' -----------------------------------------------------------

if (is.null(getClassDef("flextable"))) {
  setOldClass('flextable')
}

if (is.null(getClassDef("gg"))) {
  setOldClass("gg")
}

if (is.null(getClassDef("tbl_df"))) {
  setOldClass('tbl_df')
}

if (is.null(getClassDef("gtable"))) {
  setOldClass("gtable")
}

if (is.null(getClassDef("patchwork"))) {
  setOldClass("patchwork")
}

if (is.null(getClassDef("flextableORggORtbl_dfORgtableORpatchwork"))) {
  setClassUnion('flextableORggORtbl_dfORgtableORpatchwork', c('flextable', 'gg', 'tbl_df', 'gtable', 'patchwork'))
}

setClass(
 'Content',
 slots = c(
  section = 'character',
  title = 'character',
  subtitle = 'character',
  population = 'character',
  type = 'character',
  content = 'flextableORggORtbl_dfORgtableORpatchwork',
  fdim = 'list',
  export_name = 'character',
  fun = 'function',
  bk_name = 'character'
 )
)

# 'Content' methods ------------------------------------------------------------

setGeneric('prepare_to_export_method', function(x, number, doc, sep_subtitle, sep_population, fig_path, last, language, supp) standardGeneric('prepare_to_export_method'))
setMethod('prepare_to_export_method', 'Content', function(x, number, doc, sep_subtitle, sep_population, fig_path, last, language, supp) {

 if (language == 'PT-BR') {
  table_text <- 'Tabela '
  figure_text <- 'Figura '
  listing_text <- 'Lista '
 } else if (language == 'EN-US') {
  table_text <- 'Table '
  figure_text <- 'Figure '
  listing_text <- 'Listing '
 }

 if (x@type == 'T') {
   if (supp == TRUE) {
     title_text <- paste0(table_text, 'S', number, ': ', x@title)
   } else {
     title_text <- paste0(table_text, number, ': ', x@title)
   }
  text_style <- 'StatsTLF T\u00edtulo 2'
  else_style <- NA_character_
 } else if (x@type == 'F') {
   if (supp == TRUE) {
     title_text <- paste0(figure_text, 'S', number, ': ', x@title)
   } else {
     title_text <- paste0(figure_text, number, ': ', x@title)
   }
  text_style <- 'StatsTLF T\u00edtulo 2'
  else_style <- 'StatsTLF Normal Centralizado'
  img_path <- paste0(fig_path, '/', 'Figure_', number)
 } else if (x@type == 'L') {
   if (supp == TRUE) {
     title_text <- paste0(listing_text, 'S', number, ': ', x@title)
   } else {
     title_text <- paste0(listing_text, number, ': ', x@title)
   }
  text_style <- 'StatsTLF T\u00edtulo 2'
  else_style <- 'StatsTLF Tabela 1'
 }

 if (!is.na(x@subtitle)) {
  if (!is.na(x@population)) {
   if (sep_subtitle == 'newline' & sep_population == 'newline') {
     fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@subtitle, officer::run_linebreak(), x@population)
     fpar_text2 <- officer::block_list(
       officer::fpar(title_text, fp_p = officer::fp_par(text.align = "center")),
       officer::run_linebreak(),
       officer::fpar(x@subtitle, fp_p = officer::fp_par(text.align = "center")),
       officer::run_linebreak(),
       officer::fpar(x@population, fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
   if (sep_subtitle == 'newline' & sep_population != 'newline') {
     fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@subtitle, sep_population, x@population)
     fpar_text2 <- officer::block_list(
       officer::fpar(title_text, fp_p = officer::fp_par(text.align = "center")),
       officer::run_linebreak(),
       officer::fpar(paste0(x@subtitle, sep_population, x@population), fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
   if (sep_subtitle != 'newline' & sep_population == 'newline') {
     fpar_text <- officer::fpar(title_text, sep_subtitle, x@subtitle, officer::run_linebreak(), x@population)
     fpar_text2 <- officer::block_list(
       officer::fpar(paste0(title_text, sep_subtitle, x@subtitle), fp_p = officer::fp_par(text.align = "center")),
       officer::run_linebreak(),
       officer::fpar(x@population, fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
   if (sep_subtitle != 'newline' & sep_population != 'newline') {
     fpar_text <- officer::fpar(title_text, sep_subtitle, x@subtitle, sep_population, x@population)
     fpar_text2 <- officer::block_list(
       officer::fpar(paste0(title_text, sep_subtitle, x@subtitle, sep_population, x@population), fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
  } else {
   if (sep_subtitle == 'newline') {
     fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@subtitle)
     fpar_text2 <- officer::block_list(
       officer::fpar(title_text, fp_p = officer::fp_par(text.align = "center")),
       officer::run_linebreak(),
       officer::fpar(x@subtitle, fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
   if (sep_subtitle != 'newline') {
     fpar_text <- officer::fpar(title_text, sep_subtitle, x@subtitle)
     fpar_text2 <- officer::block_list(
       officer::fpar(paste0(title_text, sep_subtitle, x@subtitle), fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
  }
 } else {
  if (!is.na(x@population)) {
   if (sep_population == 'newline') {
     fpar_text <- officer::fpar(title_text, officer::run_linebreak(), x@population)
     fpar_text2 <- officer::block_list(
       officer::fpar(title_text, fp_p = officer::fp_par(text.align = "center")),
       officer::run_linebreak(),
       officer::fpar(x@population, fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
   if (sep_population != 'newline') {
     fpar_text <- officer::fpar(title_text, sep_population, x@population)
     fpar_text2 <- officer::block_list(
       officer::fpar(paste0(title_text, sep_subtitle, x@population), fp_p = officer::fp_par(text.align = "center")),
       officer::fpar()
     )
   }
  } else {
   fpar_text <- officer::fpar(title_text)
   fpar_text2 <- officer::block_list(
     officer::fpar(title_text, fp_p = officer::fp_par(text.align = "center")),
     officer::fpar()
   )
  }
 }

 content_prepared <- list(
  fpar_text = fpar_text,
  text_style = text_style,
  else_style = else_style,
  content = x@content
 )

 sect_properties <- officer::prop_section(
   page_size = officer::page_size(
     orient = "landscape",
     width = 8.3, height = 11.7
   ),
   type = "continuous",
   page_margins = officer::page_mar(),
   header_default = fpar_text2
 )

 if (x@type == 'F') {
  ggplot2::ggsave(paste0(img_path, '.jpeg'), x@content, width = x@fdim$width, height = x@fdim$height, dpi = x@fdim$dpi, units = "in", device = 'jpeg', quality = 100)

  doc <- doc |>
   officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
   officer::body_add_img(src = paste0(img_path, '.jpeg'), width = 9, height = 5, style = content_prepared$else_style)

  rtf_doc <- officer::rtf_doc(def_sec = sect_properties)
  rtf_doc <- officer::rtf_add(rtf_doc, content_prepared$content, width = 9, height = 5)
  print(rtf_doc, target = file.path(paste0(fig_path, '/', 'tlf', '/', 'rtf'), paste0("figure_", number, ".rtf")))

  unlink(paste0(img_path, '.jpeg'))
 } else if (x@type == 'T') {
  doc <- doc |>
   officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
   flextable::body_add_flextable(value = content_prepared$content)

  flextable::save_as_rtf(
    content_prepared$content,
    path = file.path(paste0(fig_path, '/', 'tlf', '/', 'rtf'), paste0("table_", number, ".rtf")), pr_section = sect_properties
  )
 } else if (x@type == 'L') {
  flex_cont <- flextable::flextable(content_prepared$content) |>
    flextable::align(align = "center", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::theme_vanilla() |>
    flextable::bg(i = seq(1, nrow(content_prepared$content), 2), bg = "#F2F2F2", part = "body")

  doc <- doc |>
   officer::body_add_fpar(content_prepared$fpar_text, style = content_prepared$text_style) |>
   flextable::body_add_flextable(value = flex_cont)

  flextable::save_as_rtf(
    flex_cont,
    path = file.path(paste0(fig_path, '/', 'tlf', '/', 'rtf'), paste0("listing_", number, ".rtf")), pr_section = sect_properties
  )
 }

 if (last == FALSE) doc <- doc |> officer::body_add_break()

 return(doc)
})

# S4 Class 'ContentPackage' ----------------------------------------------------

setClass(
 'ContentPackage',
 slots = c(
  name = 'character',
  content_list = 'list',
  start_number = 'list',
  sep_subtitle = 'character',
  sep_population = 'character',
  language = 'character'
 )
)

# 'ContentPackage' methods -----------------------------------------------------

setGeneric('add_to_package_method', function(x, content) standardGeneric('add_to_package_method'))
setMethod('add_to_package_method', 'ContentPackage', function(x, content) {

 x@content_list <- append(x@content_list, list(content))

 return(x)
})

setGeneric('export_report_method', function(x, template_name, supp = FALSE, add_toc = TRUE, spec = "") standardGeneric('export_report_method'))
setMethod('export_report_method', 'ContentPackage', function(x, template_name, supp = FALSE, add_toc = TRUE, spec = "") {
  convert_r_to_txt <- function(input_folder, output_folder) {
    r_files <- list.files(path = input_folder, pattern = "\\.R$", full.names = TRUE)
    for (r_file in r_files) {
      content <- readLines(r_file, warn = FALSE)

      file_base <- tools::file_path_sans_ext(basename(r_file))
      txt_file <- file.path(output_folder, paste0(file_base, ".R"))

      writeLines(content, txt_file)
    }
  }

 report_name <- paste0('SAR - ', x@name)

 cat('  Exporting contents:\n')

 zipfolder <- here::here('05_Results')

 output <- list(
   dir = tempfile(pattern = format(Sys.time(), "%Y_%m_%d_%H_%M_%S_"))
 )
 dir.create(output$dir)
 dir.create(paste0(output$dir, '/', 'adam'))
 dir.create(paste0(output$dir, '/', 'adam', '/', 'programs'))
 dir.create(paste0(output$dir, '/', 'documents'))
 dir.create(paste0(output$dir, '/', 'documents', '/', 'define'))
 dir.create(paste0(output$dir, '/', 'tlf'))
 dir.create(paste0(output$dir, '/', 'tlf', '/', 'rtf'))

 unlink(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")), recursive = TRUE, force = TRUE)
 dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

 types <- sapply(x@content_list, function(x) return(x@type))
 sections <- sapply(x@content_list, function(x) return(x@section))
 sections_levels <- unique(sections)

 content_numbers <- tibble::tibble(ID = seq(1, length(x@content_list)), type = factor(types, levels = c('T', 'F', 'L')), section = factor(sections, levels = sections_levels)) |>
   dplyr::group_by(type) |>
   dplyr::group_split() |>
   lapply(function(type_data) {
     if (unique(type_data$type) == 'T') start <- x@start_number$T
     else if (unique(type_data$type) == 'F') start <- x@start_number$F
     else if (unique(type_data$type) == 'L') start <- x@start_number$L

     type_data |>
       dplyr::mutate(number = seq(start, start + dplyr::n() - 1))
   }) |>
   purrr::reduce(dplyr::bind_rows) |>
   dplyr::mutate(last = ID == dplyr::n()) |>
   dplyr::arrange(ID)

 sections_start <- content_numbers |>
   dplyr::group_by(section) |>
   dplyr::summarise(section_start = min(ID))

 template_path <- paste0(here::here('00_Template'), '\\', template_name)

 mapping_list <- list()

 doc <- officer::read_docx(path = template_path)

 if (add_toc) {
   doc <- doc |>
     officer::body_add_toc() |>
     officer::body_add_break()
 }

 for (i in seq(1, length(x@content_list))) {
   cat('    \u2500 Content', i, '...')
   if (i %in% c(sections_start$section_start)) {
     if (!is.na(sections_start |> dplyr::filter(section_start == i) |> dplyr::pull(section))) doc <- officer::body_add_fpar(doc, officer::fpar(sections_start |> dplyr::filter(section_start == i) |> dplyr::pull(section)), style = 'StatsTLF T\u00edtulo 1')
     doc <- prepare_to_export_method(x@content_list[[i]], content_numbers$number[i], doc, x@sep_subtitle, x@sep_population, output$dir, last = content_numbers$last[i], language = x@language, supp = supp)
   } else {
     doc <- prepare_to_export_method(x@content_list[[i]], content_numbers$number[i], doc, x@sep_subtitle, x@sep_population, output$dir, last = content_numbers$last[i], language = x@language, supp = supp)
   }

   caux <- x@content_list[[i]]

   if (x@language == 'PT-BR') {
     table_text <- 'Tabela '
     figure_text <- 'Figura '
     listing_text <- 'Lista '
   } else if (x@language == 'EN-US') {
     table_text <- 'Table '
     figure_text <- 'Figure '
     listing_text <- 'Listing '
   }

   if (caux@type == 'T') {
     if (supp == TRUE) {
       title_text <- paste0(table_text, 'S', content_numbers$number[i])
     } else {
       title_text <- paste0(table_text, content_numbers$number[i])
     }
   } else if (caux@type == 'F') {
     if (supp == TRUE) {
       title_text <- paste0(figure_text, 'S', content_numbers$number[i])
     } else {
       title_text <- paste0(figure_text, content_numbers$number[i])
     }
   } else if (caux@type == 'L') {
     if (supp == TRUE) {
       title_text <- paste0(listing_text, 'S', content_numbers$number[i])
     } else {
       title_text <- paste0(listing_text, content_numbers$number[i])
     }
   }

   if (spec != '') {
     mapping_list[[i]] <- tibble::tibble(
       Display = title_text,
       ID = title_text,
       Documentation = 'Caller Contents File',
       Title = combine_title(caux@title, caux@subtitle, caux@population, x@sep_subtitle, x@sep_population)
     ) |>
       dplyr::bind_cols(mapping_backbone(caux@fun, spec = spec)) |>
       dplyr::mutate(`Programming Document` = paste0(caux@bk_name, '.R'))
   }

   cat('  Done!\n')
 }
 print(doc, target = paste0(output$dir, '/tlf/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.docx'))

 if (spec != '') {
   mapping_tb <- purrr::reduce(mapping_list, dplyr::bind_rows, .init = tibble::tibble(Display = character(), ID = character(), Documentation = character(), Title = character(), Variables = character(), `Programming Context` = character(), `Programming Code` = character()))
   writexl::write_xlsx(mapping_tb, paste0(output$dir, '/documents/Mapping Report.xlsx'))
 }

 files_to_copy <- list.files(output$dir, full.names = TRUE)
 file.copy(from = files_to_copy, to = paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")), overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
 Sys.chmod(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/tlf/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '.docx'), mode = "0444", use_umask = FALSE)

 cat('  Done!\n')

 cat(paste0('\n\n Report avaliable in: ', zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

 convert_r_to_txt(paste0(here::here('03_Algorithm'), '/', x@name), paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/adam/programs'))
 convert_r_to_txt(paste0(here::here('03_Algorithm'), '/', x@name, '/backbones'), paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/adam/programs'))

 return(invisible(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"))))
})

setGeneric('export_datasets_method', function(x, spec = "") standardGeneric('export_datasets_method'))
setMethod('export_datasets_method', 'ContentPackage', function(x, spec = "") {
  convert_r_to_txt <- function(input_folder, output_folder) {
    r_files <- list.files(path = input_folder, pattern = "\\.R$", full.names = TRUE)
    for (r_file in r_files) {
      content <- readLines(r_file, warn = FALSE)

      file_base <- tools::file_path_sans_ext(basename(r_file))
      txt_file <- file.path(output_folder, paste0(file_base, ".R"))

      writeLines(content, txt_file)
    }
  }

  combine_title <- function(title, subtitle, population, sep_subtitle, sep_population) {
    fix_sep <- function(sep) ifelse(sep == "newline", " ", sep)

    sep_subtitle <- fix_sep(sep_subtitle)
    sep_population <- fix_sep(sep_population)

    paste0(
      title,
      ifelse(!is.na(subtitle), paste0(sep_subtitle, subtitle), ""),
      ifelse(!is.na(population), paste0(sep_population, population), "")
    )
  }

  report_name <- paste0('Datasets - ', x@name)

  cat('  Exporting datasets:\n')

  zipfolder <- here::here('04_Datasets')

  unlink(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")), recursive = TRUE, force = TRUE)
  dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

  dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', 'datasets'))
  dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', 'datasets', '/', 'programs'))

  dir.create(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', 'documents'))

  mapping_list <- list()

  for (i in seq(1, length(x@content_list))) {
    cat('    \u2500 Dataset', i, '...')
    caux <- x@content_list[[i]]
    if (is.na(caux@export_name)) {
      cname <- paste0('adyy', i)
    } else {
      cname <- caux@export_name
    }
    saveRDS(caux@content, paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/', cname, '.RDS'))
    write.csv(caux@content, paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/', cname, '.csv'))
    haven::write_xpt(caux@content, path = paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/', cname, ".xpt"), version = 5)

    if (spec != '') {
      mapping_list[[i]] <- tibble::tibble(
        Display = cname,
        ID = cname,
        Documentation = 'Caller Contents File',
        Title = combine_title(caux@title, caux@subtitle, caux@population, x@sep_subtitle, x@sep_population)
      ) |>
      dplyr::bind_cols(mapping_backbone(caux@fun, spec = spec)) |>
      dplyr::mutate(`Programming Document` = paste0(caux@bk_name, '.R'))
    }

    cat('  Done!\n')
  }

  if (spec != '') {
    mapping_tb <- purrr::reduce(mapping_list, dplyr::bind_rows, .init = tibble::tibble(Display = character(), ID = character(), Documentation = character(), Title = character(), Variables = character(), `Programming Context` = character(), `Programming Code` = character()))
    writexl::write_xlsx(mapping_tb, paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/', 'documents/Mapping Datasets.xlsx'))
  }

  convert_r_to_txt(paste0(here::here('03_Algorithm'), '/', x@name), paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/programs'))
  convert_r_to_txt(paste0(here::here('03_Algorithm'), '/', x@name, '/backbones'), paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"), '/datasets/programs'))

  cat('  Done!\n')

  cat(paste0('\n\n Datasets avaliable in: ', zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d")))

  return(invisible(paste0(zipfolder, '/', report_name, ' - ', format(Sys.time(), "%Y-%m-%d"))))
})
